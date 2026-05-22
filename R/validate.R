#' Validate a data frame against a schema
#'
#' Checks that a data frame conforms to a schema, validating column presence,
#' types, missing values, uniqueness constraints, and nesting structure.
#'
#' @param schema A schema object created by [sch_schema()].
#' @param data A data frame to validate.
#' @param check A character vector specifying which checks to perform. The
#'   default runs all checks. Possible values:
#'   - `"names"`: check for missing required columns and unexpected extra columns.
#'   - `"types"`: check column types and missing-value (`NA`) constraints.
#'   - `"distinct"`: check uniqueness constraints for columns marked
#'     `distinct = TRUE`. Relatively expensive.
#'   - `"relationships"`: validate relationship formulas (primary-key uniqueness
#'     and crossing/nesting completeness). Relatively expensive.
#' @param call The environment or call used for error reporting, passed to
#'   [rlang::abort()]. Useful when wrapping `sch_validate()` inside another
#'   function so that the error points to the right place.
#'
#' @returns `data`, invisibly, if validation succeeds. Otherwise, an error of
#'   class `sch_validation_error` is raised with a formatted summary of all
#'   issues found.
#'
#' @examples
#' # Basic validation: valid data passes silently
#' schema <- sch_schema(
#'     id = sch_integer(distinct = TRUE),
#'     name = sch_character(missing = FALSE),
#'     age = sch_numeric(required = FALSE)
#' )
#' df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Carol"), age = c(25, NA, 30))
#' sch_validate(schema, df)
#'
#' # Invalid data throws validation errors; wrap in try() so examples can run
#' # missing required columns
#' try(sch_validate(schema, data.frame(id = 1:2)))
#'
#' # type constraints not satisfied
#' try(sch_validate(schema, data.frame(id = c(1L, 1L), name = c("Alice", NA))))
#' @export
sch_validate <- function(
    schema,
    data,
    check = c("names", "types", "distinct", "relationships"),
    call = rlang::caller_env()
) {
    assert(
        inherits(schema, "sch_schema"),
        "{.arg schema} must be an {.cls sch_schema} object."
    )
    assert(is.data.frame(data), "{.arg data} must be a data frame.")

    check = rlang::arg_match(check, multiple = TRUE)
    col_info <- classify_columns(schema$cols)
    issues <- list()

    issues <- c(issues, validate_cols(schema$cols, data, col_info, check))

    if (any(c("names", "types", "distinct") %in% check)) {
        issues <- c(issues, validate_nests(schema$cols, data, col_info, check))
    }
    if ("relationships" %in% check) {
        issues <- c(issues, validate_relationships(schema, data))
    }

    if (length(issues) > 0) {
        classes = setdiff(class(data), c("sch_df", "tbl", "tbl_df", "data.frame", "data.table"))
        print_validation_issues(issues, class = classes, call = call)
    }

    invisible(data)
}


# Validators -------------------------------------------

classify_columns <- function(cols) {
    is_other <- vapply(cols, function(x) x$type == "other", FALSE)
    is_nest <- vapply(cols, function(x) x$type == "schema_nest", FALSE)
    is_multiple <- vapply(cols, function(x) x$type == "schema_multiple", FALSE)
    nms <- names(cols)
    if (is.null(nms)) {
        nms <- rep("", length(cols))
    }
    is_named_nest <- is_nest & nzchar(nms)

    list(
        nms = nms,
        is_other = is_other,
        is_nest = is_nest,
        is_multiple = is_multiple,
        is_named_nest = is_named_nest
    )
}


# Run the per-column validators (names, types, distinct) for a set of columns
validate_cols <- function(cols, data, col_info, check, path = character(0)) {
    issues <- list()
    if ("names" %in% check) {
        issues <- c(issues, validate_names(cols, data, col_info, path))
    }
    if ("types" %in% check) {
        issues <- c(issues, validate_types_missing(cols, data, col_info, path))
    }
    if ("distinct" %in% check) {
        issues <- c(issues, validate_distinct(cols, data, col_info, path))
    }
    issues
}


validate_names <- function(cols, data, col_info, path = character(0)) {
    issues <- list()
    groups_attr <- attr(data, "sch_groups")

    if (any(col_info$is_multiple) && is.null(groups_attr)) {
        issues <- c(issues, list(make_issue("missing_sch_groups", path)))
    }

    expected_names <- character(0)
    for (i in which(!col_info$is_other)) {
        if (col_info$is_multiple[i]) {
            if (!is.null(groups_attr)) {
                res <- check_multiple_names(cols[[i]], data, groups_attr, path)
                issues <- c(issues, res$issues)
                expected_names <- c(expected_names, res$expected_nms)
            }
        } else {
            col_nm <- col_info$nms[i]
            expected_names <- c(expected_names, col_nm)
            if (
                !col_info$is_named_nest[i] &&
                    isTRUE(attr(cols[[i]], "required")) &&
                    !col_nm %in% names(data)
            ) {
                new_issue = make_issue(
                    "missing_column",
                    c(path, col_nm),
                    expected = a_an_msg(cols[[i]])
                )
                issues <- c(issues, list(new_issue))
            }
        }
    }

    if (!any(col_info$is_other)) {
        extra <- setdiff(names(data), expected_names)
        if (length(extra) > 0) {
            issues <- c(issues, list(make_issue("extra_columns", path, columns = extra)))
        }
    }

    issues
}


check_multiple_names <- function(tt, data, groups_attr, path) {
    if (!tt$name %in% names(groups_attr)) {
        return(list(
            issues = list(make_issue("missing_group", path, name = tt$name)),
            expected_nms = character(0)
        ))
    }

    col_nms <- groups_attr[[tt$name]]
    issues <- list()
    if (length(col_nms) == 0 && isTRUE(attr(tt, "required"))) {
        issues <- c(issues, list(make_issue("empty_group", path, name = tt$name)))
    }
    for (col_nm in col_nms[!col_nms %in% names(data)]) {
        new_issue <- make_issue(
            "missing_column",
            c(path, col_nm),
            expected = a_an_msg(tt$inner)
        )
        issues <- c(issues, list(new_issue))
    }

    list(issues = issues, expected_nms = col_nms)
}


validate_types_missing <- function(cols, data, col_info, path = character(0)) {
    issues <- list()
    groups_attr <- attr(data, "sch_groups")

    for (i in which(!col_info$is_other & !col_info$is_nest)) {
        if (col_info$is_multiple[i]) {
            issues <- c(issues, check_multiple_types(cols[[i]], data, groups_attr, path))
        } else {
            col_nm <- col_info$nms[i]
            if (col_nm %in% names(data)) {
                issues <- c(
                    issues,
                    check_col_type_and_na(data[[col_nm]], cols[[i]], c(path, col_nm))
                )
            }
        }
    }

    issues
}


check_multiple_types <- function(tt, data, groups_attr, path) {
    if (is.null(groups_attr) || !tt$name %in% names(groups_attr)) {
        return(list())
    }
    col_nms <- groups_attr[[tt$name]]
    if (length(col_nms) == 0) {
        return(list())
    }

    issues <- list()
    per_col_ok <- TRUE
    for (col_nm in col_nms) {
        if (!col_nm %in% names(data)) {
            per_col_ok <- FALSE
            next
        }
        col_issues <- check_col_type_and_na(data[[col_nm]], tt$inner, c(path, col_nm))
        issues <- c(issues, col_issues)
        if (length(col_issues) > 0) per_col_ok <- FALSE
    }

    if (per_col_ok && !is.null(tt$cross_check)) {
        present_nms <- intersect(col_nms, names(data))
        col_list <- stats::setNames(lapply(present_nms, function(nm) data[[nm]]), present_nms)
        if (!isTRUE(tt$cross_check(col_list, tt))) {
            new_issue <- make_issue(
                "cross_check_failed",
                path,
                name = tt$name,
                expected = tt$cross_msg(tt)
            )
            issues <- c(issues, list(new_issue))
        }
    }

    issues
}


validate_distinct <- function(cols, data, col_info, path = character(0)) {
    issues <- list()

    for (i in seq_along(cols)) {
        if (col_info$is_other[i] || col_info$is_nest[i]) {
            next
        }

        if (col_info$is_multiple[i]) {
            tt <- cols[[i]]
            if (!isTRUE(attr(tt$inner, "distinct"))) {
                next
            }
            groups_attr <- attr(data, "sch_groups")
            if (is.null(groups_attr) || !tt$name %in% names(groups_attr)) {
                next
            }
            for (col_nm in groups_attr[[tt$name]]) {
                if (!col_nm %in% names(data)) {
                    next
                }
                issues <- c(issues, check_col_distinct(data[[col_nm]], c(path, col_nm)))
            }
            next
        }

        tt <- cols[[i]]
        if (!attr(tt, "distinct")) {
            next
        }
        col_nm <- col_info$nms[i]
        issues <- c(issues, check_col_distinct(data[[col_nm]], c(path, col_nm)))
    }

    issues
}


validate_nests <- function(cols, data, col_info, check, path = character(0)) {
    issues <- list()

    for (i in which(col_info$is_named_nest)) {
        issues <- c(
            issues,
            validate_named_nest(cols[[i]], col_info$nms[i], data, check, path)
        )
    }

    issues
}


validate_named_nest <- function(nest, col_nm, data, check, path) {
    issues <- list()
    col_path <- c(path, col_nm)

    if (!col_nm %in% names(data)) {
        if ("names" %in% check && attr(nest, "required")) {
            issues <- c(
                issues,
                list(make_issue("missing_column", col_path, expected = "nested data frame"))
            )
        }
        return(issues)
    }

    x <- data[[col_nm]]

    if (!is.list(x)) {
        issues <- c(issues, list(make_issue("not_nested_df", col_path)))
        return(issues)
    }

    # Validate each nested data frame element
    inner_col_info <- classify_columns(nest$cols)
    for (idx in seq_along(x)) {
        elem <- x[[idx]]
        if (!is.data.frame(elem)) {
            issues <- c(
                issues,
                list(make_issue("not_data_frame_element", col_path, index = idx))
            )
            next
        }

        inner_issues <- c(
            validate_cols(nest$cols, elem, inner_col_info, check, col_path),
            validate_nests(nest$cols, elem, inner_col_info, check, col_path)
        )

        for (j in seq_along(inner_issues)) {
            inner_issues[[j]]$element <- idx
        }
        issues <- c(issues, inner_issues)
    }

    issues
}


# Relationship validation -------------------------------------------------

validate_relationships <- function(schema, data) {
    tree <- schema$relationships
    if (is.null(tree)) {
        return(list())
    }

    # All formula columns
    all_cols <- relationship_columns(tree)

    # Skip if any formula column missing from data (names check catches it)
    if (!all(all_cols %in% names(data))) {
        return(list())
    }

    # Skip empty data frames
    if (nrow(data) == 0L) {
        return(list())
    }

    issues <- list()

    # Uniqueness: all formula columns form a unique key
    key_data <- data[, all_cols, drop = FALSE]
    grp <- vctrs::vec_group_loc(key_data)
    dup_mask <- lengths(grp$loc) > 1L
    if (any(dup_mask)) {
        n_dup <- nrow(data) - nrow(vctrs::vec_unique(key_data))
        first_key <- vctrs::vec_slice(grp$key, which(dup_mask)[1L])
        issues <- c(
            issues,
            list(make_issue(
                "duplicate_key",
                character(0),
                columns = all_cols,
                n_duplicates = n_dup,
                first_key = first_key
            ))
        )
    }

    issues <- c(issues, validate_rel_node(tree, data, group_cols = character(0)))

    issues
}


# Recursively validate a relationship tree node
validate_rel_node <- function(node, data, group_cols) {
    switch(
        node$type,
        var = list(),
        compound = list(),
        cross = validate_rel_cross(node, data, group_cols),
        nest = validate_rel_nest(node, data, group_cols),
        rlang::abort(paste0("Unknown relationship node type: ", node$type))
    )
}


# Check crossing completeness: within each group defined by group_cols,
# the unique combos of all children = product of unique combos per child.
# When group_cols is non-empty, all failing groups are summarized into a
# single issue showing the count and the first offending group key.
validate_rel_cross <- function(node, data, group_cols) {
    issues <- list()
    children <- node$children

    # Get column names per child
    child_cols <- lapply(children, relationship_columns)
    label <- paste(
        vapply(child_cols, function(cc) paste(cc, collapse = " + "), ""),
        collapse = " \u00d7 "
    )

    if (length(group_cols) == 0L) {
        issues <- c(issues, check_cross_completeness(data, child_cols, label, NULL, 1L, 1L))
    } else {
        grp_data <- data[, group_cols, drop = FALSE]
        locs <- vctrs::vec_group_loc(grp_data)
        n_groups <- length(locs$loc)
        n_fail <- 0L
        first_key <- NULL
        first_actual <- NULL
        first_expected <- NULL

        for (i in seq_len(n_groups)) {
            sub <- vctrs::vec_slice(data, locs$loc[[i]])
            result <- cross_completeness_counts(sub, child_cols)
            if (result$actual < result$expected) {
                n_fail <- n_fail + 1L
                if (is.null(first_key)) {
                    first_key <- vctrs::vec_slice(grp_data, locs$loc[[i]][1L])
                    first_actual <- result$actual
                    first_expected <- result$expected
                }
            }
        }

        if (n_fail > 0L) {
            issues <- c(
                issues,
                list(make_issue(
                    "incomplete_crossing",
                    character(0),
                    label = label,
                    actual = first_actual,
                    expected = first_expected,
                    group_cols = group_cols,
                    first_key = first_key,
                    n_fail = n_fail,
                    n_groups = n_groups
                ))
            )
        }
    }

    # Recurse into non-leaf children only; cross completeness handles var/compound
    for (k in seq_along(children)) {
        child <- children[[k]]
        if (child$type %in% c("var", "compound")) {
            next
        }
        sibling_cols <- unique(unlist(child_cols[-k]))
        new_group <- c(group_cols, sibling_cols)
        issues <- c(issues, validate_rel_node(child, data, new_group))
    }

    issues
}


# Returns list(actual, expected) counts for crossing completeness
cross_completeness_counts <- function(data, child_cols) {
    all_cols <- unique(unlist(child_cols))
    actual <- nrow(vctrs::vec_unique(data[, all_cols, drop = FALSE]))
    expected <- prod(vapply(
        child_cols,
        function(cc) nrow(vctrs::vec_unique(data[, cc, drop = FALSE])),
        0L
    ))
    list(actual = actual, expected = expected)
}


# Check that unique(all child cols) == product of unique(each child's cols)
check_cross_completeness <- function(data, child_cols, label, first_key, n_fail, n_groups) {
    if (length(child_cols) < 2) {
        return(list())
    }

    result <- cross_completeness_counts(data, child_cols)

    if (result$actual < result$expected) {
        return(list(make_issue(
            "incomplete_crossing",
            character(0),
            label = label,
            actual = result$actual,
            expected = result$expected,
            group_cols = character(0),
            first_key = first_key,
            n_fail = n_fail,
            n_groups = n_groups
        )))
    }

    list()
}


# Check nesting: inner is scoped within outer groups
validate_rel_nest <- function(node, data, group_cols) {
    issues <- list()
    outer_cols <- relationship_columns(node$outer)

    # For three-level (or deeper) formulas where outer is itself a nest and
    # inner is a cross, sibling groups at the intermediate level must carry
    # consistent inner structure. The scope for this consistency check
    # depends on the structure of the intermediate cross:
    #
    #   Pure cross (no nested children): groups are fully symmetric, so scope
    #   to the grandparent (outer$outer) level. All siblings within a
    #   grandparent group must share the same inner grid.
    #
    #   Mixed cross (some nested children): the non-nested children define a
    #   "symmetric" dimension and the nested children define a heterogeneous
    #   one. Extend the grandparent scope by the non-nested children's columns
    #   so that only groups sharing both the grandparent identity AND the
    #   symmetric dimension are compared. E.g. for
    #   `(race) / (geo * (party / candidate)) / (time * method)`, scope is
    #   per (race, geo): within a geo, all (party, candidate) groups must have
    #   the same time × method grid, but different geos may have different grids.
    #
    # All failures across every scope group are aggregated into a single issue
    # so the user sees one summary rather than one error per scope group.
    if (node$outer$type == "nest" && node$inner$type == "cross") {
        first_outer_cols <- relationship_columns(node$outer$outer)
        scope_extension <- character(0L)

        if (node$outer$inner$type == "cross") {
            outer_inner_children <- node$outer$inner$children
            is_nest_ch <- vapply(outer_inner_children, function(ch) ch$type == "nest", logical(1L))
            if (any(is_nest_ch)) {
                scope_extension <- unlist(lapply(
                    outer_inner_children[!is_nest_ch],
                    relationship_columns
                ))
            }
        }

        scope_cols <- unique(c(group_cols, first_outer_cols, scope_extension))

        # Column info for the inner cross node (for reference computation & label)
        child_cols <- lapply(node$inner$children, relationship_columns)
        all_child_cols <- unique(unlist(child_cols))
        label <- paste(
            vapply(child_cols, function(cc) paste(cc, collapse = " + "), ""),
            collapse = " \u00d7 "
        )

        # Aggregate failures across all scope groups into a single issue
        n_fail <- 0L
        n_groups <- 0L
        first_key <- NULL
        first_actual <- NULL
        first_expected <- NULL

        run_scope <- function(sub_scope) {
            ref_expected <- cross_completeness_counts(sub_scope, child_cols)$expected
            sub_grp <- sub_scope[, outer_cols, drop = FALSE]
            sub_locs <- vctrs::vec_group_loc(sub_grp)
            n_groups <<- n_groups + length(sub_locs$loc)
            for (i in seq_len(length(sub_locs$loc))) {
                sub <- vctrs::vec_slice(sub_scope, sub_locs$loc[[i]])
                actual <- nrow(vctrs::vec_unique(sub[, all_child_cols, drop = FALSE]))
                if (actual < ref_expected) {
                    n_fail <<- n_fail + 1L
                    if (is.null(first_key)) {
                        first_key <<- vctrs::vec_slice(sub_grp, sub_locs$loc[[i]][1L])
                        first_actual <<- actual
                        first_expected <<- ref_expected
                    }
                }
            }
        }

        if (length(scope_cols) == 0L) {
            run_scope(data)
        } else {
            scope_data <- data[, scope_cols, drop = FALSE]
            scope_locs <- vctrs::vec_group_loc(scope_data)
            for (si in seq_len(length(scope_locs$loc))) {
                run_scope(vctrs::vec_slice(data, scope_locs$loc[[si]]))
            }
        }

        if (n_fail > 0L) {
            issues <- c(
                issues,
                list(make_issue(
                    "incomplete_crossing",
                    character(0),
                    label = label,
                    actual = first_actual,
                    expected = first_expected,
                    group_cols = outer_cols,
                    first_key = first_key,
                    n_fail = n_fail,
                    n_groups = n_groups
                ))
            )
        }

        # Recurse into non-leaf children of the inner cross
        for (k in seq_along(node$inner$children)) {
            child <- node$inner$children[[k]]
            if (child$type %in% c("var", "compound")) {
                next
            }
            sibling_cols <- unique(unlist(child_cols[-k]))
            issues <- c(issues, validate_rel_node(child, data, c(group_cols, sibling_cols)))
        }
    } else {
        issues <- c(issues, validate_rel_node(node$inner, data, outer_cols))
    }

    # Only recurse into outer if it has sub-structure (cross or nest);
    # var/compound outers are pure grouping context, not independently validated.
    if (node$outer$type %in% c("cross", "nest")) {
        issues <- c(issues, validate_rel_node(node$outer, data, group_cols))
    }

    issues
}


# Printing ---------------------------------------------------------------

print_validation_issues <- function(issues, class = NULL, call = NULL) {
    n <- length(issues)

    bullets <- vapply(issues, print_validation_issue, character(1))
    names(bullets) <- rep("x", n)
    if (is.null(class) || length(class) == 0) {
        class <- "Data"
    } else {
        class = paste0("{.cls ", paste(class, collapse = "}/{.cls "), "}")
    }

    cli::cli_abort(
        c(paste(class, "validation failed with {n} issue{?s}:"), bullets),
        class = "sch_validation_error",
        issues = issues,
        call = call
    )
}

print_validation_issue <- function(issue) {
    p <- paste(issue$path, collapse = "$")
    elem_note <- if (!is.null(issue$element)) {
        paste0(" (element ", issue$element, ")")
    } else {
        ""
    }

    switch(
        issue$type,
        missing_column = cli::format_inline(
            "Required column {.field {p}}{elem_note} is missing. Expected {issue$expected}."
        ),
        wrong_type = cli::format_inline(
            "Column {.field {p}}{elem_note} has wrong type. Expected {issue$expected}."
        ),
        has_na = cli::format_inline(
            "Column {.field {p}}{elem_note} must not contain missing values."
        ),
        not_distinct = cli::format_inline(
            "Column {.field {p}}{elem_note} must not contain duplicate values."
        ),
        extra_columns = {
            nms <- cli::cli_vec(issue$columns, list("vec-trunc" = 5))
            cli::format_inline("Unexpected column{?s}: {.field {nms}}.")
        },
        not_nested_df = cli::format_inline(
            "Column {.field {p}} should be a list of data frames."
        ),
        not_data_frame_element = cli::format_inline(
            "Column {.field {p}} element {issue$index} is not a data frame."
        ),
        missing_sch_groups = cli::format_inline(
            "Data frame is missing the {.field sch_groups} attribute (required by {.fn sch_multiple})."
        ),
        missing_group = cli::format_inline(
            "Malformed attributes: group {.field {issue$name}} not found in {.field sch_groups}."
        ),
        empty_group = cli::format_inline(
            "Malformed attributes: group {.field {issue$name}} in {.field sch_groups} is emptpy but {.code required = TRUE})."
        ),
        cross_check_failed = cli::format_inline(
            "Column group {.field {issue$name}} failed cross-column check. Expected {issue$expected}."
        ),
        duplicate_key = {
            cols_str <- paste(issue$columns, collapse = ", ")
            key_str <- format_key(issue$first_key)
            cli::format_inline(
                "Columns ({.field {cols_str}}) should be unique per row. Found {issue$n_duplicates} duplicate combination{?s}. First duplicate: ({key_str})."
            )
        },
        incomplete_crossing = {
            if (length(issue$group_cols) == 0L) {
                cli::format_inline(
                    "Incomplete crossing of {issue$label}: found {issue$actual} of {issue$expected} expected combinations."
                )
            } else {
                grp_label <- paste(issue$group_cols, collapse = " + ")
                key_str <- format_key(issue$first_key)
                if (issue$n_fail == 1L) {
                    cli::format_inline(
                        "Incomplete crossing of {issue$label} in ({.field {grp_label}}) group ({key_str}): found {issue$actual} of {issue$expected} expected combinations."
                    )
                } else {
                    cli::format_inline(
                        "Incomplete crossing of {issue$label} in {issue$n_fail} of {issue$n_groups} ({.field {grp_label}}) groups. First at ({key_str}): found {issue$actual} of {issue$expected} expected combinations."
                    )
                }
            }
        },
        paste0("Unknown issue: ", issue$type)
    )
}


# Helpers ----------

make_issue <- function(type, path, ...) {
    c(list(type = type, path = path), list(...))
}

# Format a single-row data frame key as "col1=val1, col2=val2, ..."
format_key <- function(key) {
    if (is.null(key) || ncol(key) == 0L) {
        return("")
    }
    parts <- mapply(
        function(nm, val) paste0(nm, "=", as.character(val[[1L]])),
        names(key),
        as.list(key),
        SIMPLIFY = TRUE
    )
    paste(parts, collapse = ", ")
}

# Build "a <type description>" / "an <type description>" for a type object
a_an_msg <- function(tt) {
    msg <- type_fns[[tt$type]]$msg(tt)
    paste0(if (grepl("^[aeiou]", msg)) "an " else "a ", msg)
}

# TRUE if a vector or list-column contains any NA / NULL elements
has_missing <- function(x) {
    if (is.list(x)) anyNA(x) || any(vapply(x, is.null, logical(1))) else anyNA(x)
}

# Type check + (gated) NA check for a single column vector
check_col_type_and_na <- function(x, tt, col_path) {
    issues <- list()
    type_ok <- isTRUE(type_fns[[tt$type]]$check(x, tt))
    if (!type_ok) {
        issues <- c(issues, list(make_issue("wrong_type", col_path, expected = a_an_msg(tt))))
    }
    if (type_ok && !attr(tt, "missing") && has_missing(x)) {
        issues <- c(issues, list(make_issue("has_na", col_path)))
    }
    issues
}

# Distinctness check for a single column vector
check_col_distinct <- function(x, col_path) {
    x_obs <- x[!is.na(x)]
    if (vctrs::vec_unique_count(x_obs) != vctrs::vec_size(x_obs)) {
        list(make_issue("not_distinct", col_path))
    } else {
        list()
    }
}
