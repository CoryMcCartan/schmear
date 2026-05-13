#' Define a structured data type
#'
#' Defines the structure of a single 'observation' for a structured data frame.
#' Each column has type restrictions and may be required or optional.
#' Schemas support nesting relationships.
#'
#' @param ... Column specifications, in the form of `col_name = col_type` pairs,
#'   where `col_type` is a call to a column type constructor listed here, such
#'   as `sch_numeric()`. Every type must be a kind of vector, i.e.,
#'   [vctrs::obj_is_vector()] must return `TRUE`.
#'
#'   All columns must be named, except for `sch_others()`, as described below,
#'   and `sch_multiple()`, which describes a group of columns sharing the same
#'   type. A named `sch_nest()` describes columns stored as a nested data frame.
#'
#'   The special function `sch_others()` indicates the preferred location of
#'   other columns not explicitly mentioned in the schema. If no `sch_others()`
#'   appears, then other columns are not allowed.
#'   Trailing commas are permitted.
#' @param .relationships An optional one-sided formula describing the structural
#'   relationships between values in different columns. Formulas can only involve
#'   named arguments to `...`. Use `*` to signify crossed levels, which will
#'   verify all combinations exist, `/` to signify nested levels, and `+` to create
#'   compound keys (bundling columns into a single identifier).
#'   See the examples below.
#' @param desc,.desc A description of the column for consumers of the schema.
#'   The type contraints will be described separately and do not need to be
#'   included in the description.  For example for "age", the description might
#'   be "Age of the patient in years", not "Non-negative integer representing
#'   the age of the patient in years". For the overall `sch_schema`, the `desc`
#'   will be printed as part of the header for data frames implementing the
#'   schema, by default.
#' @param missing If `TRUE`, the column may be contain missing values. Otherwise,
#'   any missing values result in an error.
#' @param required If `TRUE`, the column must be present. If `FALSE`, the column
#'   is optional.
#' @param distinct If `TRUE`, the column must contain no duplicate values (after
#'   accounting for nesting structure).
#'
#' @returns An object of class `sch_schema`,
#' @examples
#' sch_schema(
#'     .desc = "MCMC draws",
#'     .relationships = ~ chain * draw * parameter,
#'     chain = sch_integer("Chain number"),
#'     draw = sch_integer("Draw number", bounds = c(1, Inf), closed = c(TRUE, FALSE)),
#'     parameter = sch_factor("Parameter name", levels = c("mu", "sigma", "log_lik")),
#'     value = sch_numeric("Parameter value")
#' )
#'
#' sch_schema(
#'     .desc = "Student data",
#'     .relationships = ~ (grade + teacher) / table_group,
#'     birthday = sch_date("Date of birth", required = FALSE),
#'     height = sch_numeric(
#'         "Height in inches",
#'         bounds = c(0, 108),
#'         closed = c(FALSE, TRUE)
#'     ),
#'     grade = sch_factor(strict = FALSE, levels = c("Kindergarten", "1st", "2nd")),
#'     teacher = sch_nest(
#'         first = sch_character("First name"),
#'         last = sch_character("Last name")
#'     ),
#'     table_group = sch_integer(bounds=c(1, 6)),
#'     enrolled = sch_logical(missing = FALSE),
#'     sch_others()
#' )
#'
#' sch_schema(
#'     .desc = "Causal inference data",
#'     treatment = sch_factor(levels = c("control", "treatment"), missing = FALSE),
#'     outcome = sch_numeric(missing = FALSE),
#'     sch_multiple("covariates", type = sch_any(missing = FALSE), required = FALSE),
#'     sch_others()
#' )
#'
#' sch_custom(
#'    name = "even",
#'    check = function(x, type) is.integer(x) && all(x %% 2 == 0),
#'    msg = function(type) "vector of even integers",
#'    coerce = function(x, type) (as.integer(x) %/% 2) * 2
#' )
#' @export
sch_schema <- function(..., .desc = NULL, .relationships = NULL) {
    cols = rlang::dots_list(..., .homonyms = "error", .check_assign = TRUE)

    assert(length(cols) > 0, "{.arg ...} must not be empty")
    if (!all(vapply(cols, inherits, FALSE, what = "sch_type"))) {
        rlang::abort("All columns must be specified using a column type constructor.")
    }

    is_other = vapply(cols, function(x) x$type == "other", FALSE)
    is_nest = vapply(cols, function(x) x$type == "schema_nest", FALSE)
    is_multiple = vapply(cols, function(x) x$type == "schema_multiple", FALSE)
    is_unnamed_ok = is_other | is_multiple
    if (sum(is_other) > 1) {
        rlang::abort("Only one {.fn sch_others()} is allowed in a schema.")
    }
    if (any(is_other) && rlang::is_named(cols[is_other])) {
        rlang::abort("{.fn sch_others()} must not be named.")
    }
    if (any(is_multiple) && rlang::is_named(cols[is_multiple])) {
        rlang::abort("{.fn sch_multiple()} must not be named.")
    }
    # Unnamed sch_nest() is no longer supported
    if (any(is_nest & !rlang::have_name(cols))) {
        rlang::abort(
            "Unnamed {.fn sch_nest()} is no longer supported. Use named nests or {.arg .relationships}."
        )
    }

    named_cols = cols[!is_unnamed_ok]
    if (length(named_cols) > 0 && !rlang::is_named(named_cols)) {
        rlang::abort("All columns must be named.")
    }
    nms = names(named_cols)
    if (anyDuplicated(nms[nzchar(nms)]) > 0) {
        rlang::abort("Column names must be unique.")
    }

    # Parse and validate .relationships
    rel_tree = NULL
    if (!is.null(.relationships)) {
        rel_tree = parse_relationship(.relationships)

        # Collect all column names referenced in the formula
        rel_col_names = relationship_columns(rel_tree)

        # All named columns (excluding unnamed sch_others() and sch_multiple())
        regular_names = names(cols)[!is_other & !is_multiple]

        bad_names = setdiff(rel_col_names, regular_names)
        if (length(bad_names) > 0) {
            rlang::abort(c(
                ".relationships references columns not in the schema:",
                paste0("  ", bad_names, collapse = "\n")
            ))
        }

        # Disallow distinct=TRUE columns in formula
        for (cn in rel_col_names) {
            col_type = cols[[cn]]
            if (!is.null(col_type) && isTRUE(attr(col_type, "distinct"))) {
                rlang::abort(c(
                    "Column {.field {cn}} has {.code distinct=TRUE} but appears in {.arg .relationships}.",
                    "i" = "Columns in a relationship formula cannot have {.code distinct=TRUE}."
                ))
            }
        }
    }

    structure(
        list(cols = cols, relationships = rel_tree),
        desc = check_desc(.desc),
        class = "sch_schema"
    )
}


#' @describeIn sch_schema A placeholder for other non-required columns in a schema.
#' @export
sch_others <- function() {
    structure(
        list(type = "other"),
        missing = NA,
        required = FALSE,
        distinct = FALSE,
        class = "sch_type"
    )
}

#' @describeIn sch_schema A column of any type. No type checking is performed.
#' @export
sch_any <- function(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE) {
    structure(
        list(type = "any"),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A group of multiple columns sharing the same type. The
#'   group is identified by `name`, which must appear as an entry in the
#'   `sch_groups` attribute of the data frame being validated. That entry is a
#'   character vector of column names that belong to this group.
#'
#'   Optionally accepts cross-column `check`, `msg`, and `coerce` functions that
#'   are applied to the entire group after per-column type checks pass. These
#'   must all be provided together or not at all.
#'
#'   `sch_multiple()` must be unnamed in an `sch_schema()` call. Per-column
#'   constraints such as `missing` and `distinct` are set on the inner `type`
#'   argument.
#'
#' @param name A single string identifying the group. Must match a key in the
#'   `sch_groups` attribute of the data frame.
#' @param type A column type constructor (e.g. [sch_numeric()]) specifying the
#'   expected type of every column in the group.
#' @param required If `TRUE` (default), the group entry in `sch_groups` must
#'   contain at least one column name. If `FALSE`, an empty character vector
#'   for that entry is also accepted.
#' @export
sch_multiple <- function(
    name,
    type,
    desc = NULL,
    required = TRUE,
    check = NULL,
    msg = NULL,
    coerce = NULL
) {
    if (!is.character(name) || length(name) != 1) {
        rlang::abort("{.arg name} must be a single string.")
    }

    invalid_types = c("other", "schema_nest", "schema_multiple")
    if (!inherits(type, "sch_type") || type$type %in% invalid_types) {
        rlang::abort(
            "{.arg type} must be an {.cls sch_type} column type constructor ",
            "(not {.fn sch_others()}, {.fn sch_nest()}, or {.fn sch_multiple()})."
        )
    }

    fns_given = c(!is.null(check), !is.null(msg), !is.null(coerce))
    if (any(fns_given) && !all(fns_given)) {
        rlang::abort(
            "{.arg check}, {.arg msg}, and {.arg coerce} must all be provided together or not at all."
        )
    }

    if (!is.null(check)) {
        if (!is.function(check) || length(formals(check)) != 2) {
            rlang::abort("{.arg check} must be a function with two arguments: `x` and `type`.")
        }
        if (!is.function(msg) || length(formals(msg)) != 1) {
            rlang::abort("{.arg msg} must be a function with one argument: `type`.")
        }
        if (!is.function(coerce) || length(formals(coerce)) != 2) {
            rlang::abort("{.arg coerce} must be a function with two arguments: `x` and `type`.")
        }
    }

    structure(
        list(
            type = "schema_multiple",
            name = name,
            inner = type,
            cross_check = check,
            cross_msg = msg,
            cross_coerce = coerce
        ),
        desc = check_desc(desc),
        required = isTRUE(required),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A set of columns stored as a nested list-column of
#'   data frames. Must be given a name in the outer `sch_schema()`.
#' @export
sch_nest <- function(..., .desc = NULL) {
    cols = rlang::dots_list(..., .homonyms = "error", .check_assign = TRUE)

    if (!all(vapply(cols, inherits, FALSE, what = "sch_type"))) {
        rlang::abort("All columns must be specified using a column type constructor.")
    }
    has_other = any(vapply(cols, function(x) x$type == "other", FALSE))
    if (has_other) {
        rlang::abort("{.fn sch_others()} is not allowed inside {.fn sch_nest()}.")
    }

    if (length(cols) > 0 && !rlang::is_named(cols)) {
        rlang::abort("All columns inside {.fn sch_nest()} must be named.")
    }
    nms = names(cols)
    if (anyDuplicated(nms[nzchar(nms)]) > 0) {
        rlang::abort("Column names must be unique.")
    }

    structure(
        list(type = "schema_nest", cols = cols),
        desc = check_desc(.desc),
        missing = FALSE,
        required = TRUE,
        class = c("sch_schema", "sch_type")
    )
}


# Formula parsing ---------------------------------------------------------

# Parse a relationship formula into a tree
parse_relationship <- function(formula) {
    if (!inherits(formula, "formula")) {
        rlang::abort("{.arg formula} must be a formula.")
    }
    if (length(formula) == 3L) {
        rlang::abort("{.arg formula} must be one-sided (e.g., {.code ~ a * b}).")
    }
    parse_rel_expr(formula[[2L]])
}

# Recursively parse an R expression from the formula RHS
parse_rel_expr <- function(expr) {
    if (is.symbol(expr)) {
        return(list(type = "var", name = as.character(expr)))
    }
    if (is.call(expr)) {
        op = as.character(expr[[1L]])
        if (op == "(") {
            return(parse_rel_expr(expr[[2L]]))
        }
        if (op %in% c("*", ":")) {
            left = parse_rel_expr(expr[[2L]])
            right = parse_rel_expr(expr[[3L]])
            # Flatten nested crosses into one node
            children = c(
                if (left$type == "cross") left$children else list(left),
                if (right$type == "cross") right$children else list(right)
            )
            return(list(type = "cross", children = children))
        }
        if (op == "/") {
            outer = parse_rel_expr(expr[[2L]])
            inner = parse_rel_expr(expr[[3L]])
            return(list(type = "nest", outer = outer, inner = inner))
        }
        if (op == "+") {
            left = parse_rel_expr(expr[[2L]])
            right = parse_rel_expr(expr[[3L]])
            # Flatten nested compounds into one node
            children = c(
                if (left$type == "compound") left$children else list(left),
                if (right$type == "compound") right$children else list(right)
            )
            return(list(type = "compound", children = children))
        }
    }
    rlang::abort(paste0("Unsupported operator in relationship formula: ", deparse(expr)))
}

# Extract all column names from a relationship tree
relationship_columns <- function(tree) {
    switch(
        tree$type,
        var = tree$name,
        cross = unique(unlist(lapply(tree$children, relationship_columns))),
        compound = unique(unlist(lapply(tree$children, relationship_columns))),
        nest = unique(c(relationship_columns(tree$outer), relationship_columns(tree$inner))),
        rlang::abort(paste0("Unknown relationship node type: ", tree$type))
    )
}


#' @describeIn sch_schema A numeric vector that is optionally constrained to be
#'   within a certain range.
#'
#' @param bounds Length-two vector `c(min, max)` specifying the allowed range of values.
#' @param closed Length-two logical vector specifying whether the bounds are
#'   closed (inclusive) or open (exclusive).
#' @export
sch_numeric <- function(
    desc = NULL,
    bounds = c(-Inf, Inf),
    closed = c(TRUE, TRUE),
    missing = TRUE,
    required = TRUE,
    distinct = FALSE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "numeric", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}


#' @describeIn sch_schema An integer vector that is optionally constrained to be
#'   within a certain range.
#' @export
sch_integer <- function(
    desc = NULL,
    bounds = c(-Inf, Inf),
    closed = c(TRUE, TRUE),
    missing = TRUE,
    required = TRUE,
    distinct = FALSE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "integer", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A logical vector.
#' @export
sch_logical <- function(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE) {
    structure(
        list(type = "logical"),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A character vector.
#' @export
sch_character <- function(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE) {
    structure(
        list(type = "character"),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A factor with specified levels.
#' @param levels A character vector of factor levels, or NULL not enforce specific levels.
#' @param strict If `TRUE`, only factors with the specified levels are accepted.
#'   If `FALSE`, character vectors with the specified levels are also accepted.
#' @export
sch_factor <- function(
    desc = NULL,
    levels = NULL,
    strict = TRUE,
    missing = TRUE,
    required = TRUE,
    distinct = FALSE
) {
    if (!(is.character(levels) || is.null(levels))) {
        rlang::abort("`levels` must be a character vector.")
    }
    structure(
        list(type = "factor", levels = levels, strict = isTRUE(strict)),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A Date vector that is optionally constrained to be
#'   within a certain range.
#' @export
sch_date <- function(
    desc = NULL,
    bounds = c(as.Date(-Inf), as.Date(Inf)),
    closed = c(FALSE, FALSE),
    missing = TRUE,
    required = TRUE,
    distinct = FALSE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "date", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}
#' @describeIn sch_schema A POSIXct vector that is optionally constrained to be
#'   within a certain range.
#' @export
sch_datetime <- function(
    desc = NULL,
    bounds = c(as.POSIXct(-Inf), as.POSIXct(Inf)),
    closed = c(FALSE, FALSE),
    missing = TRUE,
    required = TRUE,
    distinct = FALSE
) {
    check_bounds_closed(bounds, closed)

    structure(
        list(type = "datetime", bounds = bounds, closed = closed),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A list-column whose elements satisfy `inherits(_, class)`.
#' @param class A character vector of class names.
#'
#' @export
sch_inherits <- function(desc = NULL, class, missing = TRUE, required = TRUE, distinct = FALSE) {
    structure(
        list(type = "inherits", class = as.character(class)),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A vector satisfying `inherits(_, class)`.
#' @export
sch_list_of <- function(desc = NULL, class, missing = TRUE, required = TRUE, distinct = FALSE) {
    structure(
        list(type = "list_of", class = as.character(class)),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}

#' @describeIn sch_schema A custom type defined by user-provided check, type message,
#'   and coercion functions. Additional named values to be stored along with the
#'   type specification may be passed via `...` and will be available to the
#'   check, message, and coercion function as elements of the `type` argument.
#' @param name A name for the custom type.
#' @param check A two-argument function that checks whether an object satisfies
#'   the type. The first argument is the object to check, and the second is the
#'   full type specification.
#' @param msg A one-argument function that generates a descriptive message
#'   about the type when passed the type object itself. Should not end with a period.
#' @param coerce A two-argument function that attempts to coerce an object to the
#'   type. The first argument is the object to coerce, and the second is the full
#'   type specification.
#'
#' @export
sch_custom <- function(
    name,
    desc = NULL,
    check,
    msg,
    coerce,
    ...,
    missing = TRUE,
    required = TRUE,
    distinct = FALSE
) {
    if (!is.character(name) || length(name) != 1) {
        rlang::abort("{.arg name} must be a single string.")
    }
    reserved_nms = c("other", names(type_fns))
    if (name %in% reserved_nms) {
        rlang::abort(
            "{.arg name} must not be one of the reserved type names: {.field {reserved_nms}}."
        )
    }
    err_fn = function(arg) {
        rlang::abort("{.arg {arg}} must be a function with two arguments: `x` and `type`.")
    }
    if (!is.function(check) || length(formals(check)) != 2) {
        err_fn("check")
    }
    if (!is.function(coerce) || length(formals(coerce)) != 2) {
        err_fn("coerce")
    }
    if (!is.function(msg) || length(formals(msg)) != 1) {
        rlang::abort("{.arg {arg}} must be a function with one argument `type`.")
    }

    extras = rlang::list2(...)
    if (length(extras) > 0 && !rlang::is_named(extras)) {
        rlang::abort("All additional arguments must be named.")
    }

    structure(
        rlang::list2(
            type = "custom",
            name = name,
            check = check,
            msg = msg,
            coerce = coerce,
            !!!extras
        ),
        desc = check_desc(desc),
        missing = isTRUE(missing),
        required = isTRUE(required),
        distinct = isTRUE(distinct),
        class = "sch_type"
    )
}


# these are covered but covr doesn't recognize them
# nocov start
check_num <- function(x, type) {
    switch(
        type$type,
        numeric = is.numeric(x),
        integer = is.integer(x),
        date = inherits(x, "Date"),
        datetime = inherits(x, "POSIXct")
    ) &&
        (if (type$closed[1]) {
            all(x >= type$bounds[1], na.rm = TRUE)
        } else {
            all(x > type$bounds[1], na.rm = TRUE)
        }) &&
        (if (type$closed[2]) {
            all(x <= type$bounds[2], na.rm = TRUE)
        } else {
            all(x < type$bounds[2], na.rm = TRUE)
        })
}
msg_num <- function(type) {
    out = paste0(type$type, " vector")
    if (!all(is.infinite(type$bounds))) {
        out = paste0(
            out,
            " with values in ",
            if (type$closed[1]) "[" else "(",
            type$bounds[1],
            ", ",
            type$bounds[2],
            if (type$closed[2]) "]" else ")"
        )
    }
    out
}
# nocov end

type_fns = list(
    numeric = list(check = check_num, msg = msg_num, coerce = function(x, type) as.numeric(x)),
    integer = list(check = check_num, msg = msg_num, coerce = function(x, type) as.integer(x)),
    date = list(check = check_num, msg = msg_num, coerce = function(x, type) as.Date(x)),
    datetime = list(check = check_num, msg = msg_num, coerce = function(x, type) as.POSIXct(x)),
    logical = list(
        check = function(x, type) is.logical(x),
        msg = function(type) "logical vector",
        coerce = function(x, type) as.logical(x)
    ),

    factor = list(
        check = function(x, type) {
            if (type$strict) {
                is.factor(x) &&
                    (is.null(type$levels) || identical(levels(x), type$levels))
            } else {
                (is.factor(x) || is.character(x)) &&
                    (is.null(type$levels) || all(x[!is.na(x)] %in% type$levels))
            }
        },
        msg = function(type) {
            out = if (type$strict) "factor" else "factor or character"
            if (!is.null(type$levels)) {
                levs = cli::cli_vec(
                    type$levels,
                    list(
                        "vec-trunc" = 10,
                        "vec-last" = ", or "
                    )
                )
                out = cli::format_inline("{out}; one of {.strong {levs}}")
            }
            out
        },
        coerce = function(x, type) {
            if (!is.null(type$levels)) {
                factor(x, levels = type$levels)
            } else {
                as.factor(x)
            }
        }
    ),

    character = list(
        check = function(x, type) is.character(x),
        msg = function(type) "character vector",
        coerce = function(x, type) as.character(x)
    ),

    any = list(
        check = function(x, type) TRUE,
        msg = function(type) "vector of any type",
        coerce = function(x, type) x
    ),

    inherits = list(
        check = function(x, type) {
            inherits(x, type$class)
        },
        msg = function(type) {
            cli::format_inline("vector inheriting from {.cls {type$class}}")
        },
        coerce = function(x, type) {
            methods::as(x, type$class)
        }
    ),
    list_of = list(
        check = function(x, type) {
            is.list(x) &&
                all(vapply(x, function(e) is.null(e) || inherits(e, type$class), logical(1)))
        },
        msg = function(type) {
            cli::format_inline("list-column with elements of type {.cls {type$class}}")
        },
        coerce = function(x, type) {
            lapply(x, function(y) {
                methods::as(y, type$class)
            })
        }
    ),

    custom = list(
        check = function(x, type) {
            type$check(x, type)
        },
        msg = function(type) {
            type$msg(type)
        },
        coerce = function(x, type) {
            type$coerce(x, type)
        }
    )
)


check_bounds_closed = function(bounds, closed) {
    assert(length(bounds) == 2, "{.arg bounds} must be a length-two vector.")
    assert(
        length(closed) == 2 && is.logical(closed),
        "{.arg closed} must be a length-two logical vector."
    )
}
check_desc = function(desc) {
    if (is.null(desc)) {
        NULL
    } else if (is.character(desc) && length(desc) == 1) {
        desc
    } else {
        rlang::abort("{.arg desc} must be NULL or a single string.", call = parent.frame())
    }
}

# Printing -----------

# Internal helper: formats an "other" column type
format_col_other <- function(tt, col_nm, ansi, depth) {
    out = format(tt, ansi = ansi)
    list(out = c(out), nms = c("..."), levels = c(depth))
}

# Internal helper: formats a "schema_nest" column type
format_col_nest <- function(tt, col_nm, ansi, depth) {
    constraint_parts = c(
        if (!attr(tt, "required")) "optional"
    )
    all_parts = c(constraint_parts, "nested")
    mode_label = paste0("(", paste(all_parts, collapse = "; "), ")")
    if (isTRUE(ansi)) {
        mode_label = cli::col_grey(mode_label)
    }
    desc = attr(tt, "desc")
    hdr = if (!is.null(desc)) {
        paste0(mode_label, " ", desc, ":")
    } else {
        paste0(mode_label, ":")
    }
    # Header lives at the current depth; inner columns at depth+1
    out = c(hdr)
    nms = c(col_nm)
    levels = c(depth)
    inner = format_schema_cols(tt$cols, ansi = ansi, depth = depth + 1L)
    list(out = c(out, inner$out), nms = c(nms, inner$nms), levels = c(levels, inner$levels))
}

# Internal helper: formats a "schema_multiple" column type
format_col_multiple <- function(tt, col_nm, ansi, depth) {
    constraint_parts = c(
        if (!attr(tt, "required")) "optional",
        if (attr(tt$inner, "distinct")) "distinct"
    )
    all_parts = c("multiple", constraint_parts)
    mode_label = paste0("(", paste(all_parts, collapse = "; "), ")")
    if (isTRUE(ansi)) {
        mode_label = cli::col_grey(mode_label)
    }
    desc = attr(tt, "desc")
    hdr = if (!is.null(desc)) {
        paste0(mode_label, " ", desc, ": each ")
    } else {
        paste0(mode_label, ": each ")
    }
    inner_fmt = format(tt$inner, ansi = ansi, capitalize = FALSE)
    inner_fmt = paste0(
        inner_fmt,
        ".",
        if (!attr(tt$inner, "missing")) " No NAs allowed."
    )
    if (!is.null(tt$cross_msg)) {
        inner_fmt = paste0(inner_fmt, " Cross-column: ", tt$cross_msg(tt), ".")
    }
    combined_fmt = paste0(hdr, unname(inner_fmt))
    list(out = c(combined_fmt), nms = c(tt$name), levels = c(depth))
}

# Internal helper: formats a regular column type
format_col_regular <- function(tt, col_nm, ansi, depth) {
    fmt = format(tt, ansi = ansi)
    desc_nm = names(fmt)
    fmt = paste0(
        fmt,
        ".",
        if (!attr(tt, "missing")) " No NAs allowed."
    )
    names(fmt) = desc_nm
    if (!is.null(names(fmt))) {
        fmt = paste0(names(fmt), ": ", fmt)
    }
    # Add constraint label at the very start
    constraint_parts = c(
        if (!attr(tt, "required")) "optional",
        if (attr(tt, "distinct")) "distinct"
    )
    if (length(constraint_parts) > 0L) {
        constraint_label = paste0("(", paste(constraint_parts, collapse = "; "), ")")
        if (isTRUE(ansi)) {
            constraint_label = cli::col_grey(constraint_label)
        }
        fmt = paste0(constraint_label, " ", fmt)
    }
    list(out = c(unname(fmt)), nms = c(col_nm), levels = c(depth))
}

# Format a relationship tree as a human-readable string
format_relationship <- function(tree) {
    switch(
        tree$type,
        var = tree$name,
        cross = paste(
            vapply(tree$children, fmt_rel_child, "", parent_type = "cross"),
            collapse = " \u00d7 "
        ),
        compound = {
            inner = paste(vapply(tree$children, format_relationship, ""), collapse = " + ")
            paste0("(", inner, ")")
        },
        nest = {
            outer_str = fmt_rel_child(tree$outer, "nest")
            inner_str = fmt_rel_child(tree$inner, "nest")
            paste0(outer_str, " / ", inner_str)
        },
        rlang::abort(paste0("Unknown relationship node type: ", tree$type))
    )
}

# Format a child node, adding parens when the child type conflicts with the parent type
# (cross child that is a nest node, or nest child that is a cross node)
fmt_rel_child <- function(child, parent_type) {
    s <- format_relationship(child)
    needs_parens <- switch(
        parent_type,
        cross = child$type == "nest",
        nest = child$type == "cross",
        FALSE
    )
    if (needs_parens) paste0("(", s, ")") else s
}

# Internal helper: recursively formats schema columns, tracking nesting depth.
# Returns a list(out, nms, levels).
format_schema_cols <- function(cols, ansi = FALSE, depth = 0L) {
    out = character(0)
    nms = character(0)
    levels = integer(0)

    for (i in seq_along(cols)) {
        tt = cols[[i]]
        col_nm = names(cols)[i]

        col_result = switch(
            tt$type,
            other = format_col_other(tt, col_nm, ansi, depth),
            schema_nest = format_col_nest(tt, col_nm, ansi, depth),
            schema_multiple = format_col_multiple(tt, col_nm, ansi, depth),
            format_col_regular(tt, col_nm, ansi, depth)
        )

        out = c(out, col_result$out)
        nms = c(nms, col_result$nms)
        levels = c(levels, col_result$levels)
    }
    list(out = out, nms = nms, levels = levels)
}

#' @export
format.sch_schema <- function(x, ansi = FALSE, ...) {
    res = format_schema_cols(x$cols, ansi = ansi, depth = 0L)
    out = res$out
    names(out) = res$nms
    attr(out, "desc") = attr(x, "desc")
    out
}

#' @export
print.sch_schema <- function(x, ...) {
    if (!is.null(attr(x, "desc"))) {
        cat(cli::style_bold(attr(x, "desc")), "\n", sep = "")
    }
    n_req = sum(vapply(x$cols, function(y) attr(y, "required"), FALSE))
    hdr = cli::format_inline("A schema with {n_req} required element{?s}:")
    cat(cli::col_grey(hdr), "\n", sep = "")

    l_fmt = format_schema_cols(x$cols, ansi = TRUE, depth = 0L)
    fmt = l_fmt$out
    nms = l_fmt$nms
    lvls = l_fmt$levels
    max_lvl = max(lvls)

    # Per-level max name width (empty-string names contribute 0)
    w_by_lvl = vapply(0:max_lvl, function(l) max(cli::ansi_nchar(nms[lvls == l])), 0L)

    # Cumulative indent at each level: level 0 has no indent;
    # level L is indented by sum of (w_by_lvl[0:L-1] + 2) each
    cum_indent = integer(max_lvl + 1L)
    for (l in seq_len(max_lvl)) {
        cum_indent[l + 1L] = cum_indent[l] + w_by_lvl[l] + 2L
    }

    console_w = cli::console_width()
    for (i in seq_along(fmt)) {
        l = lvls[i]
        indent = strrep(" ", cum_indent[l + 1L])
        lbl = cli::ansi_align(cli::col_green(nms[i]), width = w_by_lvl[l + 1L], align = "right")
        prefix_width = cum_indent[l + 1L] + w_by_lvl[l + 1L] + 2L
        text_width = max(20L, console_w - prefix_width)
        lines = cli::ansi_strwrap(fmt[i], width = text_width)
        cat(indent, lbl, "  ", lines[1L], "\n", sep = "")
        if (length(lines) > 1L) {
            cont_indent = strrep(" ", prefix_width)
            for (j in seq_len(length(lines) - 1L) + 1L) {
                cat(cont_indent, lines[j], "\n", sep = "")
            }
        }
    }

    # Print relationships if present
    if (!is.null(x$relationships)) {
        cat(
            cli::col_grey("Relationships:"),
            " ",
            cli::style_italic(format_relationship(x$relationships)),
            "\n",
            sep = ""
        )
    }
}

#' @export
format.sch_type <- function(x, ansi = FALSE, capitalize = TRUE, ...) {
    if (x$type == "other") {
        if (isTRUE(ansi)) {
            return(cli::style_italic("Other columns"))
        } else {
            return("Other columns")
        }
    }
    if (x$type == "schema_multiple") {
        inner_msg = type_fns[[x$inner$type]]$msg(x$inner)
        if (!isTRUE(ansi)) {
            inner_msg = cli::ansi_strip(inner_msg)
        }
        a_an = if (grepl("^[aeiou]", inner_msg)) "an " else "a "
        out = paste0("(multiple) ", x$name, ": each column is ", a_an, inner_msg)
        names(out) = attr(x, "desc")
        return(out)
    }
    assert(x$type %in% names(type_fns), paste0("Unknown type: ", x$type))
    msg = type_fns[[x$type]]$msg(x)
    if (!isTRUE(ansi)) {
        msg = cli::ansi_strip(msg)
    }
    a_an = if (grepl("^[aeiou]", msg)) "An " else "A "
    if (!capitalize) {
        a_an = tolower(a_an)
    }
    out = paste0(a_an, msg)
    names(out) = attr(x, "desc")
    out
}
#' @export
print.sch_type <- function(x, ...) {
    fmt = format(x, ansi = TRUE)
    if (!is.null(names(fmt))) {
        cat(names(fmt), ": ", fmt, "\n", sep = "")
    } else {
        cat(fmt, "\n", sep = "")
    }
}
