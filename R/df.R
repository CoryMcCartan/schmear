#' Construct and validate a schema-aware data frame
#'
#' Bare-bones constructor for a data frame with an attached schema. This function
#' should be called by package developers writing their own internal constructors.
#' The only checks are for the types of `data` and `schema`.
#' The `validate_sch_df()` is a lightweight wrapper around [sch_validate()] that
#' also returns the input.
#'
#' @param data A data frame.
#' @param schema A `sch_schema` object.
#' @param groups A named list of character vectors of column names, for use
#'   with [sch_multiple()].
#' @param class Additional classes to add to the object, in addition to "sch_df" and tibble/data.frame classes.
#' @param use_tbl If `TRUE`, the returned object will have tibble
#'   classes `tbl_df` and `tbl` in addition to `data.frame`. As a reminder,
#'   `tbl_df` affects the behavior of the object (slicing, row names, etc.),
#'   while `tbl` affects printing only.
#'
#' @returns A data frame with class `c(class, "sch_df", ...)` and attributes
#'   `sch_schema` and `sch_groups`.
#'
#' @examples
#' schema = sch_schema(
#'     .desc = "MCMC draws",
#'     .relationships = ~ chain * draw * parameter,
#'     chain = sch_integer("Chain number"),
#'     draw = sch_integer("Draw number", bounds = c(1, Inf), closed = c(TRUE, FALSE)),
#'     parameter = sch_character("Parameter name"),
#'     value = sch_numeric("Parameter value")
#' )
#' d_raw = data.frame(chain = 1L, draw = 1:4, parameter = "mu", value = rnorm(4))
#' d = new_sch_df(d_raw, schema, class="mcmc_draws")
#' validate_sch_df(d)
#' str(d)
#' @name sch_df
NULL

#' @rdname sch_df
#' @export
new_sch_df <- function(data, schema, groups = NULL, class = NULL, use_tbl = TRUE) {
    assert(is.list(data), "{.arg data} must be a data frame or list")
    assert(inherits(schema, "sch_schema"), "{.arg schema} must be a {.cls sch_schema}")

    if (isTRUE(use_tbl)) {
        class = c(class, "sch_df", "tbl_df", "tbl")
    } else {
        class = c(class, "sch_df")
    }

    vctrs::new_data_frame(
        data,
        sch_schema = schema,
        sch_groups = groups,
        class = class
    )
}

#' @param x A `sch_df` object.
#' @returns The input, if validation is successful.
#'
#' @rdname sch_df
#' @export
validate_sch_df <- function(x) {
    sch_validate(attr(x, "sch_schema"), x)
    x
}


#' @exportS3Method pillar::tbl_sum
tbl_sum.sch_df <- function(x, ...) {
    sch = attr(x, "sch_schema")
    pre = paste(length(sch$cols), "column rules")
    desc = attr(sch, "desc")
    names(pre) = if (is.null(desc)) "<sch_df>" else desc
    c(pre, NextMethod())
}
