# Construct and validate a schema-aware data frame

Bare-bones constructor for a data frame with an attached schema. This
function should be called by package developers writing their own
internal constructors. The only checks are for the types of `data` and
`schema`. The `validate_sch_df()` is a lightweight wrapper around
[`sch_validate()`](http://corymccartan.com/schmear/reference/sch_validate.md)
that also returns the input.

## Usage

``` r
new_sch_df(data, schema, groups = NULL, class = NULL, use_tbl = TRUE)

validate_sch_df(x)
```

## Arguments

- data:

  A data frame.

- schema:

  A `sch_schema` object.

- groups:

  A named list of character vectors of column names, for use with
  [`sch_multiple()`](http://corymccartan.com/schmear/reference/sch_schema.md).

- class:

  Additional classes to add to the object, in addition to "sch_df" and
  tibble/data.frame classes.

- use_tbl:

  If `TRUE`, the returned object will have tibble classes `tbl_df` and
  `tbl` in addition to `data.frame`. As a reminder, `tbl_df` affects the
  behavior of the object (slicing, row names, etc.), while `tbl` affects
  printing only.

- x:

  A `sch_df` object.

## Value

A data frame with class `c(class, "sch_df", ...)` and attributes
`sch_schema` and `sch_groups`.

The input, if validation is successful.

## Examples

``` r
schema = sch_schema(
    .desc = "MCMC draws",
    .relationships = ~ chain * draw * parameter,
    chain = sch_integer("Chain number"),
    draw = sch_integer("Draw number", bounds = c(1, Inf), closed = c(TRUE, FALSE)),
    parameter = sch_character("Parameter name"),
    value = sch_numeric("Parameter value")
)
d_raw = data.frame(chain = 1L, draw = 1:4, parameter = "mu", value = rnorm(4))
d = new_sch_df(d_raw, schema, class="mcmc_draws")
validate_sch_df(d)
#> # MCMC draws: 4 column rules
#> # A tibble:   4 × 4
#>   chain  draw parameter    value
#>   <int> <int> <chr>        <dbl>
#> 1     1     1 mu        -1.40   
#> 2     1     2 mu         0.255  
#> 3     1     3 mu        -2.44   
#> 4     1     4 mu        -0.00557
str(d)
#> mcmc_drw [4 × 4] (S3: mcmc_draws/sch_df/tbl_df/tbl/data.frame)
#>  $ chain    : int [1:4] 1 1 1 1
#>  $ draw     : int [1:4] 1 2 3 4
#>  $ parameter: chr [1:4] "mu" "mu" "mu" "mu"
#>  $ value    : num [1:4] -1.40004 0.25532 -2.43726 -0.00557
#>  - attr(*, "sch_schema")=List of 2
#>   ..$ cols         :List of 4
#>   .. ..$ chain    :List of 3
#>   .. .. ..$ type  : chr "integer"
#>   .. .. ..$ bounds: num [1:2] -Inf Inf
#>   .. .. ..$ closed: logi [1:2] TRUE TRUE
#>   .. .. ..- attr(*, "desc")= chr "Chain number"
#>   .. .. ..- attr(*, "missing")= logi TRUE
#>   .. .. ..- attr(*, "required")= logi TRUE
#>   .. .. ..- attr(*, "distinct")= logi FALSE
#>   .. .. ..- attr(*, "class")= chr "sch_type"
#>   .. ..$ draw     :List of 3
#>   .. .. ..$ type  : chr "integer"
#>   .. .. ..$ bounds: num [1:2] 1 Inf
#>   .. .. ..$ closed: logi [1:2] TRUE FALSE
#>   .. .. ..- attr(*, "desc")= chr "Draw number"
#>   .. .. ..- attr(*, "missing")= logi TRUE
#>   .. .. ..- attr(*, "required")= logi TRUE
#>   .. .. ..- attr(*, "distinct")= logi FALSE
#>   .. .. ..- attr(*, "class")= chr "sch_type"
#>   .. ..$ parameter:List of 1
#>   .. .. ..$ type: chr "character"
#>   .. .. ..- attr(*, "desc")= chr "Parameter name"
#>   .. .. ..- attr(*, "missing")= logi TRUE
#>   .. .. ..- attr(*, "required")= logi TRUE
#>   .. .. ..- attr(*, "distinct")= logi FALSE
#>   .. .. ..- attr(*, "class")= chr "sch_type"
#>   .. ..$ value    :List of 3
#>   .. .. ..$ type  : chr "numeric"
#>   .. .. ..$ bounds: num [1:2] -Inf Inf
#>   .. .. ..$ closed: logi [1:2] TRUE TRUE
#>   .. .. ..- attr(*, "desc")= chr "Parameter value"
#>   .. .. ..- attr(*, "missing")= logi TRUE
#>   .. .. ..- attr(*, "required")= logi TRUE
#>   .. .. ..- attr(*, "distinct")= logi FALSE
#>   .. .. ..- attr(*, "class")= chr "sch_type"
#>   ..$ relationships:List of 2
#>   .. ..$ type    : chr "cross"
#>   .. ..$ children:List of 3
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ type: chr "var"
#>   .. .. .. ..$ name: chr "chain"
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ type: chr "var"
#>   .. .. .. ..$ name: chr "draw"
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ type: chr "var"
#>   .. .. .. ..$ name: chr "parameter"
#>   ..- attr(*, "desc")= chr "MCMC draws"
#>   ..- attr(*, "class")= chr "sch_schema"
```
