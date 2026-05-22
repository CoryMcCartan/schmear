# Coerce a data frame to conform to a schema

Attempts to coerce each column of \`data\` to the type expected by
\`schema\`, using the coercion method defined for each column type.
After coercion, optionally validates the result with \[sch_validate()\].

## Usage

``` r
sch_coerce(schema, data, validate = TRUE, call = rlang::caller_env())
```

## Arguments

- schema:

  A schema object created by \[sch_schema()\].

- data:

  A data frame to coerce.

- validate:

  If \`TRUE\` (default), \[sch_validate()\] is called on the coerced
  data after all columns have been processed. Set to \`FALSE\` to skip
  validation, which can be useful when you want to inspect the coerced
  result before checking constraints.

- call:

  The environment or call used for error reporting, passed to
  \[rlang::abort()\]. Useful when wrapping \`sch_coerce()\` inside
  another function so that errors point to the right place.

## Value

\`data\` with columns coerced to their schema types, invisibly, if
coercion succeeds. If any column cannot be coerced, an error of class
\`sch_coercion_error\` is raised with a summary of all failures. When
\`validate = TRUE\`, a subsequent \[sch_validate()\] call may also raise
a \`sch_validation_error\` if the coerced data still violates schema
constraints (e.g., out-of-bounds values or uniqueness violations).

## Details

Coercion is applied column-by-column using the \`coerce\` function
registered for each type in the internal \`type_fns\` registry. For
example, a column specified as \`sch_integer()\` will be coerced with
\[as.integer()\]. Nested schemas (created with \[sch_nest()\]) are
handled by recursing into each element data frame, and grouped columns
(created with \[sch_multiple()\]) have each member column coerced
individually.

Columns present in \`data\` but not named in \`schema\` (i.e., those
covered by \[sch_others()\]) are left untouched.

## Examples

``` r
schema <- sch_schema(
    id = sch_integer(distinct = TRUE),
    name = sch_character(missing = FALSE),
    score = sch_numeric()
)

# Coerce a data frame with character columns
df <- data.frame(id = c("1", "2", "3"), name = c("Alice", "Bob", "Carol"), score = 1:3)
str(sch_coerce(schema, df))
#> 'data.frame':    3 obs. of  3 variables:
#>  $ id   : int  1 2 3
#>  $ name : chr  "Alice" "Bob" "Carol"
#>  $ score: num  1 2 3

# Nested schema coercion
nested_schema <- sch_schema(
    group = sch_factor(),
    info = sch_nest(x = sch_numeric(), y = sch_integer())
)
nested_df <- data.frame(group = "A")
nested_df$info <- list(data.frame(x = "1.5", y = "2"))
str(sch_coerce(nested_schema, nested_df))
#> 'data.frame':    1 obs. of  2 variables:
#>  $ group: Factor w/ 1 level "A": 1
#>  $ info :List of 1
#>   ..$ :'data.frame': 1 obs. of  2 variables:
#>   .. ..$ x: num 1.5
#>   .. ..$ y: int 2
```
