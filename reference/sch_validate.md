# Validate a data frame against a schema

Checks that a data frame conforms to a schema, validating column
presence, types, missing values, uniqueness constraints, and nesting
structure.

## Usage

``` r
sch_validate(
  schema,
  data,
  check = c("names", "types", "distinct", "relationships"),
  call = rlang::caller_env()
)
```

## Arguments

- schema:

  A schema object created by \[sch_schema()\].

- data:

  A data frame to validate.

- check:

  A character vector specifying which checks to perform. The default
  runs all checks. Possible values: - \`"names"\`: check for missing
  required columns and unexpected extra columns. - \`"types"\`: check
  column types and missing-value (\`NA\`) constraints. - \`"distinct"\`:
  check uniqueness constraints for columns marked \`distinct = TRUE\`.
  Relatively expensive. - \`"relationships"\`: validate relationship
  formulas (primary-key uniqueness and crossing/nesting completeness).
  Relatively expensive.

- call:

  The environment or call used for error reporting, passed to
  \[rlang::abort()\]. Useful when wrapping \`sch_validate()\` inside
  another function so that the error points to the right place.

## Value

\`data\`, invisibly, if validation succeeds. Otherwise, an error of
class \`sch_validation_error\` is raised with a formatted summary of all
issues found.

## Examples

``` r
# Basic validation: valid data passes silently
schema <- sch_schema(
    id = sch_integer(distinct = TRUE),
    name = sch_character(missing = FALSE),
    age = sch_numeric(required = FALSE)
)
df <- data.frame(id = 1:3, name = c("Alice", "Bob", "Carol"), age = c(25, NA, 30))
sch_validate(schema, df)

# Invalid, throw validation errors
if (FALSE) { # \dontrun{
# missing required columns
sch_validate(schema, data.frame(id = 1:2))

# type constrains not satisfied
sch_validate(schema, data.frame(id = c(1L, 1L), name = c("Alice", NA)))
} # }
```
