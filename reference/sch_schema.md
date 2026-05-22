# Define a structured data type

Defines the structure of a single 'observation' for a structured data
frame. Each column has type restrictions and may be required or
optional. Schemas support nesting relationships.

## Usage

``` r
sch_schema(..., .desc = NULL, .relationships = NULL)

sch_others()

sch_any(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE)

sch_multiple(
  name,
  type,
  desc = NULL,
  required = TRUE,
  check = NULL,
  msg = NULL,
  coerce = NULL
)

sch_nest(..., .desc = NULL)

sch_numeric(
  desc = NULL,
  bounds = c(-Inf, Inf),
  closed = c(TRUE, TRUE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_integer(
  desc = NULL,
  bounds = c(-Inf, Inf),
  closed = c(TRUE, TRUE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_logical(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE)

sch_character(desc = NULL, missing = TRUE, required = TRUE, distinct = FALSE)

sch_factor(
  desc = NULL,
  levels = NULL,
  strict = TRUE,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_date(
  desc = NULL,
  bounds = c(as.Date(-Inf), as.Date(Inf)),
  closed = c(FALSE, FALSE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_datetime(
  desc = NULL,
  bounds = c(as.POSIXct(-Inf), as.POSIXct(Inf)),
  closed = c(FALSE, FALSE),
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_inherits(
  desc = NULL,
  class,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_list_of(
  desc = NULL,
  class,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)

sch_custom(
  name,
  desc = NULL,
  check,
  msg,
  coerce,
  ...,
  missing = TRUE,
  required = TRUE,
  distinct = FALSE
)
```

## Arguments

- ...:

  Column specifications, in the form of \`col_name = col_type\` pairs,
  where \`col_type\` is a call to a column type constructor listed here,
  such as \`sch_numeric()\`. Every type must be a kind of vector, i.e.,
  \[vctrs::obj_is_vector()\] must return \`TRUE\`.

  All columns must be named, except for \`sch_others()\`, as described
  below, and \`sch_multiple()\`, which describes a group of columns
  sharing the same type. A named \`sch_nest()\` describes columns stored
  as a nested data frame.

  The special function \`sch_others()\` indicates the preferred location
  of other columns not explicitly mentioned in the schema. If no
  \`sch_others()\` appears, then other columns are not allowed. Trailing
  commas are permitted.

- .relationships:

  An optional one-sided formula describing the structural relationships
  between values in different columns. Formulas can only involve named
  arguments to \`...\`. Use \`\*\` to signify crossed levels, which will
  verify all combinations exist, \`/\` to signify nested levels, and
  \`+\` to create compound keys (bundling columns into a single
  identifier). See the examples below.

- desc, .desc:

  A description of the column for consumers of the schema. The type
  contraints will be described separately and do not need to be included
  in the description. For example for "age", the description might be
  "Age of the patient in years", not "Non-negative integer representing
  the age of the patient in years". For the overall \`sch_schema\`, the
  \`desc\` will be printed as part of the header for data frames
  implementing the schema, by default.

- missing:

  If \`TRUE\`, the column may be contain missing values. Otherwise, any
  missing values result in an error.

- required:

  If \`TRUE\` (default), the group entry in \`sch_groups\` must contain
  at least one column name. If \`FALSE\`, an empty character vector for
  that entry is also accepted.

- distinct:

  If \`TRUE\`, the column must contain no duplicate values (after
  accounting for nesting structure).

- name:

  A name for the custom type.

- type:

  A column type constructor (e.g. \[sch_numeric()\]) specifying the
  expected type of every column in the group.

- check:

  A two-argument function that checks whether an object satisfies the
  type. The first argument is the object to check, and the second is the
  full type specification.

- msg:

  A one-argument function that generates a descriptive message about the
  type when passed the type object itself. Should not end with a period.

- coerce:

  A two-argument function that attempts to coerce an object to the type.
  The first argument is the object to coerce, and the second is the full
  type specification.

- bounds:

  Length-two vector \`c(min, max)\` specifying the allowed range of
  values.

- closed:

  Length-two logical vector specifying whether the bounds are closed
  (inclusive) or open (exclusive).

- levels:

  A character vector of factor levels, or NULL not enforce specific
  levels.

- strict:

  If \`TRUE\`, only factors with the specified levels are accepted. If
  \`FALSE\`, character vectors with the specified levels are also
  accepted.

- class:

  A character vector of class names.

## Value

An object of class \`sch_schema\`,

## Functions

- `sch_others()`: A placeholder for other non-required columns in a
  schema.

- `sch_any()`: A column of any type. No type checking is performed.

- `sch_multiple()`: A group of multiple columns sharing the same type.
  The group is identified by \`name\`, which must appear as an entry in
  the \`sch_groups\` attribute of the data frame being validated. That
  entry is a character vector of column names that belong to this group.

  Optionally accepts cross-column \`check\`, \`msg\`, and \`coerce\`
  functions that are applied to the entire group after per-column type
  checks pass. These must all be provided together or not at all.

  \`sch_multiple()\` must be unnamed in an \`sch_schema()\` call.
  Per-column constraints such as \`missing\` and \`distinct\` are set on
  the inner \`type\` argument.

- `sch_nest()`: A set of columns stored as a nested list-column of data
  frames. Must be given a name in the outer \`sch_schema()\`.

- `sch_numeric()`: A numeric vector that is optionally constrained to be
  within a certain range.

- `sch_integer()`: An integer vector that is optionally constrained to
  be within a certain range.

- `sch_logical()`: A logical vector.

- `sch_character()`: A character vector.

- `sch_factor()`: A factor with specified levels.

- `sch_date()`: A Date vector that is optionally constrained to be
  within a certain range.

- `sch_datetime()`: A POSIXct vector that is optionally constrained to
  be within a certain range.

- `sch_inherits()`: A list-column whose elements satisfy \`inherits(\_,
  class)\`.

- `sch_list_of()`: A vector satisfying \`inherits(\_, class)\`.

- `sch_custom()`: A custom type defined by user-provided check, type
  message, and coercion functions. Additional named values to be stored
  along with the type specification may be passed via \`...\` and will
  be available to the check, message, and coercion function as elements
  of the \`type\` argument.

## Examples

``` r
sch_schema(
    .desc = "MCMC draws",
    .relationships = ~ chain * draw * parameter,
    chain = sch_integer("Chain number"),
    draw = sch_integer("Draw number", bounds = c(1, Inf), closed = c(TRUE, FALSE)),
    parameter = sch_factor("Parameter name", levels = c("mu", "sigma", "log_lik")),
    value = sch_numeric("Parameter value")
)
#> MCMC draws
#> A schema with 4 required elements:
#>     chain  Chain number: An integer vector.
#>      draw  Draw number: An integer vector with values in [1, Inf).
#> parameter  Parameter name: A factor; one of mu, sigma, or log_lik.
#>     value  Parameter value: A numeric vector.
#> Relationships: chain × draw × parameter

sch_schema(
    .desc = "Student data",
    .relationships = ~ (grade + teacher) / table_group,
    birthday = sch_date("Date of birth", required = FALSE),
    height = sch_numeric(
        "Height in inches",
        bounds = c(0, 108),
        closed = c(FALSE, TRUE)
    ),
    grade = sch_factor(strict = FALSE, levels = c("Kindergarten", "1st", "2nd")),
    teacher = sch_nest(
        first = sch_character("First name"),
        last = sch_character("Last name")
    ),
    table_group = sch_integer(bounds=c(1, 6)),
    enrolled = sch_logical(missing = FALSE),
    sch_others()
)
#> Student data
#> A schema with 5 required elements:
#>    birthday  (optional) Date of birth: A date vector.
#>      height  Height in inches: A numeric vector with values in (0, 108].
#>       grade  A factor or character; one of Kindergarten, 1st, or 2nd.
#>     teacher  (nested):
#>              first  First name: A character vector.
#>               last  Last name: A character vector.
#> table_group  An integer vector with values in [1, 6].
#>    enrolled  A logical vector. No NAs allowed.
#>         ...  Other columns
#> Relationships: (grade + teacher) / table_group

sch_schema(
    .desc = "Causal inference data",
    treatment = sch_factor(levels = c("control", "treatment"), missing = FALSE),
    outcome = sch_numeric(missing = FALSE),
    sch_multiple("covariates", type = sch_any(missing = FALSE), required = FALSE),
    sch_others()
)
#> Causal inference data
#> A schema with 2 required elements:
#>  treatment  A factor; one of control or treatment. No NAs allowed.
#>    outcome  A numeric vector. No NAs allowed.
#> covariates  (multiple; optional): each a vector of any type. No NAs allowed.
#>        ...  Other columns

sch_custom(
   name = "even",
   check = function(x, type) is.integer(x) && all(x %% 2 == 0),
   msg = function(type) "vector of even integers",
   coerce = function(x, type) (as.integer(x) %/% 2) * 2
)
#> A vector of even integers
```
