# Copilot Instructions for schmear

## Build & Test Commands

**Full test suite:**

``` r

devtools::test()
```

**Single test file or context:**

``` r

devtools::test(filter = "schema")     # test-schema.R
devtools::test(filter = "validate")   # test-validate.R
```

**Generate documentation from roxygen:**

``` r

devtools::document()
```

**Full package check (runs R CMD CHECK equivalent):**

``` r

devtools::check()
```

**Code formatting:** Uses the [air](https://github.com/posit-dev/air)
formatter configured in `air.toml` (100 char line width, 4-space
indent). Run after all edits with `air format .` from the main
directory.

## Package Architecture

**schmear** is a developer-focused package for creating structured data
frame subtypes. It has three core layers:

### 1. Schema Definition (R/schema.R)

- **`sch_schema(...)`** – Top-level function that defines a structured
  data frame schema. Takes named column specifications (type
  constructors) and an optional description.
- **Type Constructors** –
  [`sch_numeric()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_integer()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_character()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_factor()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_date()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_datetime()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_logical()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_inherits()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_list_of()`](http://corymccartan.com/schmear/reference/sch_schema.md),
  [`sch_custom()`](http://corymccartan.com/schmear/reference/sch_schema.md).
- **Type registry** – `type_fns` list maps type names to
  check/msg/coerce functions. Custom types are added the same way.
- **Coercion convention** – `type_fns[[type]]$coerce(x, type)` always
  accepts the value and the full type spec. Wrap base coercers like
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) /
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) so the second
  argument is not misinterpreted.

### 2. Validation (R/validate.R)

- **`sch_validate(schema, data, check=c("names","types","distinct","relationships"), ...)`**
  – Core validation function.
- **Check phases** (independent; each can be toggled):
  - `"names"` → `validate_names()` – missing required columns, extra
    columns, missing inner columns in nests
  - `"types"` → `validate_types_missing()` – type mismatches, NAs when
    `missing=FALSE`
  - `"distinct"` → `validate_distinct()` – duplicate values in
    `distinct=TRUE` columns (NAs excluded per semantics)
  - `"relationships"` → `validate_relationships()` – key uniqueness,
    crossing completeness, nesting completeness
- **Error handling** – Collects *all* issues before raising a single
  `sch_validation_error` with attached `$issues` list.
- **Nested schemas** – Validation recurses into each nested data-frame
  element and tracks the nested path plus element index in issues.

### 3. Display & Format (R/schema.R + R/validate.R)

- **`print.sch_schema()`** – Pretty-prints schema with indentation for
  nesting levels, type descriptions, and constraint flags.
- **`format.sch_schema()`** – Returns named character vector for
  internal consumption.

## Key Conventions

### Type Definition Pattern

Every type in `type_fns` has this structure:

``` r

type_name = list(
    check = function(x, type) { ... },      # Returns TRUE/FALSE; must handle NA gracefully
    msg = function(type) { ... },           # Returns human-readable type description
    coerce = function(x, type) { ... }      # Attempts coercion (may fail)
)
```

**Important:** Check functions must wrap non-logical results with
[`isTRUE()`](https://rdrr.io/r/base/Logic.html) in validation to avoid
crashes on NA returns.

### NA and NULL Semantics

- **Regular columns** – NAs are explicitly allowed/forbidden per
  `missing` attribute.
- **List columns** – `NULL` list elements are treated as “missing”
  (analogous to NA).
- **Distinct checking** – NAs/NULLs are **excluded** from uniqueness
  checks when `missing=TRUE` (no “duplicate NA” error).
- **Custom checks** – Must handle NAs gracefully; if a check returns
  `NA`, it’s treated as a type failure, not a crash.

## Important Implementation Notes

1.  **sch_validate() modularization** – Each check phase can run
    independently.
2.  **vctrs integration** – Uses
    [`vctrs::vec_unique_count()`](https://vctrs.r-lib.org/reference/vec_unique.html),
    `vec_group_loc()`, `vec_slice()` for vectorized operations; handles
    NA semantics correctly.
3.  **Error collection** – All validators return a list of issues
    (possibly empty); issues are collected, then printed as a bullet
    list with
    [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
4.  **Tests** – Prefer
    [`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
    with `expect_*` assertions, matching the existing `test-schema.R`
    and `test-validate.R` style.
