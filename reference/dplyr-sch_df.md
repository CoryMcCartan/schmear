# dplyr integration for sch_df

These methods hook into dplyr's three extension generics
([`dplyr::dplyr_row_slice()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html),
[`dplyr::dplyr_col_modify()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html),
[`dplyr::dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html))
plus the base-R [`names()`](https://rdrr.io/r/base/names.html)
replacement and 1-d `[` to enforce schema constraints across dplyr
operations with near-zero overhead.

## Details

Each method calls
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) first
(performing the actual dplyr operation) and then re-validates the result
against the schema, running only the checks that can plausibly be
violated by that type of operation:

|  |  |
|----|----|
| Operation | Checks run |
| `dplyr_row_slice` | none by default; `"relationships"` behind a flag |
| `dplyr_col_modify` | `"names"`, `"types"`, `"distinct"` |
| `dplyr_reconstruct` | `"names"`, `"types"` |
| `[` (1-d column subsetting) | `"names"` |
| `names<-` | errors if any *schema* column name is changed |

## Row slicing (`arrange`, `filter`, `slice`, semi/anti joins)

Row operations cannot introduce new name or type violations, so no
validation is run by default. Relationship constraints (crossing
completeness, primary-key uniqueness) *can* be broken by removing rows.
Pass `.check_relationships = TRUE` in `...` to opt in to that check.

## Column modification (`mutate`)

A column modification can delete required columns (via `NULL`
assignment), assign the wrong type, or introduce duplicate values into a
`distinct = TRUE` column, so all three cheap checks are run.

## Reconstruction (joins)

`dplyr_reconstruct()` is called after joins. Only names and types are
checked: distinct and relationship checks are omitted because joins can
intentionally produce non-distinct rows or incomplete crossings.

## Column subsetting (`select`, `relocate`)

A 1-d `[` call selects or reorders columns. Reordering is always safe;
but selecting a column subset could drop required columns, so a names
check is run.

## Renaming (`rename`, `rename_with`, `select` with rename)

Renaming a column that belongs to the schema (either directly or as a
member of a
[`sch_multiple()`](http://corymccartan.com/schmear/reference/sch_schema.md)
group) is never valid without also updating the schema definition, so an
error is raised immediately.
