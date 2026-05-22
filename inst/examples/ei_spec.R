# Example: Ecological inference specification (matching 'seine' package)
#
# An ei_spec data frame has one row per geographic unit (e.g., county).
# It contains:
#   - predictor columns (racial composition shares, summing to 1 per row)
#   - outcome columns (vote share estimates, summing to 1 per row)
#   - a `total` column (number of individuals per unit, e.g. voters)
#   - optional covariate columns
#
# The sch_groups attribute maps group names ("predictors", "outcomes") to
# the actual column names in the data frame. Cross-column checks enforce that
# both groups sum to 1 in every row.

library(schmear)

# Schema -----------------------------------------------------------------------

schema <- sch_schema(
    .desc = "Ecological inference specification",
    sch_multiple(
        name = "predictors",
        desc = "Racial composition shares",
        type = sch_numeric(bounds = c(0, 1), missing = FALSE),
        check = function(x, type) {
            row_sums <- Reduce("+", x)
            all(abs(row_sums - 1) < 1e-6)
        },
        msg = function(type) "rows sum to 1",
        coerce = function(x, type) x
    ),
    sch_multiple(
        name = "outcomes",
        desc = "Vote share estimates",
        type = sch_numeric(missing = FALSE),
    ),
    sch_multiple(
        name = "covariates",
        type = sch_numeric(missing = FALSE),
        required = FALSE,
    ),
    total = sch_numeric("Total individuals per unit", bounds = c(1, Inf), missing = FALSE),
    sch_others()
)

print(schema)

# Compliant data ---------------------------------------------------------------

n_units <- 50L

# Generate Dirichlet-like composition data (rows sum exactly to 1)
dirichlet_sample <- function(n, k, alpha = rep(1, k)) {
    raw <- matrix(rgamma(n * k, shape = alpha), nrow = n, byrow = TRUE)
    raw / rowSums(raw)
}

pred_shares <- dirichlet_sample(n_units, 3)
out_shares <- dirichlet_sample(n_units, 3)

df <- data.frame(
    vap_white = pred_shares[, 1],
    vap_black = pred_shares[, 2],
    vap_other = pred_shares[, 3],
    pres_dem = out_shares[, 1],
    pres_rep = out_shares[, 2],
    pres_other = out_shares[, 3],
    total = as.integer(runif(n_units, 500, 50000)),
    income_med = runif(n_units, 30000, 120000)
)
class(df) <- c("ei_spec", "sch_df", "tbl_df", "tbl", "data.frame")

attr(df, "sch_groups") <- list(
    predictors = c("vap_white", "vap_black", "vap_other"),
    outcomes = c("pres_dem", "pres_rep", "pres_other"),
    covariates = "income_med"
)

cat("\nCompliant data (first 5 rows):\n")
print(head(df, 5))
cat("\nValidating compliant data...\n")
sch_validate(schema, df)
cat("OK\n")

# Corruptions ------------------------------------------------------------------

cat("\n--- Corruption 1: missing sch_groups attribute ---\n")
bad1 <- df
attr(bad1, "sch_groups") <- NULL
try(sch_validate(schema, bad1))

cat("\n--- Corruption 2: predictor column out of [0, 1] bounds ---\n")
bad2 <- df
bad2$vap_white[1] <- 1.5
try(sch_validate(schema, bad2))

cat("\n--- Corruption 3: predictor shares do not sum to 1 ---\n")
bad3 <- df
bad3$vap_white[1] <- 0.1
try(sch_validate(schema, bad3))

cat("\n--- Corruption 4: missing 'total' column ---\n")
bad4 <- df[, setdiff(names(df), "total")]
attr(bad4, "sch_groups") <- attr(df, "sch_groups")
try(sch_validate(schema, bad4))

cat("\n--- Corruption 5: total contains zero (below lower bound of 1) ---\n")
bad5 <- df
bad5$total[1] <- 0L
try(sch_validate(schema, bad5))

cat("\n--- Corruption 6: predictor column contains NA ---\n")
bad6 <- df
bad6$vap_white[1] <- NA_real_
try(sch_validate(schema, bad6))

cat("\n--- Corruption 7: sch_groups references a column not present in data ---\n")
bad7 <- df[, setdiff(names(df), "vap_white")]
attr(bad7, "sch_groups") <- attr(df, "sch_groups")
try(sch_validate(schema, bad7))

cat("\n--- Corruption 8: outcomes group absent from sch_groups ---\n")
bad8 <- df
attr(bad8, "sch_groups") <- list(predictors = c("vap_white", "vap_black", "vap_other"))
try(sch_validate(schema, bad8))

cat("\n--- Corruption 9: outcomes are character, not numeric ---\n")
bad9 <- df
bad9$pres_dem <- as.character(bad9$pres_dem)
try(sch_validate(schema, bad9))
