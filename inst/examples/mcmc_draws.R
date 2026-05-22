# Example: MCMC draws schema
#
# Structure: chain x draw x param fully crossed.
# Every (chain, draw, param) combination exists exactly once.

library(schmear)

# Schema -----------------------------------------------------------------------

schema <- sch_schema(
    .desc = "MCMC posterior draws",
    .relationships = ~ chain * draw * param,
    chain = sch_integer("Chain number", bounds = c(1, Inf)),
    draw = sch_integer("Draw number within chain", bounds = c(1, Inf)),
    param = sch_factor(
        "Parameter name",
        levels = c("mu", paste0("beta", 1:20), "sigma", "log_lik"),
        strict = FALSE
    ),
    value = sch_numeric("Parameter draw value")
)

print(schema)

# Compliant data ---------------------------------------------------------------

n_chains <- 4L
n_draws <- 10000L
params <- c("mu", paste0("beta", 1:20), "sigma", "log_lik")

draws_df <- expand.grid(
    chain = seq_len(n_chains),
    draw = seq_len(n_draws),
    param = params,
    stringsAsFactors = FALSE
)
draws_df$chain <- as.integer(draws_df$chain)
draws_df$draw <- as.integer(draws_df$draw)
draws_df$param <- factor(draws_df$param, levels = params)
draws_df$value <- c(
    rnorm(n_chains * n_draws),                  # mu draws
    rnorm(n_chains * n_draws * 20, 0, 0.5),     # beta draws
    abs(rnorm(n_chains * n_draws, 1, 0.2)),     # sigma draws (positive)
    rnorm(n_chains * n_draws, -200, 10)         # log_lik draws
)

draws_df <- draws_df[order(draws_df$chain, draws_df$draw, draws_df$param), ]
rownames(draws_df) <- NULL

cat("\nCompliant data (first 12 rows):\n")
print(head(draws_df, 12))
cat("\nValidating compliant data...\n")
sch_validate(schema, draws_df)
cat("OK\n")

# Corruptions ------------------------------------------------------------------

cat("\n--- Corruption 1: wrong type for 'value' (character instead of numeric) ---\n")
bad1 <- draws_df
bad1$value <- as.character(bad1$value)
try(sch_validate(schema, bad1))

cat("\n--- Corruption 2: missing required 'chain' column ---\n")
bad2 <- draws_df[, c("draw", "param", "value")]
try(sch_validate(schema, bad2))

cat("\n--- Corruption 3: duplicate (chain, draw, param) combo ---\n")
bad3 <- draws_df
bad3$draw[bad3$chain == 1L & bad3$draw == 100L] <- 1L
try(sch_validate(schema, bad3))

cat("\n--- Corruption 4: incomplete crossing (chain 2 missing log_lik) ---\n")
bad4 <- draws_df[!(draws_df$chain == 2L & draws_df$param == "log_lik"), ]
try(sch_validate(schema, bad4))

cat("\n--- Corruption 5: out-of-bounds draw number (draw = 0) ---\n")
bad5 <- draws_df
bad5$draw[bad5$chain == 1L & bad5$draw == 1L] <- 0L
try(sch_validate(schema, bad5))

cat("\n--- Corruption 6: NA in value column ---\n")
bad6 <- draws_df
bad6$value[1] <- NA_real_
# value allows missing by default, so this should PASS
sch_validate(schema, bad6)
cat("(Passed - 'value' allows NAs by default)\n")

# Stricter schema that forbids NAs in value
schema_strict <- sch_schema(
    .desc = "MCMC posterior draws (strict: no NA values)",
    .relationships = ~ chain * draw * param,
    chain = sch_integer("Chain number", bounds = c(1, Inf)),
    draw = sch_integer("Draw number within chain", bounds = c(1, Inf)),
    param = sch_factor(
        "Parameter name",
        levels = c("mu", "sigma", "log_lik"),
        strict = FALSE
    ),
    value = sch_numeric("Parameter draw value", missing = FALSE)
)

try(sch_validate(schema_strict, bad6))
