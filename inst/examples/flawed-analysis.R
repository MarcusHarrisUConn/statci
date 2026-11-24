# Example script with multiple methodological hazards caught by statci::stat_lint()
df <- read.csv("/Users/researcher/Desktop/study_data.csv")

# Leakage: scaling before splitting
df$x_scaled <- scale(df$x)
idx <- sample(nrow(df), 80)
train <- df[idx, ]

# Uncentered multilevel interaction + missing convergence/singularity check
fit_mlm <- lme4::lmer(y ~ x * w + (1 + x | school), data = train)

# Unvalidated in-sample stepwise selection + in-sample RMSE evaluation
fit_step <- step(lm(y ~ x + w, data = train), trace = 0)
pred_in <- predict(fit_step, newdata = train)
in_sample_mse <- mean((train$y - pred_in)^2)

# Unadjusted multiple hypothesis testing + mutable F literal
pvals <- sapply(1:8, function(k) t.test(rnorm(20), rnorm(20), var.equal = F)$p.value)

# Unverified merge cardinality + missing drop = FALSE in helper + implicit na.rm masking
merged_df <- merge(df, lookup, by = "school")
extract_col <- function(mat, j) mat[, j]
avg_y <- mean(merged_df$y, na.rm = TRUE)
