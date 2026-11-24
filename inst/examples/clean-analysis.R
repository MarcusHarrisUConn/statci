# Clean, reproducible statistical script passing all 17 built-in statci rules
RNGkind("L'Ecuyer-CMRG")
set.seed(2026)

df <- data.frame(
  school = factor(rep(1:10, each = 20)),
  x = rnorm(200),
  w = rnorm(200)
)
df$y <- 1.0 + 0.6 * df$x - 0.3 * df$w + rnorm(200, sd = 0.7)

# Split before preprocessing
idx <- sample(nrow(df), 140)
train <- df[idx, ]
test <- df[-idx, ]

# Cluster-mean and grand-mean centering on training split
train$x_wc <- train$x - ave(train$x, train$school)
train$w_gmc <- as.numeric(scale(train$w, center = TRUE, scale = FALSE))

# Linear model evaluated out-of-sample on test set
fit_lm <- lm(y ~ x + w, data = train)
pred_test <- predict(fit_lm, newdata = test)
test_mse <- mean((test$y - pred_test)^2)

# Multiple tests adjusted with Benjamini-Hochberg FDR
raw_p <- vapply(1:5, function(k) stats::t.test(rnorm(25), var.equal = FALSE)$p.value, numeric(1))
adj_p <- stats::p.adjust(raw_p, method = "BH")

# Verified merge cardinality, drop = FALSE helper, and audited missingness
school_meta <- data.frame(school = factor(1:10), urban = rep(c(0, 1), 5))
merged_df <- merge(df, school_meta, by = "school")
stopifnot(nrow(merged_df) == nrow(df))
extract_col <- function(mat, j) mat[, j, drop = FALSE]
n_miss <- sum(is.na(merged_df$y))
avg_y <- mean(merged_df$y, na.rm = TRUE)
