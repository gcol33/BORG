# Parameter-recovery and calibration tests for the previously-untested
# statistical/spatial-diagnostic functions: borg_null_test(), borg_local_moran(),
# borg_fold_similarity(), borg_global_validation(), borg_best_subset().
# These simulate data with a known truth and assert the estimator recovers it,
# complementing the shape/plumbing coverage elsewhere.

# Manual k-fold helper (plain fold list; all five functions accept either a
# borg_cv object or a bare list of list(train=, test=) folds).
make_kfold <- function(n, k = 5, seed = 1) {
  set.seed(seed)
  idx <- sample(rep(seq_len(k), length.out = n))
  lapply(seq_len(k), function(i) {
    list(train = which(idx != i), test = which(idx == i))
  })
}

# --- borg_null_test(): calibrated significance under a real vs. null effect
test_that("borg_null_test() detects real signal and stays near-nominal under the null", {
  # Positive control: a strong linear relationship should be flagged
  # significant in the large majority of replicates.
  sig_hits <- vapply(1:8, function(s) {
    set.seed(s)
    n <- 150
    d <- data.frame(x = runif(n), y = runif(n))
    d$z <- 3 * d$x + rnorm(n, sd = 0.3)
    folds <- make_kfold(n, k = 5, seed = s)
    nt <- borg_null_test(d, folds, z ~ x, n_null = 49, seed = s)
    nt$significant
  }, logical(1))
  expect_gte(mean(sig_hits), 0.75)

  # Negative control: z is unrelated to x, so the empirical CV metric should
  # behave like a draw from the null distribution -- the significance rate
  # across seeds should sit near the nominal alpha = 0.05, not far above it.
  null_hits <- vapply(1:20, function(s) {
    set.seed(100 + s)
    n <- 150
    d <- data.frame(x = runif(n), y = runif(n))
    d$z <- rnorm(n)
    folds <- make_kfold(n, k = 5, seed = s)
    nt <- borg_null_test(d, folds, z ~ x, n_null = 49, seed = s)
    nt$significant
  }, logical(1))
  expect_lte(mean(null_hits), 0.3)
})

# --- borg_local_moran(): recovers known hot/cold spatial clusters ----------
test_that("borg_local_moran() recovers known high-high/low-low residual clusters", {
  set.seed(11)
  n <- 150
  x <- runif(n, 0, 10)
  y <- runif(n, 0, 10)
  hot <- x < 3 & y < 3
  cold <- x > 7 & y > 7

  residuals <- ifelse(
    hot, rnorm(n, mean = 5, sd = 0.3),
    ifelse(cold, rnorm(n, mean = -5, sd = 0.3), rnorm(n, mean = 0, sd = 0.3))
  )

  res <- borg_local_moran(residuals, x, y, k = 8)

  expect_s3_class(res, "borg_local_moran")
  # The known hot cluster should mostly be classified high-high, the known
  # cold cluster mostly low-low, and unclustered background points should
  # mostly come back not significant.
  expect_gt(mean(res$cluster_type[hot] == "high-high"), 0.75)
  expect_gt(mean(res$cluster_type[cold] == "low-low"), 0.75)
  expect_gt(mean(res$cluster_type[!hot & !cold] == "not significant"), 0.75)
})

# --- borg_fold_similarity(): flags out-of-range test folds as novel -------
test_that("borg_fold_similarity() recovers known in-range vs. out-of-range folds", {
  set.seed(5)
  n_train <- 200
  a_train <- runif(n_train, 0, 1)
  test_in <- runif(50, 0, 1)     # within the training range
  test_out <- runif(50, 5, 6)    # far outside the training range

  data <- data.frame(a = c(a_train, test_in, test_out))
  folds <- list(
    list(train = seq_len(n_train), test = (n_train + 1):(n_train + 50)),
    list(train = seq_len(n_train), test = (n_train + 51):(n_train + 100))
  )

  res <- borg_fold_similarity(data, folds, predictors = "a")

  expect_s3_class(res, "borg_fold_similarity")
  # In-range fold: mostly not novel, positive mean similarity.
  expect_lt(res$pct_novel[1], 10)
  expect_gt(res$mean_mess[1], 0)
  # Out-of-range fold: entirely novel, strongly negative mean similarity.
  expect_gt(res$pct_novel[2], 90)
  expect_lt(res$mean_mess[2], 0)
})

# --- borg_global_validation(): pooled RMSE recovers the true noise sigma --
test_that("borg_global_validation() pooled RMSE recovers the known noise sigma", {
  sigma <- 2.0
  abs_diffs <- vapply(1:10, function(s) {
    set.seed(s)
    n <- 400
    d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
    d$y <- 3 * d$x1 - 2 * d$x2 + rnorm(n, sd = sigma)
    folds <- make_kfold(n, k = 5, seed = s)
    res <- borg_global_validation(d, folds, y ~ x1 + x2, metric = "rmse")
    abs(res$pooled - sigma)
  }, numeric(1))

  expect_s3_class(
    borg_global_validation(
      data.frame(x1 = rnorm(50), x2 = rnorm(50), y = rnorm(50)),
      make_kfold(50, k = 5, seed = 1), y ~ x1 + x2
    ),
    "borg_global_validation"
  )
  expect_lt(mean(abs_diffs), 0.15)
  expect_true(all(abs_diffs < 0.25))
})

# --- borg_best_subset(): recovers the true predictor set -------------------
test_that("borg_best_subset() recovers the true predictors over pure noise", {
  hit_rate <- mean(vapply(1:10, function(s) {
    set.seed(s)
    n <- 300
    d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
    d$z <- 3 * d$x1 - 2 * d$x2 + rnorm(n, sd = 1)  # x3, x4 are pure noise
    folds <- make_kfold(n, k = 5, seed = s)

    bss <- borg_best_subset(d, target = "z",
                            predictors = c("x1", "x2", "x3", "x4"),
                            folds = folds, metric = "rmse")
    best_vars <- strsplit(bss$variables[1], " \\+ ")[[1]]
    all(c("x1", "x2") %in% best_vars)
  }, logical(1)))

  expect_gte(hit_rate, 0.8)
})
