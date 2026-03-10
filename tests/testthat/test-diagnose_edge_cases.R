# ===========================================================================
# Edge case tests for borg_diagnose() and internal helpers
# ===========================================================================

# --- Spatial diagnosis edge cases ---

test_that("diagnose_spatial handles < 10 complete spatial observations", {
  # 15 rows total (passes n >= 10 check), but only 5 have complete coords
  data <- data.frame(
    x = c(1:5, rep(NA, 10)),
    y = c(1:5, rep(NA, 10)),
    val = rnorm(15)
  )

  result <- borg_diagnose(data, coords = c("x", "y"), target = "val")
  expect_s4_class(result, "BorgDiagnosis")
  # With only 5 complete spatial obs, detection should return not detected
  expect_false(result@spatial$detected)
})


test_that("diagnose_spatial works without target variable", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100)
  )

  result <- borg_diagnose(data, coords = c("x", "y"))
  expect_s4_class(result, "BorgDiagnosis")
})


test_that("diagnose_spatial handles large datasets (sampling)", {
  set.seed(42)
  n <- 3000
  data <- data.frame(
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    val = rnorm(n)
  )

  # Should not error - internally samples down to 2000
  result <- borg_diagnose(data, coords = c("x", "y"), target = "val")
  expect_s4_class(result, "BorgDiagnosis")
})


# --- Temporal diagnosis edge cases ---

test_that("diagnose_temporal handles < 20 observations", {
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 15),
    val = rnorm(15)
  )

  result <- borg_diagnose(data, time = "date", target = "val")
  expect_s4_class(result, "BorgDiagnosis")
  expect_false(result@temporal$detected)
})


test_that("diagnose_temporal works without target (time-only)", {
  set.seed(42)
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  )

  result <- borg_diagnose(data, time = "date")
  expect_s4_class(result, "BorgDiagnosis")
})


test_that("diagnose_temporal handles irregular time spacing", {
  set.seed(42)
  dates <- sort(as.Date("2020-01-01") + sample(1:1000, 100))
  data <- data.frame(
    date = dates,
    val = cumsum(rnorm(100))
  )

  result <- borg_diagnose(data, time = "date", target = "val")
  expect_s4_class(result, "BorgDiagnosis")
})


test_that("diagnose_temporal handles numeric time column", {
  set.seed(42)
  data <- data.frame(
    time_step = 1:100,
    val = cumsum(rnorm(100))
  )

  result <- borg_diagnose(data, time = "time_step", target = "val")
  expect_s4_class(result, "BorgDiagnosis")
})


# --- Clustered diagnosis edge cases ---

test_that("diagnose_clustered handles < 3 clusters", {
  data <- data.frame(
    group = rep(1:2, each = 20),
    val = rnorm(40)
  )

  result <- borg_diagnose(data, groups = "group", target = "val")
  # With only 2 clusters, detection should still work
  expect_s4_class(result, "BorgDiagnosis")
  expect_true(result@clustered$detected)
})


test_that("diagnose_clustered handles one-per-group", {
  data <- data.frame(
    group = 1:100,
    val = rnorm(100)
  )

  result <- borg_diagnose(data, groups = "group", target = "val")
  # Each obs is its own group - no clustering
  expect_false(result@clustered$detected)
  expect_equal(result@clustered$icc, 0)
})


test_that("diagnose_clustered without target uses numeric columns", {
  set.seed(42)
  data <- data.frame(
    group = rep(1:10, each = 10),
    x1 = rep(rnorm(10, sd = 5), each = 10) + rnorm(100, sd = 0.5),
    x2 = rnorm(100)
  )

  result <- borg_diagnose(data, groups = "group")
  expect_s4_class(result, "BorgDiagnosis")
})


# --- Mixed dependency detection ---

test_that("diagnose detects mixed spatial + temporal", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    val = cumsum(rnorm(n))
  )
  # Add spatial autocorrelation
  for (i in 2:n) {
    nearest <- which.min((data$x[1:(i-1)] - data$x[i])^2 +
                         (data$y[1:(i-1)] - data$y[i])^2)
    data$val[i] <- 0.5 * data$val[nearest] + 0.5 * data$val[i]
  }

  result <- borg_diagnose(data, coords = c("x", "y"), time = "date",
                           target = "val")
  expect_s4_class(result, "BorgDiagnosis")
  # Should detect at least one dependency
  expect_true(result@severity != "none" ||
              result@dependency_type != "none")
})


# --- Input validation ---

test_that("borg_diagnose validates inputs", {
  expect_error(borg_diagnose("not a df"), "'data' must be a data frame")
  expect_error(borg_diagnose(data.frame(x = 1:5)), "at least 10 observations")
  expect_error(
    borg_diagnose(data.frame(x = 1:20), coords = c("a", "b")),
    "Coordinate columns not found"
  )
  expect_error(
    borg_diagnose(data.frame(x = 1:20), time = "nonexistent"),
    "Time column.*not found"
  )
  expect_error(
    borg_diagnose(data.frame(x = 1:20), groups = "nonexistent"),
    "Group column.*not found"
  )
})


# --- Inflation estimate ---

test_that("inflation estimate returns NA when no dependency", {
  set.seed(42)
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  result <- borg_diagnose(data, target = "y")
  expect_true(is.na(result@inflation_estimate$auc_inflation))
})


# --- as.data.frame.BorgDiagnosis ---

test_that("as.data.frame.BorgDiagnosis returns one-row data frame", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:5, each = 40),
    val = rep(rnorm(5, sd = 10), each = 40) + rnorm(200, sd = 0.1)
  )

  diag <- borg_diagnose(data, groups = "site", target = "val")
  df <- as.data.frame(diag)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_true(all(c("dependency_type", "severity", "recommended_cv",
                     "n_obs", "clustered_detected", "icc") %in% names(df)))
  expect_equal(df$n_obs, 200L)
  expect_true(df$clustered_detected)
})


# --- summary methods ---

test_that("summary.borg_cv returns strategy and fold info", {
  set.seed(42)
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  cv <- borg_cv(data, target = "y", v = 5)
  out <- capture.output(result <- summary(cv))

  expect_true(any(grepl("Strategy", out)))
  expect_true(any(grepl("Folds", out)))
  expect_true(is.list(result))
  expect_equal(result$n_folds, 5)
})


test_that("summary.borg_power returns key metrics", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:10, each = 20),
    val = rep(rnorm(10, sd = 5), each = 20) + rnorm(200, sd = 0.5)
  )

  pw <- borg_power(data, groups = "site", target = "val")
  out <- capture.output(result <- summary(pw))

  expect_true(any(grepl("N actual", out)))
  expect_true(any(grepl("Design effect", out)))
  expect_true(is.list(result))
  expect_equal(result$n_actual, 200)
})


test_that("inflation estimate is bounded", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:5, each = 40),
    val = rep(rnorm(5, sd = 10), each = 40) + rnorm(200, sd = 0.1)
  )

  result <- borg_diagnose(data, groups = "site", target = "val")
  if (!is.na(result@inflation_estimate$auc_inflation)) {
    expect_lte(result@inflation_estimate$auc_inflation, 0.6)
    expect_gte(result@inflation_estimate$auc_inflation, 0)
  }
})
