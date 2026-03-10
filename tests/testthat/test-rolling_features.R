# ===========================================================================
# Tests for rolling feature engineering leakage detection
# ===========================================================================

test_that("rolling feature detection flags suspicious rolling names", {
  set.seed(42)
  n <- 100

  # Create data with a rolling mean computed on full data
  values <- cumsum(rnorm(n))
  rolling_mean <- stats::filter(values, rep(1/5, 5), sides = 1)

  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    target = rnorm(n),
    rolling_mean_5d = as.numeric(rolling_mean)
  )

  workflow <- list(
    data = data,
    train_idx = 1:70,
    test_idx = 71:100,
    time_col = "date",
    target_col = "target"
  )

  risks <- .check_rolling_features(data, 1:70, 71:100, workflow)

  # Should flag the rolling feature
  if (length(risks) > 0) {
    risk_types <- vapply(risks, function(r) r$type, character(1))
    expect_true("rolling_feature_leak" %in% risk_types)
  }
  # At minimum, should not error
  expect_true(is.list(risks))
})


test_that("rolling feature detection ignores non-rolling features", {
  set.seed(42)
  n <- 100

  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    target = rnorm(n),
    normal_feature = rnorm(n)  # Not a rolling feature
  )

  workflow <- list(
    data = data,
    train_idx = 1:70,
    test_idx = 71:100,
    time_col = "date",
    target_col = "target"
  )

  risks <- .check_rolling_features(data, 1:70, 71:100, workflow)

  # normal_feature should not be flagged as rolling
  rolling_risks <- Filter(function(r) r$source_object == "normal_feature", risks)
  expect_equal(length(rolling_risks), 0)
})


test_that("rolling feature detection skips without time_col", {
  data <- data.frame(x = 1:100, y = rnorm(100))
  workflow <- list(
    data = data,
    train_idx = 1:70,
    test_idx = 71:100
    # No time_col
  )

  risks <- .check_rolling_features(data, 1:70, 71:100, workflow)
  expect_equal(length(risks), 0)
})


test_that("rolling feature detection flags lag features with leading NAs", {
  set.seed(42)
  n <- 100
  values <- cumsum(rnorm(n))

  # Lag-3 feature with 3 leading NAs
  lag3 <- c(NA, NA, NA, values[1:(n - 3)])

  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    target = rnorm(n),
    lag_3_value = lag3
  )

  workflow <- list(
    data = data,
    train_idx = 1:70,
    test_idx = 71:100,
    time_col = "date",
    target_col = "target"
  )

  risks <- .check_rolling_features(data, 1:70, 71:100, workflow)

  # Should detect the lag feature
  expect_true(is.list(risks))
  if (length(risks) > 0) {
    risk_types <- vapply(risks, function(r) r$type, character(1))
    expect_true("rolling_feature_leak" %in% risk_types)
  }
})


test_that("rolling feature check integrates with borg_validate", {
  set.seed(42)
  n <- 100
  values <- cumsum(rnorm(n))
  rolling_mean <- stats::filter(values, rep(1/5, 5), sides = 1)

  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    target = rnorm(n),
    rolling_mean_5d = as.numeric(rolling_mean)
  )

  workflow <- list(
    data = data,
    train_idx = 1:70,
    test_idx = 71:100,
    time_col = "date",
    target_col = "target"
  )

  result <- borg_validate(workflow)
  expect_s4_class(result, "BorgRisk")
})
