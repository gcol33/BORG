# ===========================================================================
# Tests for visualization functions
# ===========================================================================

test_that("plot_split works with basic indices", {
  train_idx <- 1:70
  test_idx <- 71:100

  # Should not error
  expect_silent(plot_split(train_idx, test_idx))
})

test_that("plot_split handles overlap", {
  train_idx <- 1:60
  test_idx <- 51:100  # Overlap at 51-60

  expect_silent(plot_split(train_idx, test_idx))
})

test_that("plot_split works with temporal data", {
  train_idx <- 1:70
  test_idx <- 71:100
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)

  expect_silent(plot_split(train_idx, test_idx, temporal = dates))
})

test_that("plot_split works with groups", {
  train_idx <- 1:50
  test_idx <- 51:100
  groups <- rep(c("A", "B", "C", "D"), each = 25)

  expect_silent(plot_split(train_idx, test_idx, groups = groups))
})

test_that("plot_risk works with BorgRisk object", {
  data <- data.frame(x = 1:100, y = 101:200)

  # With violations
  result <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
  expect_silent(plot_risk(result))

  # Without violations
  result_clean <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100)
  expect_silent(plot_risk(result_clean))
})

test_that("plot_risk validates input", {
  expect_error(plot_risk("not a BorgRisk"), "'risk' must be a BorgRisk object")
})

test_that("plot_temporal works correctly", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  train_idx <- 1:70
  test_idx <- 71:100

  expect_silent(plot_temporal(dates, train_idx, test_idx))
})

test_that("plot_temporal detects look-ahead", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  train_idx <- 50:100  # Train is later
  test_idx <- 1:49     # Test is earlier (look-ahead!)

  expect_silent(plot_temporal(dates, train_idx, test_idx))
})

test_that("plot_spatial works correctly", {
  set.seed(42)
  x <- runif(100, -10, 10)
  y <- runif(100, -10, 10)
  train_idx <- which(x < 0)
  test_idx <- which(x >= 0)

  expect_silent(plot_spatial(x, y, train_idx, test_idx))
})

test_that("plot_spatial handles overlap", {
  set.seed(42)
  x <- runif(100, -10, 10)
  y <- runif(100, -10, 10)
  train_idx <- 1:60
  test_idx <- 41:100  # Overlap

  expect_silent(plot_spatial(x, y, train_idx, test_idx))
})

test_that("plot_groups works with clean split", {
  groups <- rep(paste0("Patient_", 1:10), each = 10)
  train_idx <- which(groups %in% paste0("Patient_", 1:7))
  test_idx <- which(groups %in% paste0("Patient_", 8:10))

  expect_silent(plot_groups(groups, train_idx, test_idx))
})

test_that("plot_groups detects group leakage", {
  groups <- rep(paste0("Patient_", 1:10), each = 10)
  train_idx <- 1:70
  test_idx <- 61:100  # Overlaps with Patient_7

  expect_silent(plot_groups(groups, train_idx, test_idx))
})

test_that("plot_groups handles many groups", {
  groups <- rep(paste0("G", 1:50), each = 2)  # 50 groups
  train_idx <- 1:60
  test_idx <- 61:100

  # Should truncate to max_groups
  expect_silent(plot_groups(groups, train_idx, test_idx, max_groups = 15))
})
