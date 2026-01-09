# ===========================================================================
# Tests for plot() S3 methods
# ===========================================================================

test_that("plot.BorgRisk works with no risks", {
  data <- data.frame(x = 1:100, y = rnorm(100))
  result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100)

  expect_silent(plot(result))
})

test_that("plot.BorgRisk works with violations", {
  data <- data.frame(x = 1:100, y = rnorm(100))
  result <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)

  expect_silent(plot(result))
})

test_that("plot.borg_result works", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100),
    response = rnorm(100)
  )

  result <- borg(data, coords = c("x", "y"), target = "response", v = 3)

  # Default plot shows fold split
  if (!is.null(result$folds) && length(result$folds) > 0) {
    expect_silent(plot(result))
  }
})

test_that("plot.borg_result with type='split' works", {
  set.seed(42)
  data <- data.frame(
    x = runif(100, 0, 100),
    y = runif(100, 0, 100),
    response = rnorm(100)
  )

  result <- borg(data, coords = c("x", "y"), target = "response", v = 3)

  # Only test if folds exist
  if (!is.null(result$folds) && length(result$folds) > 0) {
    expect_silent(plot(result, type = "split"))
  }
})

test_that("plot.borg_comparison works", {
  skip_if_not(capabilities("png"))

  set.seed(42)
  data <- data.frame(
    x = runif(50, 0, 100),
    y = runif(50, 0, 100),
    response = rnorm(50)
  )

  comparison <- borg_compare_cv(
    data = data,
    formula = response ~ x + y,
    coords = c("x", "y"),
    v = 3,
    repeats = 2,
    verbose = FALSE
  )

  expect_silent(plot(comparison, type = "boxplot"))
  expect_silent(plot(comparison, type = "density"))
  expect_silent(plot(comparison, type = "paired"))
})
