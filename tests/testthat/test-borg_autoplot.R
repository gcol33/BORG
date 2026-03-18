# Tests for autoplot() methods (R/borg_autoplot.R)

test_that("autoplot.BorgRisk returns ggplot for valid result", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(x = 1:100, y = 101:200)
  result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100)
  p <- ggplot2::autoplot(result)

  expect_s3_class(p, "gg")
})

test_that("autoplot.BorgRisk returns ggplot for risky result", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(x = 1:100, y = 101:200)
  result <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
  p <- ggplot2::autoplot(result)

  expect_s3_class(p, "gg")
})

test_that("autoplot.borg_result returns ggplot for split type", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  d <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20), each = 10) + rnorm(200, sd = 0.5)
  )
  result <- borg(d, groups = "site", target = "value")
  p <- ggplot2::autoplot(result)

  expect_s3_class(p, "gg")
})

test_that("autoplot.borg_result spatial works with data.frame", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  result <- borg(d, coords = c("x", "y"), target = "z")
  p <- ggplot2::autoplot(result, type = "spatial", data = d,
                          coords = c("x", "y"))

  expect_s3_class(p, "gg")
})

test_that("autoplot.borg_cv returns ggplot for sizes", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  cv <- borg_cv(d, coords = c("x", "y"), target = "z")
  p <- ggplot2::autoplot(cv, type = "sizes")

  expect_s3_class(p, "gg")
})

test_that("autoplot.borg_cv returns ggplot for folds tile", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  cv <- borg_cv(d, coords = c("x", "y"), target = "z")
  p <- ggplot2::autoplot(cv, type = "folds")

  expect_s3_class(p, "gg")
})

test_that("autoplot.borg_cv spatial works with coords", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  cv <- borg_cv(d, coords = c("x", "y"), target = "z")
  p <- ggplot2::autoplot(cv, type = "spatial", data = d,
                          coords = c("x", "y"))

  expect_s3_class(p, "gg")
})

test_that("autoplot.BorgDiagnosis returns ggplot", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  d <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
  )
  diag <- borg_diagnose(d, groups = "site", target = "value")
  p <- ggplot2::autoplot(diag)

  expect_s3_class(p, "gg")
})
