# ===========================================================================
# Tests for DI normalization in borg_aoa()
# ===========================================================================

make_aoa_data <- function(seed = 42) {
  set.seed(seed)
  train <- data.frame(x = runif(80, 0, 50), y = runif(80, 0, 50),
                      a = rnorm(80), b = rnorm(80))
  pred <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
                     a = rnorm(200, mean = 1), b = rnorm(200))
  list(train = train, pred = pred)
}


test_that("normalize is off by default", {
  d <- make_aoa_data()
  aoa <- borg_aoa(d$train, d$pred, predictors = c("a", "b"))
  expect_false("di_norm" %in% names(aoa))
})


test_that("normalize adds di_norm scaled by the threshold", {
  d <- make_aoa_data()
  aoa <- borg_aoa(d$train, d$pred, predictors = c("a", "b"), normalize = TRUE)
  thr <- attr(aoa, "threshold")

  expect_true("di_norm" %in% names(aoa))
  expect_equal(aoa$di_norm, aoa$di / thr)
})


test_that("normalized DI boundary sits at 1 and agrees with the AOA flag", {
  d <- make_aoa_data()
  aoa <- borg_aoa(d$train, d$pred, predictors = c("a", "b"), normalize = TRUE)

  # di_norm <= 1 is exactly the applicability condition di <= threshold
  expect_equal(as.logical(aoa$aoa), aoa$di_norm <= 1)
})
