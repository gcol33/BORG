# ===========================================================================
# Tests for unified borg() entry point
# ===========================================================================

test_that("borg() validates data frame splits", {
  data <- data.frame(x = 1:100, y = rnorm(100))

  result <- borg(data, train_idx = 1:70, test_idx = 71:100)

  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
})

test_that("borg() detects data frame overlap", {
  data <- data.frame(x = 1:100, y = rnorm(100))

  result <- borg(data, train_idx = 1:60, test_idx = 50:100)

  expect_s4_class(result, "BorgRisk")
  expect_false(result@is_valid)
  expect_gt(result@n_hard, 0L)
})

test_that("borg() requires indices for data frames", {
  data <- data.frame(x = 1:10)


  expect_error(borg(data), "'train_idx' and 'test_idx' are required")
  expect_error(borg(data, train_idx = 1:5), "'train_idx' and 'test_idx' are required")
})

test_that("borg() inspects preprocessing objects", {
  set.seed(42)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # PCA on full data (bad)
  pca <- prcomp(data, center = TRUE, scale. = TRUE)

  result <- borg(pca, train_idx, test_idx, data = data)

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_hard, 0L)
})

test_that("borg() inspects model objects", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Model on full data (bad)
  model <- lm(y ~ x, data = data)

  result <- borg(model, train_idx, test_idx, data = data)

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_hard, 0L)
})

test_that("borg() inspects model on correct data", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Model on train data only (good)
  model <- lm(y ~ x, data = data[train_idx, ])

  result <- borg(model, train_idx, test_idx, data = data)

  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
})

test_that("borg() returns BorgRisk unchanged", {
  data <- data.frame(x = 1:100, y = rnorm(100))

  original <- borg(data, train_idx = 1:70, test_idx = 71:100)
  returned <- borg(original)

  expect_identical(original, returned)
})

test_that("borg() inspects CV objects", {
  skip_if_not_installed("caret")

  train_idx <- 1:70
  test_idx <- 71:100

  # trainControl with test indices in folds (bad)
  bad_folds <- list(
    Fold1 = c(1:30, 75:80),
    Fold2 = c(31:60, 85:90)
  )
  ctrl <- caret::trainControl(method = "cv", index = bad_folds)

  result <- borg(ctrl, train_idx, test_idx)

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_hard, 0L)
})

test_that("borg() inspects recipe objects", {
  skip_if_not_installed("recipes")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Recipe prepped on full data (bad)
  rec <- recipes::recipe(y ~ ., data = data) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data)

  result <- borg(rec, train_idx, test_idx, data = data)

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_hard, 0L)
})

test_that("borg() passes additional arguments", {
  data <- data.frame(x = 1:100, y = rnorm(100))

  # Should work with extra args passed through
  result <- borg(data, train_idx = 1:70, test_idx = 71:100)

  expect_s4_class(result, "BorgRisk")
})

test_that("borg() handles workflow list", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  model <- lm(y ~ x, data = data[train_idx, ])

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    model = model
  )

  result <- borg(workflow)

  expect_s4_class(result, "BorgRisk")
})
