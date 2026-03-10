# ===========================================================================
# Tests for inspect_preprocessing.R: recipe, prcomp edge cases
# ===========================================================================

# --- prcomp inspection ---

test_that("inspect_prcomp detects leaky PCA", {
  set.seed(42)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))

  # PCA on full data (leaky)
  pca_bad <- prcomp(data, center = TRUE, scale. = TRUE)

  result <- borg_inspect(pca_bad, train_idx = 1:70, test_idx = 71:100,
                          data = data)
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("preprocessing_leak" %in% risk_types)
})


test_that("inspect_prcomp passes clean PCA", {
  set.seed(42)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))

  # PCA on train only (clean)
  pca_good <- prcomp(data[1:70, ], center = TRUE, scale. = TRUE)

  result <- borg_inspect(pca_good, train_idx = 1:70, test_idx = 71:100,
                          data = data)
  preprocess_risks <- Filter(function(r) r$type == "preprocessing_leak",
                              result@risks)
  expect_equal(length(preprocess_risks), 0)
})


test_that("inspect_prcomp handles PCA without scaling", {
  set.seed(42)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))

  pca <- prcomp(data, center = TRUE, scale. = FALSE)

  expect_no_error(
    borg_inspect(pca, train_idx = 1:70, test_idx = 71:100, data = data)
  )
})


test_that("inspect_prcomp handles PCA without centering", {
  set.seed(42)
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))

  pca <- prcomp(data, center = FALSE, scale. = FALSE)

  expect_no_error(
    borg_inspect(pca, train_idx = 1:70, test_idx = 71:100, data = data)
  )
})


test_that("inspect_prcomp returns empty with NULL data", {
  set.seed(42)
  pca <- prcomp(data.frame(x1 = rnorm(50), x2 = rnorm(50)))

  result <- borg_inspect(pca, train_idx = 1:30, test_idx = 31:50,
                          data = NULL)
  expect_s4_class(result, "BorgRisk")
})


# --- caret preProcess inspection ---

test_that("inspect_preProcess detects leaky centering", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100, mean = 10, sd = 5),
    x2 = rnorm(100, mean = 50, sd = 20)
  )

  # preProcess on full data (leaky)
  pp <- caret::preProcess(data, method = c("center", "scale"))

  result <- borg_inspect(pp, train_idx = 1:70, test_idx = 71:100,
                          data = data)
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("preprocessing_leak" %in% risk_types)
})


test_that("inspect_preProcess passes train-only preprocessing", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100, mean = 10, sd = 5),
    x2 = rnorm(100, mean = 50, sd = 20)
  )

  # preProcess on train only (clean)
  pp <- caret::preProcess(data[1:70, ], method = c("center", "scale"))

  result <- borg_inspect(pp, train_idx = 1:70, test_idx = 71:100,
                          data = data)
  preprocess_risks <- Filter(function(r) r$type == "preprocessing_leak",
                              result@risks)
  expect_equal(length(preprocess_risks), 0)
})


test_that("inspect_preProcess handles NULL data", {
  skip_if_not_installed("caret")

  pp <- caret::preProcess(data.frame(x = rnorm(50)), method = "center")

  result <- borg_inspect(pp, train_idx = 1:30, test_idx = 31:50,
                          data = NULL)
  expect_s4_class(result, "BorgRisk")
})


test_that("inspect_preProcess handles empty method", {
  skip_if_not_installed("caret")

  # preProcess with no methods
  pp <- caret::preProcess(data.frame(x = rnorm(50)))

  expect_no_error(
    borg_inspect(pp, train_idx = 1:30, test_idx = 31:50,
                 data = data.frame(x = rnorm(50)))
  )
})


# --- Recipe inspection ---

test_that("inspect_recipe detects leaky recipe (row count)", {
  skip_if_not_installed("recipes")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))

  # Prep on full data (leaky - 100 rows instead of 70)
  rec <- recipes::recipe(y ~ ., data = data) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data)

  result <- borg_inspect(rec, train_idx = 1:70, test_idx = 71:100,
                          data = data)
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("preprocessing_leak" %in% risk_types)
})


test_that("inspect_recipe passes clean recipe", {
  skip_if_not_installed("recipes")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))

  # Prep on train only (clean)
  rec <- recipes::recipe(y ~ ., data = data[1:70, ]) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data[1:70, ])

  result <- borg_inspect(rec, train_idx = 1:70, test_idx = 71:100,
                          data = data)
  # Row count matches, so no row-count-based leak detected
  row_risks <- Filter(function(r) grepl("prepped on", r$description),
                       result@risks)
  expect_equal(length(row_risks), 0)
})


test_that("inspect_recipe handles unprepped recipe", {
  skip_if_not_installed("recipes")

  data <- data.frame(y = rnorm(50), x = rnorm(50))
  rec <- recipes::recipe(y ~ ., data = data) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  # Unprepped - should return no risks
  result <- borg_inspect(rec, train_idx = 1:30, test_idx = 31:50,
                          data = data)
  expect_s4_class(result, "BorgRisk")
})
