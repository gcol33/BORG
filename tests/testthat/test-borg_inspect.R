# ===========================================================================
# Tests for borg_inspect()
# ===========================================================================

test_that("borg_inspect validates index arguments", {
 data <- data.frame(x = 1:10, y = 11:20)

 # Non-integer train_idx
 expect_error(
   borg_inspect(data, train_idx = "a", test_idx = 6:10),
   "'train_idx' must be an integer vector"
 )

 # Non-integer test_idx
 expect_error(
   borg_inspect(data, train_idx = 1:5, test_idx = "b"),
   "'test_idx' must be an integer vector"
 )
})


test_that("borg_inspect detects index overlap", {
 data <- data.frame(x = 1:10, y = 11:20)

 # Overlapping indices
 result <- borg_inspect(data, train_idx = 1:6, test_idx = 5:10)

 expect_s4_class(result, "BorgRisk")
 expect_false(result@is_valid)
 expect_equal(result@n_hard, 1L)

 # First risk should be index overlap
 expect_equal(result@risks[[1]]$type, "index_overlap")
 expect_equal(result@risks[[1]]$severity, "hard_violation")
 expect_equal(result@risks[[1]]$affected_indices, c(5L, 6L))
})


test_that("borg_inspect passes with clean split", {
 data <- data.frame(x = 1:10, y = 11:20)

 result <- borg_inspect(data, train_idx = 1:5, test_idx = 6:10)

 expect_s4_class(result, "BorgRisk")
 expect_true(result@is_valid)
 expect_equal(result@n_hard, 0L)
 expect_equal(result@n_soft, 0L)
})


test_that("borg_inspect detects duplicate rows", {
 # Create data with duplicates
 data <- data.frame(
   x = c(1, 2, 3, 4, 5, 1, 2, 3),  # rows 6-8 duplicate rows 1-3
   y = c(10, 20, 30, 40, 50, 10, 20, 30)
 )

 result <- borg_inspect(data, train_idx = 1:5, test_idx = 6:8)

 expect_s4_class(result, "BorgRisk")
 expect_false(result@is_valid)
 expect_equal(result@n_hard, 1L)

 # Check duplicate detection
 dup_risk <- Filter(function(r) r$type == "duplicate_rows", result@risks)
 expect_length(dup_risk, 1)
 expect_equal(dup_risk[[1]]$severity, "hard_violation")
})


test_that("borg_inspect returns BorgRisk with correct structure", {
 data <- data.frame(x = 1:10, y = 11:20)

 result <- borg_inspect(data, train_idx = 1:5, test_idx = 6:10)

 # Check all slots exist
 expect_true(is.list(result@risks))
 expect_true(is.integer(result@n_hard))
 expect_true(is.integer(result@n_soft))
 expect_true(is.logical(result@is_valid))
 expect_true(is.integer(result@train_indices))
 expect_true(is.integer(result@test_indices))
 expect_s3_class(result@timestamp, "POSIXct")
 expect_true(is.language(result@call))
})


test_that("borg_inspect works without indices for basic objects", {
 # Should not error even without indices
 pp <- scale(matrix(1:20, ncol = 2))

 result <- borg_inspect(pp)

 expect_s4_class(result, "BorgRisk")
})


test_that("borg_inspect handles out-of-bounds indices", {
 data <- data.frame(x = 1:10, y = 11:20)

 result <- borg_inspect(data, train_idx = 1:5, test_idx = 11:15)

 # Should detect invalid indices
 invalid_risk <- Filter(function(r) r$type == "invalid_indices", result@risks)
 expect_length(invalid_risk, 1)
 expect_equal(invalid_risk[[1]]$severity, "hard_violation")
})


# ===========================================================================
# Integration tests for preprocessing leak detection
# ===========================================================================

test_that("borg_inspect detects prcomp leakage", {
  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: PCA on full data
  pca_bad <- prcomp(data, center = TRUE, scale. = TRUE)

  result_bad <- borg_inspect(pca_bad, train_idx, test_idx, data = data)
  expect_false(result_bad@is_valid)
  expect_gt(result_bad@n_hard, 0L)

  # GOOD: PCA on train only
  pca_good <- prcomp(data[train_idx, ], center = TRUE, scale. = TRUE)

  result_good <- borg_inspect(pca_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
  expect_equal(result_good@n_hard, 0L)
})


test_that("borg_inspect detects preprocessing mean/sd leakage", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100, mean = 10, sd = 5),
    x2 = rnorm(100, mean = 50, sd = 20)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: preProcess on full data
  pp_bad <- caret::preProcess(data, method = c("center", "scale"))

  result_bad <- borg_inspect(pp_bad, train_idx, test_idx, data = data)
  expect_false(result_bad@is_valid)
  leak_risk <- Filter(function(r) r$type == "preprocessing_leak", result_bad@risks)
  expect_gt(length(leak_risk), 0)

  # GOOD: preProcess on train only
  pp_good <- caret::preProcess(data[train_idx, ], method = c("center", "scale"))

  result_good <- borg_inspect(pp_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})


test_that("borg_inspect detects trainControl CV leakage", {
  skip_if_not_installed("caret")

  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: CV indices include test data
  bad_folds <- list(
    Fold1 = c(1:30, 75:80),  # includes test indices!
    Fold2 = c(31:60, 85:90),
    Fold3 = c(61:70, 1:20)
  )
  ctrl_bad <- caret::trainControl(method = "cv", index = bad_folds)

  result_bad <- borg_inspect(ctrl_bad, train_idx, test_idx)
  expect_false(result_bad@is_valid)
  cv_leak <- Filter(function(r) r$type == "cv_leak", result_bad@risks)
  expect_gt(length(cv_leak), 0)

  # GOOD: CV indices within train only
  good_folds <- list(
    Fold1 = 1:25,
    Fold2 = 26:50,
    Fold3 = 51:70
  )
  ctrl_good <- caret::trainControl(method = "cv", index = good_folds)

  result_good <- borg_inspect(ctrl_good, train_idx, test_idx)
  expect_true(result_good@is_valid)
})


test_that("borg_inspect handles recipe inspection", {
  skip_if_not_installed("recipes")

  set.seed(42)
  data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100, mean = 10),
    x2 = rnorm(100, mean = 50)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: recipe prepped on full data
  rec_bad <- recipes::recipe(y ~ ., data = data) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data)  # All data!

  result_bad <- borg_inspect(rec_bad, train_idx, test_idx, data = data)
  # Should detect row count mismatch
  expect_gt(result_bad@n_hard, 0L)

  # GOOD: recipe prepped on train only
  rec_good <- recipes::recipe(y ~ ., data = data[train_idx, ]) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data[train_idx, ])

  result_good <- borg_inspect(rec_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})


test_that("borg_inspect handles rsample objects", {
  skip_if_not_installed("rsample")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: vfold_cv on full data
  folds_bad <- rsample::vfold_cv(data, v = 5)

  result_bad <- borg_inspect(folds_bad, train_idx, test_idx)
  # Should detect CV scope issue (created on 100 obs, expected 70)
  scope_risk <- Filter(function(r) r$type == "cv_scope", result_bad@risks)
  expect_gt(length(scope_risk), 0)

  # GOOD: vfold_cv on train only
  folds_good <- rsample::vfold_cv(data[train_idx, ], v = 5)

  result_good <- borg_inspect(folds_good, train_idx, test_idx)
  expect_true(result_good@is_valid)
})


test_that("borg_inspect distinguishes hard and soft violations", {
  data <- data.frame(x = 1:10, y = 11:20)

  # Hard violation: index overlap
  result <- borg_inspect(data, train_idx = 1:6, test_idx = 5:10)
  expect_gt(result@n_hard, 0L)
  expect_equal(result@risks[[1]]$severity, "hard_violation")

  # The classification matters for downstream behavior
  expect_false(result@is_valid)  # Hard violations invalidate
})


# ---------------------------------------------------------------------------
# Additional coverage tests
# ---------------------------------------------------------------------------

test_that("borg_inspect validates missing object argument", {
  expect_error(borg_inspect(), "'object' is required")
})

test_that("borg_inspect validates train_idx type", {
  data <- data.frame(x = 1:10)
  expect_error(
    borg_inspect(data, train_idx = "not numeric", test_idx = 6:10),
    "'train_idx' must be an integer vector"
  )
})

test_that("borg_inspect validates test_idx type", {
  data <- data.frame(x = 1:10)
  expect_error(
    borg_inspect(data, train_idx = 1:5, test_idx = list(1, 2)),
    "'test_idx' must be an integer vector"
  )
})

test_that("borg_inspect handles recipe step_center leakage", {
  skip_if_not_installed("recipes")

  set.seed(42)
  data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100, mean = 50, sd = 10)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: step_center on full data
  rec_bad <- recipes::recipe(y ~ ., data = data) |>
    recipes::step_center(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data)  # Full data

  result_bad <- borg_inspect(rec_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)

  # GOOD: step_center on train only
  rec_good <- recipes::recipe(y ~ ., data = data[train_idx, ]) |>
    recipes::step_center(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data[train_idx, ])

  result_good <- borg_inspect(rec_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})

test_that("borg_inspect handles recipe step_scale leakage", {
  skip_if_not_installed("recipes")

  set.seed(42)
  data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100, mean = 0, sd = 20)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: step_scale on full data
  rec_bad <- recipes::recipe(y ~ ., data = data) |>
    recipes::step_scale(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data)

  result_bad <- borg_inspect(rec_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)

  # GOOD: step_scale on train only
  rec_good <- recipes::recipe(y ~ ., data = data[train_idx, ]) |>
    recipes::step_scale(recipes::all_numeric_predictors()) |>
    recipes::prep(training = data[train_idx, ])

  result_good <- borg_inspect(rec_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})

test_that("borg_inspect handles prcomp leakage detection", {
  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: PCA on full data
  pca_bad <- prcomp(data, center = TRUE, scale. = TRUE)

  result_bad <- borg_inspect(pca_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)
  # PCA leaks are reported as preprocessing_leak with "PCA" in description
  pca_leak <- Filter(function(r) grepl("PCA", r$description), result_bad@risks)
  expect_gt(length(pca_leak), 0)

  # GOOD: PCA on train only
  pca_good <- prcomp(data[train_idx, ], center = TRUE, scale. = TRUE)

  result_good <- borg_inspect(pca_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})

test_that("borg_inspect handles rsplit objects",
{
  skip_if_not_installed("rsample")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Create an rsplit (from initial_split)
  split <- rsample::initial_split(data, prop = 0.7)

  # This is an rsplit object, should be inspectable
  result <- borg_inspect(split, train_idx, test_idx)
  # rsplit inspection should work without error
expect_s4_class(result, "BorgRisk")
})

test_that("borg_inspect handles NULL indices gracefully", {
  data <- data.frame(x = 1:10, y = 11:20)

  # With NULL indices, should still return a BorgRisk
  result <- borg_inspect(data, train_idx = NULL, test_idx = NULL)
  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)  # No violations without indices
})

test_that("borg_inspect handles print.borg_context output", {
  data <- data.frame(x = 1:100, y = 101:200)
  train_idx <- 1:70
  test_idx <- 71:100

  ctx <- borg_guard(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    mode = "warn"
  )

  # Test print method produces output
  output <- capture.output(print(ctx))
  expect_true(length(output) > 0)
  expect_true(any(grepl("BORG", output)))
})

test_that("borg_inspect handles preProcess with multiple methods", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100, mean = 100),
    x2 = rnorm(100, mean = 50)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: preProcess on full data with multiple methods
  pp_bad <- caret::preProcess(
    data[, c("x1", "x2")],
    method = c("center", "scale", "pca")
  )

  result_bad <- borg_inspect(pp_bad, train_idx, test_idx, data = data)
  expect_gt(result_bad@n_hard, 0L)

  # GOOD: preProcess on train only
  pp_good <- caret::preProcess(
    data[train_idx, c("x1", "x2")],
    method = c("center", "scale")
  )

  result_good <- borg_inspect(pp_good, train_idx, test_idx, data = data)
  expect_true(result_good@is_valid)
})
