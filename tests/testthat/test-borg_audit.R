# ===========================================================================
# Tests for audit functions
# ===========================================================================

# ---------------------------------------------------------------------------
# audit_predictions
# ---------------------------------------------------------------------------

test_that("audit_predictions validates prediction count", {
  train_idx <- 1:70
  test_idx <- 71:100

  # Wrong count
  predictions <- rnorm(50)  # Should be 30

  result <- audit_predictions(predictions, train_idx, test_idx)
  expect_false(result@is_valid)
  expect_gt(result@n_hard, 0L)

  mismatch <- Filter(function(r) r$type == "prediction_count_mismatch", result@risks)
  expect_gt(length(mismatch), 0)
})

test_that("audit_predictions accepts correct prediction count", {
  train_idx <- 1:70
  test_idx <- 71:100

  predictions <- rnorm(30)  # Correct count

  result <- audit_predictions(predictions, train_idx, test_idx)
  expect_true(result@is_valid)
})

test_that("audit_predictions detects perfect predictions", {
  train_idx <- 1:70
  test_idx <- 71:100

  actual <- rnorm(30)
  predictions <- actual  # Perfect match!

  result <- audit_predictions(predictions, train_idx, test_idx, actual = actual)
  expect_false(result@is_valid)

  perfect <- Filter(function(r) r$type == "exact_predictions" || r$type == "perfect_prediction",
                    result@risks)
  expect_gt(length(perfect), 0)
})

test_that("audit_predictions detects near-perfect correlation", {
  train_idx <- 1:70
  test_idx <- 71:100

  actual <- rnorm(30)
  predictions <- actual + rnorm(30, sd = 0.0001)  # Near-perfect

  result <- audit_predictions(predictions, train_idx, test_idx, actual = actual)
  expect_false(result@is_valid)
})

test_that("audit_predictions handles classification", {
  train_idx <- 1:70
  test_idx <- 71:100

  actual <- factor(sample(c("A", "B"), 30, replace = TRUE))
  predictions <- actual  # Perfect classification

  result <- audit_predictions(predictions, train_idx, test_idx, actual = actual)
  # Should flag as suspicious
  expect_gt(result@n_soft + result@n_hard, 0L)
})

test_that("audit_predictions checks model leakage", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD model: trained on full data
  model_bad <- lm(y ~ x, data = data)
  predictions <- predict(model_bad, newdata = data[test_idx, ])

  result <- audit_predictions(predictions, train_idx, test_idx, model = model_bad, data = data)
  expect_false(result@is_valid)
})

test_that("audit_predictions detects wrong prediction set", {
  train_idx <- 1:70
  test_idx <- 71:100

  # Predictions match training size, not test size
  predictions <- rnorm(70)

  result <- audit_predictions(predictions, train_idx, test_idx,
                               data = data.frame(x = 1:100))
  expect_false(result@is_valid)

  wrong_set <- Filter(function(r) r$type == "wrong_prediction_set", result@risks)
  expect_gt(length(wrong_set), 0)
})

test_that("audit_predictions detects full data prediction", {
  train_idx <- 1:70
  test_idx <- 71:100

  # Predictions on full data
  predictions <- rnorm(100)

  result <- audit_predictions(predictions, train_idx, test_idx,
                               data = data.frame(x = 1:100))
  expect_false(result@is_valid)

  full_data <- Filter(function(r) r$type == "full_data_prediction", result@risks)
  expect_gt(length(full_data), 0)
})

test_that("audit_predictions requires non-empty predictions", {
  expect_error(audit_predictions(NULL, 1:70, 71:100))
  expect_error(audit_predictions(numeric(0), 1:70, 71:100))
})


# ---------------------------------------------------------------------------
# cv_leakage_report
# ---------------------------------------------------------------------------

test_that("cv_leakage_report works with rsample vfold_cv", {
  skip_if_not_installed("rsample")

  set.seed(42)
  data <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: CV on full data
  folds_bad <- rsample::vfold_cv(data, v = 5)

  report <- cv_leakage_report(folds_bad, train_idx, test_idx)

  expect_s3_class(report, "borg_cv_report")
  expect_true(report$summary$has_leakage)
  expect_gt(report$summary$n_issues, 0)
})

test_that("cv_leakage_report works with caret trainControl", {
  skip_if_not_installed("caret")

  train_idx <- 1:70
  test_idx <- 71:100

  # BAD: CV folds include test indices
  bad_folds <- list(
    Fold1 = c(1:25, 75:80),   # Includes test indices!
    Fold2 = c(26:50, 81:85),
    Fold3 = 51:70
  )
  ctrl_bad <- caret::trainControl(method = "cv", index = bad_folds)

  report <- cv_leakage_report(ctrl_bad, train_idx, test_idx)

  expect_s3_class(report, "borg_cv_report")
  expect_true(report$summary$has_leakage)
})

test_that("cv_leakage_report detects clean CV", {
  skip_if_not_installed("caret")

  train_idx <- 1:70
  test_idx <- 71:100

  # GOOD: CV folds within train only
  good_folds <- list(
    Fold1 = 1:25,
    Fold2 = 26:50,
    Fold3 = 51:70
  )
  ctrl_good <- caret::trainControl(method = "cv", index = good_folds)

  report <- cv_leakage_report(ctrl_good, train_idx, test_idx)

  expect_false(report$summary$has_leakage)
  expect_equal(report$summary$n_issues, 0)
})

test_that("cv_leakage_report print method works", {
  skip_if_not_installed("caret")

  train_idx <- 1:70
  test_idx <- 71:100

  good_folds <- list(Fold1 = 1:35, Fold2 = 36:70)
  ctrl <- caret::trainControl(method = "cv", index = good_folds)

  report <- cv_leakage_report(ctrl, train_idx, test_idx)

  output <- capture.output(print(report))
  expect_true(length(output) > 0)
  expect_true(any(grepl("CV Leakage Report", output)))
})


# ---------------------------------------------------------------------------
# audit_importance
# ---------------------------------------------------------------------------

test_that("audit_importance detects importance on full data", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  importance <- c(x1 = 0.6, x2 = 0.4)

  # BAD: computed on full data
  result <- audit_importance(importance, data, train_idx, test_idx)
  expect_false(result@is_valid)

  full_data <- Filter(function(r) r$type == "importance_on_full_data", result@risks)
  expect_gt(length(full_data), 0)
})

test_that("audit_importance accepts importance on training data", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  importance <- c(x1 = 0.6, x2 = 0.4)

  # GOOD: computed on training data only
  result <- audit_importance(importance, data[train_idx, ], train_idx, test_idx)
  expect_true(result@is_valid)
})

test_that("audit_importance detects SHAP on test data", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  importance <- c(x1 = 0.6, x2 = 0.4)

  # BAD: SHAP computed on test data only
  result <- audit_importance(importance, data[test_idx, ], train_idx, test_idx, method = "shap")
  expect_false(result@is_valid)

  shap_test <- Filter(function(r) r$type == "shap_on_test_data", result@risks)
  expect_gt(length(shap_test), 0)
})

test_that("audit_importance detects permutation on test data", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  importance <- c(x1 = 0.6, x2 = 0.4)

  # BAD: permutation importance on test data
  result <- audit_importance(importance, data[test_idx, ], train_idx, test_idx, method = "permutation")
  expect_false(result@is_valid)

  perm_test <- Filter(function(r) r$type == "permutation_on_test_data", result@risks)
  expect_gt(length(perm_test), 0)
})

test_that("audit_importance handles data frames and matrices", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # As data frame
  imp_df <- data.frame(feature = c("x1", "x2"), importance = c(0.6, 0.4))
  result1 <- audit_importance(imp_df, data[train_idx, ], train_idx, test_idx)
  expect_true(result1@is_valid)

  # As matrix
  imp_mat <- matrix(c(0.6, 0.4), ncol = 1)
  result2 <- audit_importance(imp_mat, data[train_idx, ], train_idx, test_idx)
  expect_true(result2@is_valid)
})

test_that("audit_importance checks model leakage", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # BAD model: trained on full data
  model <- lm(y ~ x1 + x2, data = data)
  importance <- c(x1 = 0.6, x2 = 0.4)

  result <- audit_importance(importance, data[train_idx, ], train_idx, test_idx, model = model)
  expect_false(result@is_valid)

  model_leak <- Filter(function(r) r$type == "importance_from_leaked_model", result@risks)
  expect_gt(length(model_leak), 0)
})

test_that("audit_importance detects uniform importance", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Very uniform importance: 1000 identical values and one slightly different
  # This achieves sd/range < 0.01
  importance <- c(rep(0.25, 1000), 0.26)

  result <- audit_importance(importance, data[train_idx, ], train_idx, test_idx)
  # Should flag as soft warning
  uniform <- Filter(function(r) r$type == "uniform_importance", result@risks)
  expect_gt(length(uniform), 0)
})

test_that("audit_importance detects negative importance for gain method", {
  set.seed(42)
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Negative importance (unusual for gain)
  importance <- c(x1 = 0.6, x2 = -0.2)

  result <- audit_importance(importance, data[train_idx, ], train_idx, test_idx, method = "gain")

  negative <- Filter(function(r) r$type == "negative_importance", result@risks)
  expect_gt(length(negative), 0)
})

test_that("audit_importance requires importance and data", {
  expect_error(audit_importance(NULL, data.frame(x = 1:10), 1:7, 8:10))
  expect_error(audit_importance(c(0.5, 0.5), NULL, 1:7, 8:10))
})

test_that("audit_importance detects data size mismatch", {
  set.seed(42)
  # Data size between train and total (not matching either)
  data <- data.frame(y = rnorm(80), x1 = rnorm(80))  # 80 rows
  train_idx <- 1:70
  test_idx <- 71:100  # total would be 100

  importance <- c(x1 = 0.6)

  result <- audit_importance(importance, data, train_idx, test_idx)
  mismatch <- Filter(function(r) r$type == "importance_data_size_mismatch", result@risks)
  expect_gt(length(mismatch), 0)
})

test_that("cv_leakage_report handles issues with many indices", {
  skip_if_not_installed("caret")

  train_idx <- 1:70
  test_idx <- 71:100

  # Create folds with many leaked indices
  bad_folds <- list(
    Fold1 = c(1:20, 71:85),  # 15 test indices leaked
    Fold2 = c(21:40, 86:100)  # 15 more
  )
  ctrl <- caret::trainControl(method = "cv", index = bad_folds)

  report <- cv_leakage_report(ctrl, train_idx, test_idx)

  # Print should truncate long index lists
  output <- capture.output(print(report))
  expect_true(any(grepl("more\\)", output)))
})

test_that("print.borg_cv_report handles overlap in summary", {
  skip_if_not_installed("caret")

  # Create overlapping train/test
  train_idx <- 1:75
  test_idx <- 70:100  # overlaps at 70-75

  good_folds <- list(Fold1 = 1:35, Fold2 = 36:70)
  ctrl <- caret::trainControl(method = "cv", index = good_folds)

  report <- cv_leakage_report(ctrl, train_idx, test_idx)

  output <- capture.output(print(report))
  expect_true(any(grepl("overlap", output, ignore.case = TRUE)))
})
