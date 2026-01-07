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
