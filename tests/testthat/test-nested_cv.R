# ===========================================================================
# Tests for nested CV leak detection
# ===========================================================================

test_that("nested CV detection catches leaking inner folds", {
  # Create a fake caret train object with inner folds that include test indices
  fake_train <- list(
    control = list(
      method = "cv",
      number = 5,
      repeats = 1,
      index = list(
        Fold1 = 1:80,   # Contains test indices 71:80
        Fold2 = c(1:60, 81:100),
        Fold3 = c(1:40, 61:100),
        Fold4 = c(1:20, 41:100),
        Fold5 = 21:100   # Contains test indices 71:100
      )
    ),
    trainingData = data.frame(x = 1:100, y = rnorm(100))
  )
  class(fake_train) <- "train"

  # Outer test set is 71:100
  risks <- .inspect_nested_cv_caret(fake_train,
                                     train_idx = 1:70,
                                     test_idx = 71:100)

  expect_true(length(risks) > 0)
  risk_types <- vapply(risks, function(r) r$type, character(1))
  expect_true("nested_cv_leak" %in% risk_types)
})


test_that("nested CV detection passes clean inner folds", {
  # Inner folds only use training indices
  fake_train <- list(
    control = list(
      method = "cv",
      number = 5,
      repeats = 1,
      index = list(
        Fold1 = 1:56,
        Fold2 = c(1:42, 57:70),
        Fold3 = c(1:28, 43:70),
        Fold4 = c(1:14, 29:70),
        Fold5 = 15:70
      )
    ),
    trainingData = data.frame(x = 1:70, y = rnorm(70))
  )
  class(fake_train) <- "train"

  risks <- .inspect_nested_cv_caret(fake_train,
                                     train_idx = 1:70,
                                     test_idx = 71:100)

  # No nested CV leak
  nested_risks <- Filter(function(r) r$type == "nested_cv_leak", risks)
  expect_equal(length(nested_risks), 0)
})


test_that("nested CV detects training data scope mismatch", {
  # caret train() called on 100 rows but outer train is only 70
  fake_train <- list(
    control = list(
      method = "cv",
      number = 5,
      repeats = 1,
      index = list(Fold1 = 1:80)  # Already includes test
    ),
    trainingData = data.frame(x = 1:100, y = rnorm(100))  # 100 rows
  )
  class(fake_train) <- "train"

  risks <- .inspect_nested_cv_caret(fake_train,
                                     train_idx = 1:70,
                                     test_idx = 71:100)

  # Should detect both inner fold leak AND training data scope
  expect_true(length(risks) >= 1)
  risk_types <- vapply(risks, function(r) r$type, character(1))
  expect_true("nested_cv_leak" %in% risk_types)
})


test_that("nested CV integrates with borg_validate", {
  # Create a workflow with a leaky train object
  fake_train <- list(
    control = list(
      method = "cv",
      number = 3,
      repeats = 1,
      index = list(
        Fold1 = 1:80,
        Fold2 = c(1:40, 61:100),
        Fold3 = c(21:100)
      )
    ),
    trainingData = data.frame(x = 1:100, y = rnorm(100))
  )
  class(fake_train) <- "train"

  workflow <- list(
    data = data.frame(x = 1:100, y = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    model = fake_train
  )

  result <- borg_validate(workflow)

  # Should find nested CV leak among all risks
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("nested_cv_leak" %in% risk_types)
})
