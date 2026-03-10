# ===========================================================================
# Tests for temporal expanding/sliding window CV
# ===========================================================================

test_that("temporal_expanding generates forward-chaining folds", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    value = cumsum(rnorm(n))
  )

  cv <- borg_cv(data, time = "date", target = "value",
                strategy = "temporal_expanding", v = 5)

  expect_s3_class(cv, "borg_cv")
  expect_true(length(cv$folds) >= 2)

  # Each fold's training set should be larger than the previous
  train_sizes <- vapply(cv$folds, function(f) length(f$train), integer(1))
  for (i in seq_along(train_sizes)[-1]) {
    expect_gte(train_sizes[i], train_sizes[i - 1])
  }

  # No overlap between train and test within any fold
  for (fold in cv$folds) {
    expect_equal(length(intersect(fold$train, fold$test)), 0)
  }
})


test_that("temporal_sliding generates fixed-window folds", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    value = cumsum(rnorm(n))
  )

  cv <- borg_cv(data, time = "date", target = "value",
                strategy = "temporal_sliding", v = 5)

  expect_s3_class(cv, "borg_cv")
  expect_true(length(cv$folds) >= 2)

  # No overlap between train and test within any fold
  for (fold in cv$folds) {
    expect_equal(length(intersect(fold$train, fold$test)), 0)
  }
})


test_that("temporal_expanding respects embargo", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    value = cumsum(rnorm(n))
  )

  # Use a large embargo
  cv <- borg_cv(data, time = "date", target = "value",
                strategy = "temporal_expanding", v = 5, embargo = 30)

  # Train times should be at least 30 days before test times
  for (fold in cv$folds) {
    if (length(fold$train) > 0 && length(fold$test) > 0) {
      max_train_date <- max(data$date[fold$train])
      min_test_date <- min(data$date[fold$test])
      gap <- as.numeric(min_test_date - max_train_date)
      expect_gte(gap, 30)
    }
  }
})


test_that("strategy parameter overrides diagnosis recommendation", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    value = cumsum(rnorm(n))
  )

  # Force expanding even if diagnosis suggests temporal_block
  cv_expand <- borg_cv(data, time = "date", target = "value",
                        strategy = "temporal_expanding", v = 3)
  cv_slide <- borg_cv(data, time = "date", target = "value",
                       strategy = "temporal_sliding", v = 3)

  expect_true(length(cv_expand$folds) >= 2)
  expect_true(length(cv_slide$folds) >= 2)
})


test_that("invalid strategy name errors", {
  data <- data.frame(x = 1:100, y = rnorm(100))
  expect_error(
    borg_cv(data, strategy = "nonexistent_strategy"),
    "Unknown strategy"
  )
})
