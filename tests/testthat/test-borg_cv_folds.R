# ===========================================================================
# Tests for borg_cv() fold generators and edge cases
# ===========================================================================

# --- Spatial block folds ---

test_that("spatial_block generates non-overlapping folds", {
  set.seed(42)
  data <- data.frame(
    x = runif(200, 0, 100),
    y = runif(200, 0, 100),
    response = rnorm(200)
  )

  cv <- borg_cv(data, coords = c("x", "y"), target = "response",
                v = 5, verbose = FALSE)

  # Every observation should appear in exactly one test fold
  all_test <- unlist(lapply(cv$folds, function(f) f$test))
  expect_equal(length(all_test), length(unique(all_test)))

  # No overlap within folds
 for (fold in cv$folds) {
    expect_equal(length(intersect(fold$train, fold$test)), 0)
  }
})


test_that("group_fold keeps groups intact", {
  set.seed(42)
  # Strong clustering so diagnosis recommends group_fold
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 5), each = 10) + rnorm(200, sd = 0.1)
  )

  cv <- borg_cv(data, groups = "site", target = "value", v = 5)

  for (fold in cv$folds) {
    train_groups <- unique(data$site[fold$train])
    test_groups <- unique(data$site[fold$test])
    expect_equal(length(intersect(train_groups, test_groups)), 0)
  }
})


test_that("group_fold warns when fewer groups than folds", {
  set.seed(42)
  # Strong clustering so diagnosis selects group_fold
  data <- data.frame(
    group = rep(1:3, each = 10),
    value = rep(rnorm(3, sd = 10), each = 10) + rnorm(30, sd = 0.1)
  )

  expect_warning(
    borg_cv(data, groups = "group", target = "value", v = 5),
    "Only 3 groups"
  )
})


test_that("temporal_block respects time ordering", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    value = cumsum(rnorm(n))
  )

  cv <- borg_cv(data, time = "date", target = "value", v = 5)

  for (fold in cv$folds) {
    expect_equal(length(intersect(fold$train, fold$test)), 0)
  }
})


test_that("borg_cv errors on insufficient data", {
  data <- data.frame(x = 1:5, y = rnorm(5))
  expect_error(
    borg_cv(data, v = 5),
    "Insufficient data"
  )
})


test_that("borg_cv errors on non-data.frame", {
  expect_error(borg_cv("not a df"), "'data' must be a data frame")
})


# --- Output format converters ---

test_that("convert_to_rsample produces valid rset", {
  skip_if_not_installed("rsample")

  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rnorm(200)
  )

  cv <- borg_cv(data, groups = "site", target = "value",
                v = 5, output = "rsample")

  expect_s3_class(cv, "rset")
  expect_equal(nrow(cv), 5)
  expect_true("splits" %in% names(cv))
})


test_that("convert_to_caret produces valid trainControl", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rnorm(200)
  )

  cv <- borg_cv(data, groups = "site", target = "value",
                v = 5, output = "caret")

  expect_true(is.list(cv))
  expect_true("index" %in% names(cv))
  expect_true("indexOut" %in% names(cv))
  expect_equal(length(cv$index), 5)
})


# --- allow_random override ---

test_that("allow_random warns but proceeds", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 5), each = 10) + rnorm(200, sd = 0.5)
  )

  expect_warning(
    cv <- borg_cv(data, groups = "site", target = "value",
                  v = 5, allow_random = TRUE),
    "Random CV requested despite"
  )
  expect_s3_class(cv, "borg_cv")
})


# --- print method ---

test_that("print.borg_cv works", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rnorm(200)
  )

  cv <- borg_cv(data, groups = "site", target = "value", v = 5)
  expect_output(print(cv), "BORG Cross-Validation")
  expect_output(print(cv), "Strategy")
})
