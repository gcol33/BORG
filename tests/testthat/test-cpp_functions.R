# ===========================================================================
# Tests for C++ functions
# ===========================================================================

# ---------------------------------------------------------------------------
# checkTemporalOrder
# ---------------------------------------------------------------------------

test_that("checkTemporalOrder detects look-ahead bias", {
  # Timestamps: 1-10 for rows
  timestamps <- as.numeric(1:10)
  train_idx <- 1:5   # timestamps 1-5

test_idx <- 6:10   # timestamps 6-10

  # No violations - test is after train
  result <- checkTemporalOrder(timestamps, train_idx, test_idx)
  expect_false(result$has_violations)
  expect_equal(result$n_violations, 0)
  expect_equal(result$max_train_time, 5)
})

test_that("checkTemporalOrder finds test data before train data", {
  timestamps <- as.numeric(1:10)
  train_idx <- 5:10  # timestamps 5-10
  test_idx <- 1:4    # timestamps 1-4 (BEFORE train!)

  result <- checkTemporalOrder(timestamps, train_idx, test_idx)
  expect_true(result$has_violations)
  expect_equal(result$n_violations, 4)
  expect_equal(sort(result$violation_indices), 1:4)
  expect_equal(result$max_train_time, 10)
})

test_that("checkTemporalOrder handles mixed temporal violations", {
  timestamps <- as.numeric(c(1, 5, 2, 8, 3, 9, 4, 10, 6, 7))
  train_idx <- c(2, 4, 6, 8)  # timestamps 5, 8, 9, 10
  test_idx <- c(1, 3, 5, 7, 9, 10)  # timestamps 1, 2, 3, 4, 6, 7

  result <- checkTemporalOrder(timestamps, train_idx, test_idx)
  expect_true(result$has_violations)
  # All test timestamps < max_train_time (10)
  expect_equal(result$n_violations, 6)
  expect_equal(result$max_train_time, 10)
})

test_that("checkTemporalOrder handles NA timestamps", {
  timestamps <- as.numeric(c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10))
  train_idx <- 1:5
  test_idx <- 6:10

  result <- checkTemporalOrder(timestamps, train_idx, test_idx)
  # max_train_time should be 5 (ignoring NA at position 3)
  expect_equal(result$max_train_time, 5)
  # Violations: positions 7,8,9,10 have times 7,8,9,10 > 5, but position 6 is NA
  # So no violations since test times are after train
  expect_false(result$has_violations)
})

test_that("checkTemporalOrder with partial look-ahead", {
  timestamps <- as.numeric(1:10)
  train_idx <- 3:7   # timestamps 3-7
  test_idx <- c(1, 2, 8, 9, 10)  # 1,2 are before, 8,9,10 are after

  result <- checkTemporalOrder(timestamps, train_idx, test_idx)
  expect_true(result$has_violations)
  expect_equal(result$n_violations, 2)  # positions 1 and 2
  expect_equal(sort(result$violation_indices), c(1, 2))
  expect_equal(result$max_train_time, 7)
})


# ---------------------------------------------------------------------------
# checkGroupOverlap
# ---------------------------------------------------------------------------

test_that("checkGroupOverlap detects no overlap when groups are separate", {
  groups <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
  train_idx <- c(1, 2, 3, 4, 5, 6)   # groups 1 and 2
  test_idx <- c(7, 8, 9, 10, 11, 12) # groups 3 and 4

  result <- checkGroupOverlap(groups, train_idx, test_idx)
  expect_false(result$has_overlap)
  expect_equal(result$n_overlapping_groups, 0)
  expect_equal(result$n_affected_rows, 0)
})

test_that("checkGroupOverlap detects group overlap", {
  groups <- c(1, 1, 1, 2, 2, 2, 1, 1, 3, 3)  # Group 1 in both train and test
  train_idx <- 1:6    # groups 1, 1, 1, 2, 2, 2
  test_idx <- 7:10    # groups 1, 1, 3, 3

  result <- checkGroupOverlap(groups, train_idx, test_idx)
  expect_true(result$has_overlap)
  expect_equal(result$n_overlapping_groups, 1)  # group 1
  expect_true(1 %in% result$overlapping_groups)
  expect_equal(result$n_affected_rows, 2)  # rows 7 and 8
  expect_equal(sort(result$affected_indices), c(7, 8))
})

test_that("checkGroupOverlap handles multiple overlapping groups", {
  groups <- c(1, 2, 3, 4, 1, 2, 5, 6)
  train_idx <- 1:4    # groups 1, 2, 3, 4
  test_idx <- 5:8     # groups 1, 2, 5, 6

  result <- checkGroupOverlap(groups, train_idx, test_idx)
  expect_true(result$has_overlap)
  expect_equal(result$n_overlapping_groups, 2)  # groups 1 and 2
  expect_true(all(c(1, 2) %in% result$overlapping_groups))
  expect_equal(result$n_affected_rows, 2)  # rows 5 and 6
})

test_that("checkGroupOverlap handles complete overlap", {
  groups <- rep(1:2, each = 5)  # All same groups
  train_idx <- 1:5   # group 1s
  test_idx <- 6:10   # group 2s... wait, let me fix

  # Actually test complete overlap
  groups <- rep(1, 10)  # All same group
  train_idx <- 1:5
  test_idx <- 6:10

  result <- checkGroupOverlap(groups, train_idx, test_idx)
  expect_true(result$has_overlap)
  expect_equal(result$n_overlapping_groups, 1)
  expect_equal(result$n_affected_rows, 5)
})


# ---------------------------------------------------------------------------
# computeCorrelation
# ---------------------------------------------------------------------------

test_that("computeCorrelation computes Pearson correlation correctly", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)  # Perfect positive correlation

  result <- computeCorrelation(x, y)
  expect_equal(result, 1.0, tolerance = 1e-10)
})

test_that("computeCorrelation handles negative correlation", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(10, 8, 6, 4, 2)  # Perfect negative correlation

  result <- computeCorrelation(x, y)
  expect_equal(result, -1.0, tolerance = 1e-10)
})

test_that("computeCorrelation handles zero correlation", {
  set.seed(42)
  x <- rnorm(1000)
  y <- rnorm(1000)

  result <- computeCorrelation(x, y)
  # Should be close to 0 for independent random variables

  expect_true(abs(result) < 0.1)
})

test_that("computeCorrelation handles NA values", {
  x <- c(1, 2, NA, 4, 5)
  y <- c(2, 4, 6, 8, 10)

  result <- computeCorrelation(x, y)
  # Should compute correlation excluding NA
  expect_true(!is.na(result))
  expect_equal(result, 1.0, tolerance = 1e-10)
})

test_that("computeCorrelation handles NA in both vectors", {
  x <- c(1, NA, 3, 4, 5)
  y <- c(2, 4, NA, 8, 10)

  result <- computeCorrelation(x, y)
  # Should exclude rows where either is NA
  expect_true(!is.na(result))
})

test_that("computeCorrelation returns NA for constant vectors", {
  x <- c(5, 5, 5, 5, 5)  # Zero variance
  y <- c(1, 2, 3, 4, 5)

  result <- computeCorrelation(x, y)
  expect_true(is.na(result))
})

test_that("computeCorrelation returns NA with insufficient data", {
  x <- c(1, NA, NA, NA, NA)
  y <- c(2, NA, NA, NA, NA)

  result <- computeCorrelation(x, y)
  expect_true(is.na(result))  # Only 1 complete case
})

test_that("computeCorrelation matches base R cor", {
  set.seed(123)
  x <- rnorm(100)
  y <- x * 2 + rnorm(100, sd = 0.5)

  cpp_result <- computeCorrelation(x, y)
  r_result <- cor(x, y)

  expect_equal(cpp_result, r_result, tolerance = 1e-10)
})

test_that("computeCorrelation matches cor with NAs", {
  set.seed(456)
  x <- rnorm(100)
  y <- x * 2 + rnorm(100, sd = 0.5)
  x[sample(100, 10)] <- NA
  y[sample(100, 10)] <- NA

  cpp_result <- computeCorrelation(x, y)
  r_result <- cor(x, y, use = "pairwise.complete.obs")

  expect_equal(cpp_result, r_result, tolerance = 1e-10)
})
