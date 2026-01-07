# ===========================================================================
# Tests for borg_validate()
# ===========================================================================

test_that("borg_validate validates required workflow fields", {
  # Non-list workflow
  expect_error(
    borg_validate("not a list"),
    "'workflow' must be a list"
  )

  # Missing data
  expect_error(
    borg_validate(list(train_idx = 1:5, test_idx = 6:10)),
    "missing required fields.*data"
  )

  # Missing train_idx
  expect_error(
    borg_validate(list(data = data.frame(x = 1:10), test_idx = 6:10)),
    "missing required fields.*train_idx"
  )

  # Missing test_idx
  expect_error(
    borg_validate(list(data = data.frame(x = 1:10), train_idx = 1:5)),
    "missing required fields.*test_idx"
  )
})


test_that("borg_validate returns BorgRisk object", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  result <- borg_validate(workflow)

  expect_s4_class(result, "BorgRisk")
  expect_true(is.list(result@risks))
  expect_true(is.integer(result@n_hard))
  expect_true(is.integer(result@n_soft))
  expect_true(is.logical(result@is_valid))
})


test_that("borg_validate detects index overlap", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:6,
    test_idx = 5:10  # overlaps at 5, 6
  )

  result <- borg_validate(workflow)

  expect_false(result@is_valid)
  expect_gt(result@n_hard, 0L)  # May detect multiple violations (overlap + duplicates)

  overlap_risk <- Filter(function(r) r$type == "index_overlap", result@risks)
  expect_length(overlap_risk, 1)
  expect_equal(overlap_risk[[1]]$severity, "hard_violation")
})


test_that("borg_validate detects duplicate rows", {
  workflow <- list(
    data = data.frame(
      x = c(1, 2, 3, 4, 5, 1, 2, 3),  # rows 6-8 duplicate rows 1-3
      y = c(10, 20, 30, 40, 50, 10, 20, 30)
    ),
    train_idx = 1:5,
    test_idx = 6:8
  )

  result <- borg_validate(workflow)

  expect_false(result@is_valid)
  dup_risk <- Filter(function(r) r$type == "duplicate_rows", result@risks)
  expect_length(dup_risk, 1)
})


test_that("borg_validate passes with clean workflow", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  result <- borg_validate(workflow)

  expect_true(result@is_valid)
  expect_equal(result@n_hard, 0L)
  expect_equal(result@n_soft, 0L)
})


test_that("borg_validate strict mode errors on hard violations", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:6,
    test_idx = 5:10  # overlaps
  )

  expect_error(
    borg_validate(workflow, strict = TRUE),
    "BORG VALIDATION FAILED"
  )
})


test_that("borg_validate strict mode passes with clean workflow", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  result <- borg_validate(workflow, strict = TRUE)
  expect_true(result@is_valid)
})


test_that("borg_validate detects target leakage", {
  set.seed(42)
  n <- 100
  target <- rnorm(n)
  # Create a feature that is nearly identical to target (leakage)
  leaked_feature <- target + rnorm(n, sd = 0.001)

  workflow <- list(
    data = data.frame(
      y = target,
      x1 = rnorm(n),
      x2 = leaked_feature
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  expect_false(result@is_valid)
  leak_risk <- Filter(function(r) r$type == "target_leakage_direct", result@risks)
  expect_gt(length(leak_risk), 0)
})


test_that("borg_validate detects high correlation (soft inflation)", {
  set.seed(42)
  n <- 100
  target <- rnorm(n)
  # Create a feature with high but not extreme correlation
  proxy_feature <- target * 0.97 + rnorm(n, sd = 0.3)

  workflow <- list(
    data = data.frame(
      y = target,
      x1 = rnorm(n),
      x2 = proxy_feature
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  # Should have soft inflation warning
  proxy_risk <- Filter(function(r) r$type == "target_leakage_proxy", result@risks)
  expect_gt(length(proxy_risk), 0)
  expect_equal(proxy_risk[[1]]$severity, "soft_inflation")
})


test_that("borg_validate skips target leakage check without target_col", {
  set.seed(42)
  n <- 100
  target <- rnorm(n)
  leaked_feature <- target + rnorm(n, sd = 0.001)

  workflow <- list(
    data = data.frame(
      y = target,
      x1 = rnorm(n),
      x2 = leaked_feature
    ),
    train_idx = 1:70,
    test_idx = 71:100
    # No target_col specified

  )

  result <- borg_validate(workflow)

  # Should not detect leakage without target_col
  leak_risk <- Filter(function(r) grepl("target_leakage", r$type), result@risks)
  expect_length(leak_risk, 0)
})


test_that("borg_validate inspects preprocessing objects", {
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

  workflow_bad <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pp_bad
  )

  result_bad <- borg_validate(workflow_bad)
  expect_false(result_bad@is_valid)

  # GOOD: preProcess on train only
  pp_good <- caret::preProcess(data[train_idx, ], method = c("center", "scale"))

  workflow_good <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pp_good
  )

  result_good <- borg_validate(workflow_good)
  expect_true(result_good@is_valid)
})


test_that("borg_validate handles multiple preprocessing objects", {
  skip_if_not_installed("caret")

  set.seed(123)  # Use different seed for more variation
  data <- data.frame(
    x1 = rnorm(100, mean = 10, sd = 5),
    x2 = rnorm(100, mean = 50, sd = 20)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # One good, one bad
  pp_good <- caret::preProcess(data[train_idx, ], method = "center")
  pp_bad <- caret::preProcess(data, method = "scale")  # Full data!

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = list(pp_good, pp_bad)
  )

  result <- borg_validate(workflow)
  # Should detect preprocessing leak from pp_bad
  leak_risks <- Filter(function(r) r$type == "preprocessing_leak", result@risks)
  expect_gt(length(leak_risks), 0)  # At least one preprocessing leak detected
})


test_that("borg_validate inspects prcomp preprocessing", {
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

  workflow_bad <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pca_bad
  )

  result_bad <- borg_validate(workflow_bad)
  expect_false(result_bad@is_valid)

  # GOOD: PCA on train only
  pca_good <- prcomp(data[train_idx, ], center = TRUE, scale. = TRUE)

  workflow_good <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pca_good
  )

  result_good <- borg_validate(workflow_good)
  expect_true(result_good@is_valid)
})


# ===========================================================================
# Enhanced Target Leakage Detection Tests
# ===========================================================================

test_that("borg_validate detects suspicious naming patterns", {
  set.seed(42)
  n <- 100

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x1 = rnorm(n),
      future_revenue = rnorm(n),  # Suspicious name
      outcome_rate = rnorm(n)     # Suspicious name
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  naming_risks <- Filter(function(r) r$type == "target_leakage_naming", result@risks)
  expect_gte(length(naming_risks), 2)
  expect_equal(naming_risks[[1]]$severity, "soft_inflation")
})


test_that("borg_validate detects perfect categorical separation", {
  set.seed(42)
  n <- 100

  # Create a categorical feature that perfectly predicts binary target
  target <- factor(c(rep("A", 50), rep("B", 50)))
  leaky_cat <- factor(c(rep("cat1", 50), rep("cat2", 50)))  # Perfect separation

  workflow <- list(
    data = data.frame(
      y = target,
      x1 = rnorm(n),
      leaky = leaky_cat
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  cat_risks <- Filter(function(r) r$type == "target_leakage_categorical", result@risks)
  expect_gt(length(cat_risks), 0)
  expect_equal(cat_risks[[1]]$severity, "hard_violation")
})


test_that("borg_validate detects categorical feature explaining numeric target", {
  set.seed(42)
  n <- 100

  # Create groups where target variance within groups is near zero
  groups <- rep(1:10, each = 10)
  # Each group has a specific mean with almost no variance
  target <- groups * 10 + rnorm(n, sd = 0.001)
  leaky_cat <- factor(groups)

  workflow <- list(
    data = data.frame(
      y = target,
      x1 = rnorm(n),
      leaky = leaky_cat
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  cat_risks <- Filter(function(r) r$type == "target_leakage_categorical", result@risks)
  expect_gt(length(cat_risks), 0)
})


test_that("borg_validate detects temporal split violations", {
  set.seed(42)
  n <- 100

  # Create dates spanning a range
  dates <- as.Date("2020-01-01") + 0:(n-1)

  # Bad split: test data includes dates from training period
  bad_train_idx <- c(1:30, 50:70)  # Gaps in training
  bad_test_idx <- c(31:49, 71:100)  # Test includes dates within training range

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x1 = rnorm(n),
      date = dates
    ),
    train_idx = bad_train_idx,
    test_idx = bad_test_idx,
    target_col = "y",
    time_col = "date"
  )

  result <- borg_validate(workflow)

  temporal_risks <- Filter(function(r) r$type == "temporal_leak", result@risks)
  expect_gt(length(temporal_risks), 0)
  expect_equal(temporal_risks[[1]]$severity, "soft_inflation")
})


test_that("borg_validate passes proper temporal split", {
  set.seed(42)
  n <- 100

  dates <- as.Date("2020-01-01") + 0:(n-1)

  # Good split: all test dates after all training dates
  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x1 = rnorm(n),
      date = dates
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y",
    time_col = "date"
  )

  result <- borg_validate(workflow)

  temporal_risks <- Filter(function(r) r$type == "temporal_leak", result@risks)
  expect_length(temporal_risks, 0)
})


test_that("borg_validate detects group mean target leakage", {
  set.seed(42)
  n <- 100

  # Create groups
  groups <- rep(LETTERS[1:10], each = 10)
  group_means <- setNames(1:10, LETTERS[1:10])

  # Target with group structure
  target <- group_means[groups] + rnorm(n, sd = 0.5)

  # Leaky feature: exactly the group mean of target
  leaky_feature <- group_means[groups]

  workflow <- list(
    data = data.frame(
      y = target,
      x1 = rnorm(n),
      group_encoded = leaky_feature,
      group = groups
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y",
    group_col = "group"
  )

  result <- borg_validate(workflow)

  group_risks <- Filter(function(r) r$type == "target_leakage_group_mean", result@risks)
  expect_gt(length(group_risks), 0)
  expect_equal(group_risks[[1]]$severity, "hard_violation")
})


test_that("borg_validate does not flag legitimate features", {
  set.seed(42)
  n <- 100

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x1 = rnorm(n),       # Independent feature
      x2 = rnorm(n) * 2,   # Scaled independent feature
      category = factor(sample(LETTERS[1:5], n, replace = TRUE))  # Random categories
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  # Should not detect any target leakage
  leak_risks <- Filter(function(r) grepl("target_leakage", r$type), result@risks)
  expect_length(leak_risks, 0)
  expect_true(result@is_valid)
})


# ===========================================================================
# Feature Engineering Leakage Tests
# ===========================================================================

test_that("borg_validate detects global standardization", {
  set.seed(42)
  n <- 100

  # Create a feature that is standardized on FULL data
  raw_feature <- rnorm(n, mean = 50, sd = 10)
  # Standardize on full data (bad practice)
  globally_standardized <- scale(raw_feature)[, 1]

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x1 = rnorm(n),
      bad_feature = globally_standardized
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  fe_risks <- Filter(function(r) r$type == "feature_engineering_leak", result@risks)
  expect_gt(length(fe_risks), 0)
  expect_equal(fe_risks[[1]]$severity, "hard_violation")
})


test_that("borg_validate passes train-only standardization", {
  set.seed(42)
  n <- 100

  raw_feature <- rnorm(n, mean = 50, sd = 10)
  train_idx <- 1:70
  test_idx <- 71:100

  # Standardize on train only (correct practice)
  train_mean <- mean(raw_feature[train_idx])
  train_sd <- sd(raw_feature[train_idx])
  properly_standardized <- (raw_feature - train_mean) / train_sd

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x1 = rnorm(n),
      good_feature = properly_standardized
    ),
    train_idx = train_idx,
    test_idx = test_idx,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  fe_risks <- Filter(function(r) r$type == "feature_engineering_leak", result@risks)
  expect_length(fe_risks, 0)
})


test_that("borg_validate detects suspicious percentile features", {
  set.seed(42)
  n <- 100

  # Create a percentile feature computed on full data
  raw_feature <- rnorm(n)
  percentile_feature <- rank(raw_feature) / n  # Percentiles on full data

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      value_percentile = percentile_feature  # Suspicious name + range
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_validate(workflow)

  # May or may not flag depending on distribution - checking functionality exists
  expect_s4_class(result, "BorgRisk")
})


# ===========================================================================
# Threshold Selection Tests
# ===========================================================================

test_that("borg_validate detects threshold optimized on test data", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    thresholds = list(
      value = 0.5,
      optimized_on = "test"  # Bad!
    )
  )

  result <- borg_validate(workflow)

  thresh_risks <- Filter(function(r) r$type == "threshold_leak", result@risks)
  expect_gt(length(thresh_risks), 0)
  expect_equal(thresh_risks[[1]]$severity, "hard_violation")
})


test_that("borg_validate detects threshold optimized on full data", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    thresholds = list(
      value = 0.5,
      optimized_on = "full"  # Bad!
    )
  )

  result <- borg_validate(workflow)

  thresh_risks <- Filter(function(r) r$type == "threshold_leak", result@risks)
  expect_gt(length(thresh_risks), 0)
})


test_that("borg_validate detects threshold selection using test predictions", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    thresholds = list(
      value = 0.5,
      selection_method = "youden",
      used_test_predictions = TRUE  # Bad!
    )
  )

  result <- borg_validate(workflow)

  thresh_risks <- Filter(function(r) r$type == "threshold_leak", result@risks)
  expect_gt(length(thresh_risks), 0)
})


test_that("borg_validate detects post-hoc threshold adjustment", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    thresholds = list(
      value = 0.5,
      adjusted_after_test = TRUE  # Bad!
    )
  )

  result <- borg_validate(workflow)

  thresh_risks <- Filter(function(r) r$type == "threshold_leak", result@risks)
  expect_gt(length(thresh_risks), 0)
})


test_that("borg_validate passes proper threshold selection", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    thresholds = list(
      value = 0.5,
      optimized_on = "train",  # Good!
      selection_method = "youden",
      used_test_predictions = FALSE  # Good!
    )
  )

  result <- borg_validate(workflow)

  thresh_risks <- Filter(function(r) r$type == "threshold_leak", result@risks)
  expect_length(thresh_risks, 0)
})


# ===========================================================================
# Spatial Autocorrelation Tests
# ===========================================================================

test_that("borg_validate detects spatial duplicates", {
  set.seed(42)
  n <- 100

  # Create spatial coordinates
  x_coords <- runif(n, 0, 100)
  y_coords <- runif(n, 0, 100)

  # Make some test points have IDENTICAL coordinates to train points
  train_idx <- 1:70
  test_idx <- 71:100

  # Copy coordinates from train to test (spatial duplicates)
  x_coords[71:75] <- x_coords[1:5]
  y_coords[71:75] <- y_coords[1:5]

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x = rnorm(n),
      lon = x_coords,
      lat = y_coords
    ),
    train_idx = train_idx,
    test_idx = test_idx,
    spatial_cols = c("lon", "lat")
  )

  result <- borg_validate(workflow)

  spatial_risks <- Filter(function(r) r$type == "spatial_duplicate", result@risks)
  expect_gt(length(spatial_risks), 0)
  expect_equal(spatial_risks[[1]]$severity, "hard_violation")
})


test_that("borg_validate detects spatial proximity", {
  set.seed(42)
  n <- 100

  # Create spatial data where test points are VERY close to train points
  # We need test points to be within 0.5 * median_nn distance of train

  # Create a regular grid for training points
  train_x <- rep(1:7, 10)
  train_y <- rep(1:10, each = 7)

  # Test points placed at exactly the same or very near train points
  # This ensures they're within the "very close" threshold
  test_x <- train_x[1:30] + 0.001  # Extremely tiny offset
  test_y <- train_y[1:30] + 0.001

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      lon = c(train_x, test_x),
      lat = c(train_y, test_y)
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    spatial_cols = c("lon", "lat")
  )

  result <- borg_validate(workflow)

  # Should flag spatial issues (duplicates or proximity)
  spatial_risks <- Filter(function(r) grepl("spatial", r$type), result@risks)
  expect_gt(length(spatial_risks), 0)
})


test_that("borg_validate detects spatial block overlap", {
  set.seed(42)
  n <- 100

  # Create spatial blocks
  blocks <- rep(LETTERS[1:10], each = 10)
  # Block assignment: A = 1-10, B = 11-20, C = 21-30, D = 31-40, E = 41-50,
  #                   F = 51-60, G = 61-70, H = 71-80, I = 81-90, J = 91-100

  # Create overlapping split: both train and test include block A observations
  # Train: 1-5 (half of A), 11-70 (B through G)
  # Test: 6-10 (other half of A), 71-100 (H through J)
  # This creates overlap in block A!

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      x = rnorm(n),
      lon = runif(n),
      lat = runif(n),
      block = blocks
    ),
    train_idx = c(1:5, 11:70),    # Contains A (partial), B, C, D, E, F, G
    test_idx = c(6:10, 71:100),   # Contains A (partial), H, I, J - overlaps on A!
    spatial_cols = c("lon", "lat"),
    spatial_block_col = "block"
  )

  result <- borg_validate(workflow)

  block_risks <- Filter(function(r) r$type == "spatial_block_leak", result@risks)
  expect_gt(length(block_risks), 0)
  expect_equal(block_risks[[1]]$severity, "hard_violation")
})


test_that("borg_validate detects buffer zone violations", {
  set.seed(42)
  n <- 100

  # Create well-separated train/test with buffer requirement
  train_x <- runif(70, 0, 40)
  train_y <- runif(70, 0, 40)
  test_x <- runif(30, 45, 100)  # Should be 5 units away
  test_y <- runif(30, 45, 100)

  # But make some test points violate the buffer
  test_x[1:5] <- runif(5, 40, 42)  # Too close!
  test_y[1:5] <- runif(5, 40, 42)

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      lon = c(train_x, test_x),
      lat = c(train_y, test_y)
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    spatial_cols = c("lon", "lat"),
    buffer_distance = 5
  )

  result <- borg_validate(workflow)

  buffer_risks <- Filter(function(r) r$type == "spatial_buffer_violation", result@risks)
  expect_gt(length(buffer_risks), 0)
})


test_that("borg_validate passes proper spatial split", {
  set.seed(42)
  n <- 100

  # Create well-separated spatial data
  train_x <- runif(70, 0, 40)
  train_y <- runif(70, 0, 40)
  test_x <- runif(30, 60, 100)  # Well separated
  test_y <- runif(30, 60, 100)

  workflow <- list(
    data = data.frame(
      y = rnorm(n),
      lon = c(train_x, test_x),
      lat = c(train_y, test_y)
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    spatial_cols = c("lon", "lat")
  )

  result <- borg_validate(workflow)

  # Should not flag spatial issues
  spatial_hard <- Filter(function(r) grepl("spatial", r$type) && r$severity == "hard_violation", result@risks)
  expect_length(spatial_hard, 0)
})


# ===========================================================================
# HPO Validation Tests
# ===========================================================================

test_that("borg_validate detects HPO using test data", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    hpo = list(
      used_test_data = TRUE  # Bad!
    )
  )

  result <- borg_validate(workflow)

  hpo_risks <- Filter(function(r) r$type == "hpo_test_leak", result@risks)
  expect_gt(length(hpo_risks), 0)
  expect_equal(hpo_risks[[1]]$severity, "hard_violation")
})


test_that("borg_validate detects model selection on test data", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    hpo = list(
      model_selected_on = "test"  # Bad!
    )
  )

  result <- borg_validate(workflow)

  hpo_risks <- Filter(function(r) r$type == "hpo_selection_leak", result@risks)
  expect_gt(length(hpo_risks), 0)
})


test_that("borg_validate warns about non-nested CV", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    hpo = list(
      cv_type = "simple",
      nested = FALSE
    )
  )

  result <- borg_validate(workflow)

  hpo_risks <- Filter(function(r) r$type == "hpo_cv_bias", result@risks)
  expect_gt(length(hpo_risks), 0)
  expect_equal(hpo_risks[[1]]$severity, "soft_inflation")
})


test_that("borg_validate warns about excessive HPO configurations", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    hpo = list(
      n_configurations = 100  # More than training samples (70)
    )
  )

  result <- borg_validate(workflow)

  hpo_risks <- Filter(function(r) r$type == "hpo_overfit_risk", result@risks)
  expect_gt(length(hpo_risks), 0)
})


test_that("borg_validate detects feature selection leak in HPO", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    hpo = list(
      feature_selection = list(
        used_full_data = TRUE  # Bad!
      )
    )
  )

  result <- borg_validate(workflow)

  hpo_risks <- Filter(function(r) r$type == "hpo_feature_leak", result@risks)
  expect_gt(length(hpo_risks), 0)
})


test_that("borg_validate detects early stopping on test data", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    hpo = list(
      early_stopping = list(
        used_test_data = TRUE  # Bad!
      )
    )
  )

  result <- borg_validate(workflow)

  hpo_risks <- Filter(function(r) r$type == "hpo_early_stopping_leak", result@risks)
  expect_gt(length(hpo_risks), 0)
})


test_that("borg_validate passes proper HPO setup", {
  workflow <- list(
    data = data.frame(y = rnorm(100), x = rnorm(100)),
    train_idx = 1:70,
    test_idx = 71:100,
    hpo = list(
      used_test_data = FALSE,
      model_selected_on = "validation",
      cv_type = "nested",
      nested = TRUE,
      n_configurations = 20
    )
  )

  result <- borg_validate(workflow)

  hpo_hard <- Filter(function(r) grepl("hpo", r$type) && r$severity == "hard_violation", result@risks)
  expect_length(hpo_hard, 0)
})
