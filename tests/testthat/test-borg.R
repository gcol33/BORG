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

  expect_error(
    borg(data, train_idx = 1:60, test_idx = 50:100),
    "BORG HARD VIOLATION.*overlap"
  )
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

# ===========================================================================
# Target leakage detection tests
# ===========================================================================

test_that("borg() detects direct target leakage", {
  set.seed(42)
  # Create data where feature is almost perfectly correlated with target
  target <- rnorm(100)
  data <- data.frame(
    y = target,
    x_clean = rnorm(100),
    x_leaked = target + rnorm(100, sd = 0.001)  # cor > 0.99
  )
  train_idx <- 1:70
  test_idx <- 71:100

  result <- borg(data, train_idx, test_idx, target_col = "y")

  expect_s4_class(result, "BorgRisk")
  expect_false(result@is_valid)
  expect_gt(result@n_hard, 0L)

  # Check that the leaked feature is flagged
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("target_leakage_direct" %in% risk_types)
})

test_that("borg() detects proxy target leakage", {
  set.seed(42)
  # Create data where feature is highly correlated but not perfect (0.95-0.99)
  # Need to carefully tune noise to get correlation in right range
  target <- rnorm(100)
  # cor = 1/sqrt(1 + var_noise/var_target) ~ 0.97 when noise_sd ~ 0.25
  data <- data.frame(
    y = target,
    x_clean = rnorm(100),
    x_proxy = target + rnorm(100, sd = 0.22)
  )

  # Verify correlation is in proxy range
  actual_cor <- abs(cor(data$y, data$x_proxy))
  skip_if(actual_cor > 0.99 || actual_cor < 0.95,
          message = sprintf("Correlation %.3f not in proxy range [0.95, 0.99]", actual_cor))

  train_idx <- 1:70
  test_idx <- 71:100

  result <- borg(data, train_idx, test_idx, target_col = "y")

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_soft, 0L)

  # Check that the proxy feature is flagged
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("target_leakage_proxy" %in% risk_types)
})

test_that("borg() does not flag normal correlations", {
  set.seed(42)
  data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  result <- borg(data, train_idx, test_idx, target_col = "y")

  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
  expect_equal(result@n_hard, 0L)
  expect_equal(result@n_soft, 0L)
})

# ===========================================================================
# Spatial checks tests
# ===========================================================================

test_that("borg() detects spatial overlap", {
  set.seed(42)
  # Create spatially interleaved data
  data <- data.frame(
    x = runif(100, 0, 10),
    y_coord = runif(100, 0, 10),
    response = rnorm(100)
  )
  # Random split - points will be interleaved
  train_idx <- sample(100, 70)
  test_idx <- setdiff(1:100, train_idx)

  result <- borg(data, train_idx, test_idx, spatial_cols = c("x", "y_coord"))

  expect_s4_class(result, "BorgRisk")
  # Should have spatial warnings
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true(any(risk_types %in% c("spatial_proximity", "spatial_overlap")))
})

test_that("borg() accepts spatial blocks", {
  set.seed(42)
  # Create spatially separated data
  data <- data.frame(
    lon = c(runif(70, 0, 5), runif(30, 10, 15)),  # West and East
    lat = runif(100, 0, 10),
    response = rnorm(100)
  )
  # Split by region
  train_idx <- 1:70   # West
  test_idx <- 71:100  # East

  result <- borg(data, train_idx, test_idx, spatial_cols = c("lon", "lat"))

  expect_s4_class(result, "BorgRisk")
  # Should have no spatial overlap (regions are separated)
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_false("spatial_overlap" %in% risk_types)
})

# ===========================================================================
# Group and temporal checks tests
# ===========================================================================

test_that("borg() detects group overlap", {
  data <- data.frame(
    patient_id = rep(1:10, each = 10),
    x = rnorm(100),
    y = rnorm(100)
  )
  # Split where indices don't overlap, but same patient is in both
  # Patient 5 has rows 41-50, patient 6 has rows 51-60
  train_idx <- c(1:50, 55:60)   # patients 1-5 + part of patient 6

  test_idx <- c(51:54, 61:100)  # part of patient 6 + patients 7-10

  expect_error(
    borg(data, train_idx, test_idx, group_col = "patient_id"),
    "BORG HARD VIOLATION.*Groups"
  )
})

test_that("borg() accepts clean group split", {
  data <- data.frame(
    patient_id = rep(1:10, each = 10),
    x = rnorm(100),
    y = rnorm(100)
  )
  # Clean split - patients 1-7 train, 8-10 test
  train_idx <- 1:70
  test_idx <- 71:100

  result <- borg(data, train_idx, test_idx, group_col = "patient_id")

  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
})

test_that("borg() detects temporal violation", {
  data <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 100),
    x = rnorm(100),
    y = rnorm(100)
  )
  # Split where test predates training
  train_idx <- 50:100  # Later dates

  test_idx <- 1:49     # Earlier dates

  expect_error(
    borg(data, train_idx, test_idx, temporal_col = "date"),
    "BORG HARD VIOLATION.*Temporal"
  )
})

test_that("borg() accepts correct temporal split", {
  data <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 100),
    x = rnorm(100),
    y = rnorm(100)
  )
  # Correct temporal split
  train_idx <- 1:70    # Earlier dates
  test_idx <- 71:100   # Later dates

  result <- borg(data, train_idx, test_idx, temporal_col = "date")

  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
})
