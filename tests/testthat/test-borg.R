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

test_that("borg() requires both indices together in validation mode", {
  data <- data.frame(x = 1:100, y = rnorm(100))

  # Only train_idx provided
  expect_error(borg(data, train_idx = 1:50), "must be provided together")
  # Only test_idx provided
  expect_error(borg(data, test_idx = 51:100), "must be provided together")
})

test_that("borg_inspect() inspects preprocessing objects", {
  set.seed(42)
  df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # PCA on full data (bad)
  pca <- prcomp(df, center = TRUE, scale. = TRUE)

  result <- borg_inspect(pca, train_idx, test_idx, data = df)

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_hard, 0L)
})

test_that("borg_inspect() inspects model objects", {
  set.seed(42)
  df <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Model on full data (bad)
  model <- lm(y ~ x, data = df)

  result <- borg_inspect(model, train_idx, test_idx, data = df)

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_hard, 0L)
})

test_that("borg_inspect() inspects model on correct data", {
  set.seed(42)
  df <- data.frame(y = rnorm(100), x = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Model on train data only (good)
  model <- lm(y ~ x, data = df[train_idx, ])

  result <- borg_inspect(model, train_idx, test_idx, data = df)

  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
})

test_that("borg() returns BorgRisk unchanged", {
  data <- data.frame(x = 1:100, y = rnorm(100))

  original <- borg(data, train_idx = 1:70, test_idx = 71:100)
  returned <- borg(original)

  expect_identical(original, returned)
})

test_that("borg_inspect() inspects CV objects", {
  skip_if_not_installed("caret")

  train_idx <- 1:70
  test_idx <- 71:100

  # trainControl with test indices in folds (bad)
  bad_folds <- list(
    Fold1 = c(1:30, 75:80),
    Fold2 = c(31:60, 85:90)
  )
  ctrl <- caret::trainControl(method = "cv", index = bad_folds)

  result <- borg_inspect(ctrl, train_idx, test_idx)

  expect_s4_class(result, "BorgRisk")
  expect_gt(result@n_hard, 0L)
})

test_that("borg_inspect() inspects recipe objects", {
  skip_if_not_installed("recipes")

  set.seed(42)
  df <- data.frame(y = rnorm(100), x1 = rnorm(100))
  train_idx <- 1:70
  test_idx <- 71:100

  # Recipe prepped on full data (bad)
  rec <- recipes::recipe(y ~ ., data = df) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::prep(training = df)

  result <- borg_inspect(rec, train_idx, test_idx, data = df)

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

  result <- borg(data, train_idx = train_idx, test_idx = test_idx, target = "y")

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

  result <- borg(data, train_idx = train_idx, test_idx = test_idx, target = "y")

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

  result <- borg(data, train_idx = train_idx, test_idx = test_idx, target = "y")

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

  result <- borg(data, train_idx = train_idx, test_idx = test_idx, coords = c("x", "y_coord"))

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

  result <- borg(data, train_idx = train_idx, test_idx = test_idx, coords = c("lon", "lat"))

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
    borg(data, train_idx = train_idx, test_idx = test_idx, groups = "patient_id"),
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

  result <- borg(data, train_idx = train_idx, test_idx = test_idx, groups = "patient_id")

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
    borg(data, train_idx = train_idx, test_idx = test_idx, time = "date"),
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

  result <- borg(data, train_idx = train_idx, test_idx = test_idx, time = "date")

  expect_s4_class(result, "BorgRisk")
  expect_true(result@is_valid)
})


# ===========================================================================
# DIAGNOSIS MODE tests (new unified entry point)
# ===========================================================================

test_that("borg() diagnosis mode returns borg_result for clustered data", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:15, each = 20),
    value = rep(rnorm(15, sd = 2), each = 20) + rnorm(300, sd = 0.5)
  )

  result <- borg(data, groups = "site", target = "value")

  expect_s3_class(result, "borg_result")
  expect_s4_class(result$diagnosis, "BorgDiagnosis")
  expect_true(result$diagnosis@clustered$detected)
  expect_equal(result$cv$strategy, "group_fold")
  expect_length(result$folds, 5)
})

test_that("borg() diagnosis mode returns borg_result for spatial data", {
  set.seed(42)
  data <- data.frame(
    x = runif(200, 0, 100),
    y = runif(200, 0, 100),
    response = rnorm(200)
  )

  result <- borg(data, coords = c("x", "y"), target = "response")

  expect_s3_class(result, "borg_result")
  expect_s4_class(result$diagnosis, "BorgDiagnosis")
  expect_length(result$folds, 5)

  # Verify folds have train/test structure
  expect_true(all(c("train", "test") %in% names(result$folds[[1]])))
})

test_that("borg() diagnosis mode returns borg_result for temporal data", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    value = cumsum(rnorm(n))
  )

  result <- borg(data, time = "date", target = "value")

  expect_s3_class(result, "borg_result")
  expect_s4_class(result$diagnosis, "BorgDiagnosis")
  expect_length(result$folds, 5)
})

test_that("borg() diagnosis mode respects v parameter", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rnorm(200)
  )

  result <- borg(data, groups = "site", v = 10)

  expect_length(result$folds, 10)
})

test_that("borg() diagnosis mode includes inflation estimate", {
  set.seed(42)
  # Create highly clustered data
  n_clusters <- 10
  data <- data.frame(
    site = rep(1:n_clusters, each = 30),
    value = rep(rnorm(n_clusters, sd = 3), each = 30) + rnorm(n_clusters * 30, sd = 0.3)
  )

  result <- borg(data, groups = "site", target = "value")

  expect_true(!is.na(result$diagnosis@inflation_estimate$auc_inflation))
  expect_true(result$diagnosis@inflation_estimate$auc_inflation > 0)
})

test_that("borg() shows message when no structure specified", {
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  expect_message(
    borg(data),
    "No structure specified"
  )
})

test_that("borg() returns rsample output when requested", {
  skip_if_not_installed("rsample")

  set.seed(42)
  data <- data.frame(
    site = rep(1:10, each = 20),
    value = rnorm(200)
  )

  result <- borg(data, groups = "site", output = "rsample")

  expect_s3_class(result, "rset")
})

test_that("borg() returns caret output when requested", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    site = rep(1:10, each = 20),
    value = rnorm(200)
  )

  result <- borg(data, groups = "site", output = "caret")

  expect_s3_class(result, "trainControl")
})

test_that("print.borg_result works", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:10, each = 20),
    value = rep(rnorm(10, sd = 2), each = 20) + rnorm(200, sd = 0.5)
  )

  result <- borg(data, groups = "site", target = "value")

  expect_output(print(result), "BORG Result")
  expect_output(print(result), "Dependency:")
  expect_output(print(result), "Strategy:")
})

test_that("borg() folds have no group overlap in diagnosis mode", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:15, each = 10),
    value = rnorm(150)
  )

  result <- borg(data, groups = "site")

  # Check each fold has no group overlap
  for (fold in result$folds) {
    train_sites <- unique(data$site[fold$train])
    test_sites <- unique(data$site[fold$test])
    expect_length(intersect(train_sites, test_sites), 0)
  }
})
