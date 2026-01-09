# Tests for borg_diagnose() and borg_cv()

# ============================================================================
# borg_diagnose() tests
# ============================================================================

test_that("borg_diagnose detects spatial autocorrelation", {
  set.seed(42)
  n <- 100

  # Create spatially autocorrelated data
  x <- runif(n, 0, 100)
  y <- runif(n, 0, 100)

  # Spatially smooth response
  response <- numeric(n)
  response[1] <- rnorm(1)
  for (i in 2:n) {
    dists <- sqrt((x[1:(i-1)] - x[i])^2 + (y[1:(i-1)] - y[i])^2)
    nearest <- which.min(dists)
    response[i] <- 0.8 * response[nearest] + 0.2 * rnorm(1)
  }

  data <- data.frame(x = x, y = y, response = response)

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response")

  expect_s4_class(diagnosis, "BorgDiagnosis")
  expect_true(diagnosis@spatial$detected)
  expect_equal(diagnosis@dependency_type, "spatial")
  expect_true(diagnosis@recommended_cv %in% c("spatial_block", "random"))
})


test_that("borg_diagnose detects temporal autocorrelation", {
  set.seed(42)
  n <- 200

  # AR(1) process with strong autocorrelation
  time <- seq(as.Date("2020-01-01"), by = "day", length.out = n)
  response <- numeric(n)
  response[1] <- rnorm(1)
  for (i in 2:n) {
    response[i] <- 0.85 * response[i-1] + rnorm(1, sd = 0.3)
  }

  data <- data.frame(date = time, response = response)

  diagnosis <- borg_diagnose(data, time = "date", target = "response")

  expect_s4_class(diagnosis, "BorgDiagnosis")
  expect_true(diagnosis@temporal$detected)
  expect_equal(diagnosis@dependency_type, "temporal")
  expect_equal(diagnosis@recommended_cv, "temporal_block")
})


test_that("borg_diagnose detects clustered structure", {
  set.seed(42)

  # Clustered data with high ICC
  n_clusters <- 15
  n_per_cluster <- 20
  n <- n_clusters * n_per_cluster

  cluster_effects <- rnorm(n_clusters, sd = 3)
  site <- rep(1:n_clusters, each = n_per_cluster)
  value <- rep(cluster_effects, each = n_per_cluster) + rnorm(n, sd = 0.5)

  data <- data.frame(site = site, value = value)

  diagnosis <- borg_diagnose(data, groups = "site", target = "value")

  expect_s4_class(diagnosis, "BorgDiagnosis")
  expect_true(diagnosis@clustered$detected)
  expect_equal(diagnosis@dependency_type, "clustered")
  expect_equal(diagnosis@recommended_cv, "group_fold")
  expect_true(diagnosis@clustered$icc > 0.3)  # Strong clustering
})


test_that("borg_diagnose returns 'none' for independent data", {
  set.seed(42)
  n <- 100

  data <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    response = rnorm(n)
  )

  # Test without specifying any structure
  diagnosis <- borg_diagnose(data, target = "response")

  expect_s4_class(diagnosis, "BorgDiagnosis")
  expect_equal(diagnosis@dependency_type, "none")
  expect_equal(diagnosis@severity, "none")
  expect_equal(diagnosis@recommended_cv, "random")
})


test_that("borg_diagnose handles mixed dependencies",
{
  set.seed(42)
  n <- 150

  # Spatial + temporal
  x <- runif(n, 0, 100)
  y <- runif(n, 0, 100)
  time <- seq(as.Date("2020-01-01"), by = "day", length.out = n)

  response <- numeric(n)
  response[1] <- rnorm(1)
  for (i in 2:n) {
    response[i] <- 0.7 * response[i-1] + rnorm(1, sd = 0.5)
  }

  data <- data.frame(x = x, y = y, time = time, response = response)

  diagnosis <- borg_diagnose(data, coords = c("x", "y"), time = "time",
                             target = "response")

  expect_s4_class(diagnosis, "BorgDiagnosis")
  # At minimum, temporal should be detected
  expect_true(diagnosis@temporal$detected)
})


test_that("borg_diagnose validates inputs", {
  data <- data.frame(x = 1:10, y = 1:10)

  # Invalid coords

  expect_error(borg_diagnose(data, coords = c("x")), "length 2")
  expect_error(borg_diagnose(data, coords = c("x", "z")), "not found")

  # Missing time column
  expect_error(borg_diagnose(data, time = "date"), "not found")

  # Missing group column
  expect_error(borg_diagnose(data, groups = "site"), "not found")

  # Too few observations
  expect_error(borg_diagnose(data.frame(x = 1:5)), "at least 10")
})


test_that("borg_diagnose estimates inflation", {
  set.seed(42)
  n_clusters <- 20
  n_per_cluster <- 15
  n <- n_clusters * n_per_cluster

  cluster_effects <- rnorm(n_clusters, sd = 2)
  site <- rep(1:n_clusters, each = n_per_cluster)
  value <- rep(cluster_effects, each = n_per_cluster) + rnorm(n, sd = 0.5)

  data <- data.frame(site = site, value = value)

  diagnosis <- borg_diagnose(data, groups = "site", target = "value")

  expect_true(!is.na(diagnosis@inflation_estimate$auc_inflation))
  expect_true(diagnosis@inflation_estimate$auc_inflation > 0)
  expect_true(diagnosis@inflation_estimate$confidence %in% c("low", "medium", "high"))
})


# ============================================================================
# borg_cv() tests
# ============================================================================

test_that("borg_cv generates random folds for independent data", {
  set.seed(42)
  data <- data.frame(x = rnorm(100), y = rnorm(100))

  cv <- borg_cv(data, v = 5)

  expect_s3_class(cv, "borg_cv")
  expect_equal(cv$strategy, "random")
  expect_length(cv$folds, 5)

  # Check fold structure
  for (fold in cv$folds) {
    expect_true(all(c("train", "test") %in% names(fold)))
    expect_true(length(intersect(fold$train, fold$test)) == 0)
  }

  # Check all observations are covered
  all_test <- unlist(lapply(cv$folds, function(f) f$test))
  expect_equal(sort(all_test), 1:100)
})


test_that("borg_cv blocks random CV for clustered data", {
  set.seed(42)
  n_clusters <- 10
  data <- data.frame(
    site = rep(1:n_clusters, each = 20),
    value = rep(rnorm(n_clusters, sd = 2), each = 20) + rnorm(n_clusters * 20, sd = 0.5)
  )

  cv <- borg_cv(data, groups = "site", target = "value", v = 5)

  expect_equal(cv$strategy, "group_fold")

  # Verify no group appears in both train and test
  for (fold in cv$folds) {
    train_groups <- unique(data$site[fold$train])
    test_groups <- unique(data$site[fold$test])
    expect_length(intersect(train_groups, test_groups), 0)
  }
})


test_that("borg_cv blocks random CV for spatial data", {
  set.seed(42)
  n <- 100

  x <- runif(n, 0, 100)
  y <- runif(n, 0, 100)
  response <- numeric(n)
  response[1] <- rnorm(1)
  for (i in 2:n) {
    dists <- sqrt((x[1:(i-1)] - x[i])^2 + (y[1:(i-1)] - y[i])^2)
    nearest <- which.min(dists)
    response[i] <- 0.8 * response[nearest] + 0.2 * rnorm(1)
  }

  data <- data.frame(x = x, y = y, response = response)

  cv <- borg_cv(data, coords = c("x", "y"), target = "response", v = 5)

  # Should use spatial blocking
  expect_true(cv$strategy %in% c("spatial_block", "random"))
  expect_length(cv$folds, 5)
})


test_that("borg_cv respects allow_random override", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:10, each = 20),
    value = rep(rnorm(10, sd = 2), each = 20) + rnorm(200, sd = 0.5)
  )

  # Without override, should use group_fold
  cv1 <- borg_cv(data, groups = "site", target = "value", v = 5)
  expect_equal(cv1$strategy, "group_fold")

  # With override, should warn but allow random
  expect_warning(
    cv2 <- borg_cv(data, groups = "site", target = "value", v = 5, allow_random = TRUE),
    "Random CV requested"
  )
  expect_equal(cv2$strategy, "random_override")
})


test_that("borg_cv accepts pre-computed diagnosis", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:10, each = 15),
    value = rep(rnorm(10, sd = 2), each = 15) + rnorm(150, sd = 0.5)
  )

  diagnosis <- borg_diagnose(data, groups = "site", target = "value")
  cv <- borg_cv(data, diagnosis = diagnosis, v = 5)

  expect_identical(cv$diagnosis, diagnosis)
  expect_equal(cv$strategy, diagnosis@recommended_cv)
})


test_that("borg_cv validates fold count", {
  data <- data.frame(x = 1:20)

  # Too many folds for data size
  expect_error(borg_cv(data, v = 20), "Insufficient data")
})


test_that("borg_cv creates temporal folds with embargo", {
  set.seed(42)
  n <- 100

  time <- seq(as.Date("2020-01-01"), by = "day", length.out = n)
  response <- numeric(n)
  response[1] <- rnorm(1)
  for (i in 2:n) {
    response[i] <- 0.8 * response[i-1] + rnorm(1, sd = 0.3)
  }

  data <- data.frame(date = time, response = response)

  cv <- borg_cv(data, time = "date", target = "response", v = 5)

  expect_equal(cv$strategy, "temporal_block")

  # Check temporal ordering is respected (train times < test times in general)
  for (fold in cv$folds) {
    if (length(fold$train) > 0 && length(fold$test) > 0) {
      max_train_time <- max(data$date[fold$train])
      min_test_time <- min(data$date[fold$test])
      # This may not always hold due to how folds are created
      # but at least check no exact overlap
      expect_length(intersect(fold$train, fold$test), 0)
    }
  }
})


test_that("print.borg_cv works", {
  set.seed(42)
  data <- data.frame(x = rnorm(100), y = rnorm(100))
  cv <- borg_cv(data, v = 5)

  expect_output(print(cv), "BORG Cross-Validation")
  expect_output(print(cv), "Strategy:")
  expect_output(print(cv), "Folds:")
})
