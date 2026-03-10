# ===========================================================================
# Tests for inspect_data.R: spatial checks, target leakage, duplicates
# ===========================================================================

# --- Duplicate row detection ---

test_that("inspect_data_frame detects duplicate rows via C++ backend", {
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 1, 2),
    y = c(10, 20, 30, 40, 50, 10, 20)
  )

  result <- borg_inspect(data, train_idx = 1:5, test_idx = 6:7)
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("duplicate_rows" %in% risk_types)
})


test_that("inspect_data_frame handles non-numeric data (R fallback)", {
  data <- data.frame(
    name = c("a", "b", "c", "a", "b"),
    cat = c("x", "y", "z", "x", "y"),
    stringsAsFactors = FALSE
  )

  result <- borg_inspect(data, train_idx = 1:3, test_idx = 4:5)
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("duplicate_rows" %in% risk_types)
})


test_that("inspect_data_frame handles no duplicates", {
  data <- data.frame(x = 1:10, y = 11:20)
  result <- borg_inspect(data, train_idx = 1:5, test_idx = 6:10)
  dup_risks <- Filter(function(r) r$type == "duplicate_rows", result@risks)
  expect_equal(length(dup_risks), 0)
})


test_that("inspect_data_frame catches out-of-bounds indices", {
  data <- data.frame(x = 1:10)
  result <- borg_inspect(data, train_idx = 1:5, test_idx = 6:15)
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("invalid_indices" %in% risk_types)
})


# --- Target leakage detection in data frames ---

test_that("inspect_data_frame detects direct target leakage", {
  set.seed(42)
  n <- 100
  target <- rnorm(n)
  leaked <- target + rnorm(n, sd = 0.001)

  data <- data.frame(
    target = target,
    leaked_feature = leaked,
    clean_feature = rnorm(n)
  )

  result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100,
                          target = "target")
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("target_leakage_direct" %in% risk_types)
})


test_that("inspect_data_frame detects proxy leakage", {
  set.seed(42)
  n <- 100
  target <- rnorm(n)
  # Correlation ~0.97 (between 0.95 and 0.99)
  proxy <- target * 0.97 + rnorm(n, sd = 0.25)

  data <- data.frame(target = target, proxy_feature = proxy)

  result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100,
                          target = "target")
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("target_leakage_proxy" %in% risk_types)
})


test_that("inspect_data_frame handles constant feature (zero sd)", {
  data <- data.frame(
    target = rnorm(100),
    constant = rep(5, 100)
  )

  # Should not error on constant column
  expect_no_error(
    borg_inspect(data, train_idx = 1:70, test_idx = 71:100,
                 target = "target")
  )
})


test_that("inspect_data_frame handles constant target", {
  data <- data.frame(
    target = rep(1, 100),
    feature = rnorm(100)
  )

  expect_no_error(
    borg_inspect(data, train_idx = 1:70, test_idx = 71:100,
                 target = "target")
  )
})


# --- Spatial separation checks ---

test_that("inspect_data_frame detects spatial proximity", {
  set.seed(42)
  # Create train and test that are very close together
  data <- data.frame(
    x = c(runif(70, 0, 10), runif(30, 0, 10)),  # All in same small region
    y = c(runif(70, 0, 10), runif(30, 0, 10)),
    val = rnorm(100)
  )

  result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100,
                          coords = c("x", "y"))
  risk_types <- vapply(result@risks, function(r) r$type, character(1))
  expect_true("spatial_proximity" %in% risk_types ||
              "spatial_overlap" %in% risk_types)
})


test_that("inspect_data_frame handles well-separated spatial data", {
  data <- data.frame(
    x = c(rep(0, 70), rep(1000, 30)),
    y = c(rep(0, 70), rep(1000, 30)),
    val = rnorm(100)
  )

  result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100,
                          coords = c("x", "y"))
  spatial_risks <- Filter(function(r) r$type == "spatial_proximity", result@risks)
  expect_equal(length(spatial_risks), 0)
})


test_that("inspect_data_frame handles missing spatial columns gracefully", {
  data <- data.frame(x = 1:100, y = rnorm(100))

  # Non-existent spatial columns should not crash
  result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100,
                          coords = c("lon", "lat"))
  # Should still return a valid BorgRisk
  expect_s4_class(result, "BorgRisk")
})


test_that("inspect_data_frame convex hull handles < 3 test points", {
  data <- data.frame(
    x = c(runif(70, 0, 100), 50, 51),
    y = c(runif(70, 0, 100), 50, 51),
    val = rnorm(72)
  )

  # Only 2 test points - chull needs 3+, should not crash
  expect_no_error(
    borg_inspect(data, train_idx = 1:70, test_idx = 71:72,
                 coords = c("x", "y"))
  )
})
