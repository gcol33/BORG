# Tests for sf/terra input acceptance in main entry points

# sf inputs ----------------------------------------------------------------

test_that("borg() accepts sf input in diagnosis mode", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(100), y = runif(100), z = rnorm(100)),
    coords = c("x", "y"), crs = 4326
  )

  result <- borg(pts, target = "z")
  expect_s3_class(result, "borg_result")
  expect_s4_class(result$diagnosis, "BorgDiagnosis")
  expect_true(length(result$folds) > 0)
})

test_that("borg_diagnose() accepts sf input", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(50), y = runif(50), z = rnorm(50)),
    coords = c("x", "y"), crs = 4326
  )

  diag <- borg_diagnose(pts, target = "z")
  expect_s4_class(diag, "BorgDiagnosis")
})

test_that("borg_cv() accepts sf input", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(100), y = runif(100), z = rnorm(100)),
    coords = c("x", "y"), crs = 4326
  )

  cv <- borg_cv(pts, target = "z")
  expect_s3_class(cv, "borg_cv")
  expect_true(length(cv$folds) > 0)
})

test_that("sf results match data.frame results", {
  skip_if_not_installed("sf")

  set.seed(42)
  df <- data.frame(x = runif(50), y = runif(50), z = rnorm(50))
  pts <- sf::st_as_sf(df, coords = c("x", "y"))

  diag_df <- borg_diagnose(df, coords = c("x", "y"), target = "z")
  diag_sf <- borg_diagnose(pts, target = "z")

  expect_equal(diag_df@dependency_type, diag_sf@dependency_type)
  expect_equal(diag_df@severity, diag_sf@severity)
  expect_equal(diag_df@recommended_cv, diag_sf@recommended_cv)
})

# terra inputs -------------------------------------------------------------

test_that("borg() accepts SpatVector input in diagnosis mode", {
  skip_if_not_installed("terra")

  set.seed(42)
  v <- terra::vect(
    data.frame(x = runif(100), y = runif(100), z = rnorm(100)),
    geom = c("x", "y"), crs = "EPSG:4326"
  )

  result <- borg(v, target = "z")
  expect_s3_class(result, "borg_result")
  expect_s4_class(result$diagnosis, "BorgDiagnosis")
})

test_that("borg_diagnose() accepts SpatVector input", {
  skip_if_not_installed("terra")

  set.seed(42)
  v <- terra::vect(
    data.frame(x = runif(50), y = runif(50), z = rnorm(50)),
    geom = c("x", "y"), crs = "EPSG:4326"
  )

  diag <- borg_diagnose(v, target = "z")
  expect_s4_class(diag, "BorgDiagnosis")
})

test_that("borg_cv() accepts SpatVector input", {
  skip_if_not_installed("terra")

  set.seed(42)
  v <- terra::vect(
    data.frame(x = runif(100), y = runif(100), z = rnorm(100)),
    geom = c("x", "y"), crs = "EPSG:4326"
  )

  cv <- borg_cv(v, target = "z")
  expect_s3_class(cv, "borg_cv")
  expect_true(length(cv$folds) > 0)
})
