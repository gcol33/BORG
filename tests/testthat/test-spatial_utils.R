# Tests for spatial abstraction layer (R/spatial_utils.R)

test_that("extract_coords works with data.frame", {
  df <- data.frame(lon = c(10, 20, 30), lat = c(40, 50, 60), z = 1:3)
  result <- extract_coords(df, c("lon", "lat"))

  expect_equal(result$x, c(10, 20, 30))
  expect_equal(result$y, c(40, 50, 60))
  expect_null(result$crs)
  expect_equal(result$coord_names, c("lon", "lat"))
})

test_that("extract_coords errors on missing columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(extract_coords(df, c("x", "y")), "not found")
})

test_that("extract_coords errors without coords for data.frame", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_error(extract_coords(df), "character vector of length 2")
})

test_that("extract_data_frame passes through data.frame", {
  df <- data.frame(a = 1:3)
  expect_identical(extract_data_frame(df), df)
})

test_that("compute_distance_matrix_geo uses Euclidean without CRS", {
  x <- c(0, 3, 0)
  y <- c(0, 0, 4)
  dm <- compute_distance_matrix_geo(x, y, crs = NULL)

  expect_equal(dm[1, 2], 3)
  expect_equal(dm[1, 3], 4)
  expect_equal(dm[2, 3], 5)
  expect_equal(diag(dm), c(0, 0, 0))
})

test_that("haversine_distance_matrix returns distances in meters", {
  # London to Paris is ~340 km
  lon <- c(-0.1278, 2.3522)
  lat <- c(51.5074, 48.8566)
  dm <- haversine_distance_matrix(lon, lat)

  expect_equal(dm[1, 1], 0)
  expect_true(dm[1, 2] > 300000 && dm[1, 2] < 400000)
  expect_equal(dm[1, 2], dm[2, 1])  # symmetric
})

test_that("is_geographic_crs returns FALSE for NULL", {
  expect_false(is_geographic_crs(NULL))
})

# sf-dependent tests -------------------------------------------------------

test_that("extract_coords works with sf", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(10, 20), y = c(40, 50), z = 1:2),
    coords = c("x", "y"), crs = 4326
  )
  result <- extract_coords(pts)

  expect_equal(result$x, c(10, 20))
  expect_equal(result$y, c(40, 50))
  expect_false(is.null(result$crs))
})

test_that("extract_data_frame drops sf geometry", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = 1:3, y = 4:6, val = 7:9),
    coords = c("x", "y"), crs = 4326
  )
  df <- extract_data_frame(pts)

  expect_true(is.data.frame(df))
  expect_false(inherits(df, "sf"))
  expect_true("val" %in% names(df))
})

test_that("is_geographic_crs detects WGS84", {
  skip_if_not_installed("sf")

  expect_true(is_geographic_crs(sf::st_crs(4326)))
})

test_that("is_geographic_crs returns FALSE for projected CRS", {
  skip_if_not_installed("sf")

  expect_false(is_geographic_crs(sf::st_crs(32632)))  # UTM zone 32N
})

test_that("compute_distance_matrix_geo uses Haversine for geographic CRS", {
  skip_if_not_installed("sf")

  lon <- c(0, 1)
  lat <- c(0, 0)
  dm_geo <- compute_distance_matrix_geo(lon, lat, crs = sf::st_crs(4326))
  dm_euc <- compute_distance_matrix_geo(lon, lat, crs = NULL)

  # Haversine should give ~111 km, Euclidean gives 1

  expect_true(dm_geo[1, 2] > 100000)
  expect_equal(dm_euc[1, 2], 1)
})

# terra-dependent tests ----------------------------------------------------

test_that("extract_coords works with SpatVector", {
  skip_if_not_installed("terra")

  v <- terra::vect(
    data.frame(x = c(10, 20), y = c(40, 50), z = 1:2),
    geom = c("x", "y"), crs = "EPSG:4326"
  )
  result <- extract_coords(v)

  expect_equal(result$x, c(10, 20))
  expect_equal(result$y, c(40, 50))
  expect_false(is.null(result$crs))
})

test_that("extract_data_frame works with SpatVector", {
  skip_if_not_installed("terra")

  v <- terra::vect(
    data.frame(x = 1:3, y = 4:6, val = 7:9),
    geom = c("x", "y"), crs = "EPSG:4326"
  )
  df <- extract_data_frame(v)

  expect_true(is.data.frame(df))
  expect_false(inherits(df, "SpatVector"))
  expect_true("val" %in% names(df))
})
