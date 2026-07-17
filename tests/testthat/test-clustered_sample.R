# ===========================================================================
# Tests for borg_clustered_sample()
# ===========================================================================

test_that("clustered sample returns the requested points within a bbox", {
  s <- borg_clustered_sample(c(0, 100, 0, 100), n = 120, n_clusters = 6, seed = 1)

  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 120)
  expect_true(all(c("x", "y", "cluster") %in% names(s)))
  expect_true(all(s$x >= 0 & s$x <= 100))
  expect_true(all(s$y >= 0 & s$y <= 100))
  expect_true(all(s$cluster %in% 1:6))
  expect_lte(length(unique(s$cluster)), 6)
  expect_equal(attr(s, "n_clusters"), 6L)
})


test_that("clustered sample is reproducible with a seed", {
  s1 <- borg_clustered_sample(c(0, 50, 0, 50), n = 40, n_clusters = 4, seed = 99)
  s2 <- borg_clustered_sample(c(0, 50, 0, 50), n = 40, n_clusters = 4, seed = 99)
  expect_identical(s1, s2)
})


test_that("clustered sample produces genuine clustering", {
  # Mean nearest-neighbour distance should be well below what a uniform
  # sample of the same size over the same box would give.
  s <- borg_clustered_sample(c(0, 100, 0, 100), n = 150, n_clusters = 5,
                             radius = 3, seed = 7)
  D <- as.matrix(stats::dist(cbind(s$x, s$y))); diag(D) <- Inf
  mean_nn_clustered <- mean(apply(D, 1, min))

  set.seed(7)
  ux <- runif(150, 0, 100); uy <- runif(150, 0, 100)
  Du <- as.matrix(stats::dist(cbind(ux, uy))); diag(Du) <- Inf
  mean_nn_uniform <- mean(apply(Du, 1, min))

  expect_lt(mean_nn_clustered, mean_nn_uniform)
})


test_that("clustered sample accepts a coordinate frame as the domain", {
  coords <- data.frame(x = c(10, 20, 30), y = c(5, 15, 25))
  s <- borg_clustered_sample(coords, n = 30, n_clusters = 3, seed = 2)
  expect_equal(nrow(s), 30)
  expect_true(all(s$x >= 10 & s$x <= 30))
  expect_true(all(s$y >= 5 & s$y <= 25))
})


test_that("clustered sample feeds directly into borg_diagnose", {
  s <- borg_clustered_sample(c(0, 100, 0, 100), n = 120, n_clusters = 6,
                             radius = 2, seed = 3)
  # A response that varies smoothly with location carries spatial signal
  s$response <- 0.05 * s$x + 0.05 * s$y + rnorm(nrow(s), sd = 0.3)
  diag <- borg_diagnose(s, coords = c("x", "y"), target = "response")
  expect_equal(diag@dependency_type, "spatial")
})


test_that("clustered sample rejects a malformed numeric domain", {
  expect_error(borg_clustered_sample(c(100, 0, 0, 100), n = 10),
               "xmax > xmin")
})


test_that("clustered sample constrains points to an sf polygon", {
  skip_if_not_installed("sf")
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)))), crs = 3857)
  poly_sf <- sf::st_sf(geometry = poly)
  s <- borg_clustered_sample(poly_sf, n = 60, n_clusters = 4, seed = 5)

  pts <- sf::st_as_sf(s, coords = c("x", "y"), crs = 3857)
  hits <- lengths(sf::st_intersects(pts, poly_sf))
  expect_true(all(hits > 0))
})
