# ===========================================================================
# Tests for NNDM leave-one-out CV (Mila et al. 2022)
# ===========================================================================

make_clustered <- function(seed = 1) {
  set.seed(seed)
  # Six tight clusters of training points inside a large domain
  centres <- data.frame(cx = runif(6, 10, 90), cy = runif(6, 10, 90))
  x <- unlist(lapply(centres$cx, function(c) rnorm(10, c, 2)))
  y <- unlist(lapply(centres$cy, function(c) rnorm(10, c, 2)))
  data.frame(x = x, y = y, response = rnorm(length(x)))
}

nn_dist <- function(xs, ys, xt, yt) {
  # nearest distance from each (xs, ys) to the set (xt, yt)
  vapply(seq_along(xs), function(i)
    min(sqrt((xs[i] - xt)^2 + (ys[i] - yt)^2)), numeric(1))
}


test_that("nndm requires prediction_points", {
  d <- make_clustered()
  expect_error(
    borg_cv(d, coords = c("x", "y"), target = "response", strategy = "nndm"),
    "prediction_points"
  )
})


test_that("nndm returns valid leave-one-out folds", {
  d <- make_clustered()
  grid <- expand.grid(x = seq(0, 100, by = 10), y = seq(0, 100, by = 10))
  cv <- borg_cv(d, coords = c("x", "y"), target = "response",
                strategy = "nndm", prediction_points = grid)

  expect_equal(cv$strategy, "nndm")
  expect_equal(length(cv$folds), nrow(d))            # one fold per observation
  for (f in cv$folds) {
    expect_equal(length(f$test), 1L)                 # leave-one-out
    expect_equal(length(intersect(f$train, f$test)), 0)
    expect_false(f$test %in% f$train)
  }
})


test_that("nndm honours the min_train floor", {
  d <- make_clustered()
  grid <- expand.grid(x = seq(0, 100, by = 10), y = seq(0, 100, by = 10))
  cv <- borg_cv(d, coords = c("x", "y"), target = "response",
                strategy = "nndm", prediction_points = grid)

  n <- nrow(d)
  min_keep <- ceiling(0.5 * (n - 1))
  train_sizes <- vapply(cv$folds, function(f) length(f$train), integer(1))
  expect_true(all(train_sizes >= min_keep))
})


test_that("nndm matches the prediction NN distance distribution better than plain LOO", {
  d <- make_clustered()
  grid <- expand.grid(x = seq(0, 100, by = 10), y = seq(0, 100, by = 10))
  cv <- borg_cv(d, coords = c("x", "y"), target = "response",
                strategy = "nndm", prediction_points = grid)

  x <- d$x; y <- d$y

  # Target: prediction grid -> nearest training point
  target_nn <- nn_dist(grid$x, grid$y, x, y)

  # Plain LOO: each training point -> nearest OTHER training point
  D <- as.matrix(stats::dist(cbind(x, y))); diag(D) <- Inf
  loo_nn <- apply(D, 1, min)

  # NNDM: held-out point -> nearest point in its (buffered) training set
  nndm_nn <- vapply(cv$folds, function(f)
    min(sqrt((x[f$test] - x[f$train])^2 + (y[f$test] - y[f$train])^2)),
    numeric(1))

  ks <- function(a, b) as.numeric(suppressWarnings(stats::ks.test(a, b)$statistic))
  ks_loo  <- ks(loo_nn, target_nn)
  ks_nndm <- ks(nndm_nn, target_nn)

  # Distance matching should move the CV distribution toward the target
  expect_lt(ks_nndm, ks_loo)
})


test_that("nndm stores diagnostic metadata", {
  d <- make_clustered()
  grid <- expand.grid(x = seq(0, 100, by = 10), y = seq(0, 100, by = 10))
  cv <- borg_cv(d, coords = c("x", "y"), target = "response",
                strategy = "nndm", prediction_points = grid)
  meta <- attr(cv$folds, "nndm_meta")
  expect_false(is.null(meta))
  expect_equal(length(meta$exclusion_counts), nrow(d))
  expect_true(is.finite(meta$ks_statistic))
})
