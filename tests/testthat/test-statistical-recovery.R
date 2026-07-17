# Parameter-recovery and coverage tests for the statistical estimators.
# These simulate data with a known truth, fit to convergence, and assert the
# estimate/interval recovers that truth. They complement the shape/plumbing
# tests in test-new_features.R.

# --- borg_conformal(): split-conformal coverage on held-out data ----------
test_that("borg_conformal() split intervals reach nominal coverage out-of-sample", {
  nominal <- 0.9
  covs <- vapply(seq_len(12), function(s) {
    set.seed(s)
    n <- 400
    d <- data.frame(x = runif(n), y = runif(n), a = rnorm(n))
    d$z <- 2 * d$a + sin(d$x * 6) + rnorm(n, sd = 0.5)
    tr <- 1:250; te <- 251:n
    m <- lm(z ~ a + x + y, data = d[tr, ])
    conf <- borg_conformal(m, d[tr, ], new = d[te, ], target = "z",
                           coords = c("x", "y"), alpha = 1 - nominal)
    mean(d$z[te] >= conf$lower & d$z[te] <= conf$upper)
  }, numeric(1))

  # Empirical coverage should be at least approximately nominal (the whole
  # point of the split-conformal guarantee); in-sample calibration would push
  # this well below 0.9.
  expect_gt(mean(covs), 0.85)
  expect_lt(mean(covs), 0.99)
})

# --- borg_aoa(): points far outside training space fall outside AOA -------
test_that("borg_aoa() flags far-outside points and yields a finite threshold", {
  set.seed(11)
  train <- data.frame(a = rnorm(120), b = rnorm(120))
  inside <- data.frame(a = rnorm(60), b = rnorm(60))
  far <- data.frame(a = rnorm(60, mean = 8), b = rnorm(60, mean = 8))

  aoa_in <- borg_aoa(train, inside, predictors = c("a", "b"))
  aoa_far <- borg_aoa(train, far, predictors = c("a", "b"))

  thr <- attr(borg_di(train), "threshold")
  expect_true(is.finite(thr) && thr > 0)

  # Nearly all far points are outside; most inside points are inside.
  expect_gt(mean(!aoa_far$aoa), 0.9)
  expect_gt(mean(aoa_in$aoa), 0.6)
})

# --- borg_willmott(): match an independent hand computation ---------------
test_that("borg_willmott() matches direct formulas for d, d1, dr", {
  a <- c(1, 2, 3, 4, 5)
  p <- c(1.2, 1.8, 3.5, 3.9, 5.2)
  mo <- mean(a)

  d_ref  <- 1 - sum((a - p)^2) / sum((abs(p - mo) + abs(a - mo))^2)
  d1_ref <- 1 - sum(abs(a - p)) / sum(abs(p - mo) + abs(a - mo))
  abs_res <- sum(abs(a - p)); dr_pot <- 2 * sum(abs(a - mo))
  dr_ref <- if (abs_res <= dr_pot) 1 - abs_res / dr_pot else dr_pot / abs_res - 1

  w <- borg_willmott(a, p)
  expect_equal(w$d, d_ref)
  expect_equal(w$d1, d1_ref)
  expect_equal(w$dr, dr_ref)
})

# --- borg_calibration(): calibrated vs miscalibrated generators -----------
test_that("borg_calibration() recovers calibration quality", {
  set.seed(21)
  n <- 4000

  # Perfectly calibrated: outcome drawn from the stated probability
  probs <- runif(n)
  y_cal <- rbinom(n, 1, probs)
  cal_good <- borg_calibration(probs, y_cal, type = "classification")

  # Miscalibrated: stated probabilities vary but the true rate is ~0.5
  y_bad <- rbinom(n, 1, 0.5)
  cal_bad <- borg_calibration(probs, y_bad, type = "classification")

  expect_lt(cal_good$ece, 0.05)
  expect_gt(cal_bad$ece, cal_good$ece + 0.1)
  expect_true(cal_good$reliability_slope > 0.7 &&
              cal_good$reliability_slope < 1.3)
  expect_true(cal_good$assessment %in% c("well_calibrated", "moderate"))
})

# --- borg_bootstrap(): CI covers the known CV-RMSE ------------------------
test_that("borg_bootstrap() CI covers the true out-of-sample RMSE", {
  sigma <- 1.0
  hits <- vapply(seq_len(10), function(s) {
    set.seed(100 + s)
    n <- 300
    d <- data.frame(x = runif(n, 0, 100), y = runif(n, 0, 100),
                    z = rnorm(n, sd = sigma))
    m <- lm(z ~ 1, data = d)  # intercept-only: OOS RMSE ~ sigma
    boot <- borg_bootstrap(m, d, target = "z", coords = c("x", "y"),
                           metric = "rmse", n_boot = 120, n_blocks = 8)
    boot$ci_lower <= sigma && sigma <= boot$ci_upper
  }, logical(1))

  # A 95% CI should cover the truth in the large majority of replicates.
  expect_gte(mean(hits), 0.7)
})

# --- borg_debias(): recovers the spatial variance fraction ----------------
test_that("borg_debias() recovers the spatial share of each predictor", {
  set.seed(31)
  n <- 400
  d <- data.frame(x = runif(n, 0, 100), y = runif(n, 0, 100))
  # p_spatial is almost entirely a smooth spatial surface; p_random is noise.
  d$p_spatial <- sin(d$x / 15) + cos(d$y / 15) + rnorm(n, sd = 0.05)
  d$p_random <- rnorm(n)
  d$z <- d$p_spatial + d$p_random + rnorm(n)

  db <- borg_debias(d, predictors = c("p_spatial", "p_random"),
                    coords = c("x", "y"), target = "z")

  expect_gt(db$spatial_r2["p_spatial"], 0.6)
  expect_lt(db$spatial_r2["p_random"], 0.2)
})
