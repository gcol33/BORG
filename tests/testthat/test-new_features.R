# ===========================================================================
# Tests for new features: conformal, calibration, drift, debias,
# bootstrap, disc_cv, shap, stability_map
# ===========================================================================

# Shared test data --------------------------------------------------------
make_spatial_data <- function(n = 200, seed = 42) {
  set.seed(seed)
  d <- data.frame(
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    a = rnorm(n),
    b = rnorm(n)
  )
  d$z <- 2 * d$a - d$b + sin(d$x / 10) + rnorm(n, sd = 0.5)
  d
}

# borg_conformal() --------------------------------------------------------
test_that("borg_conformal() produces valid prediction intervals", {
  d <- make_spatial_data()
  model <- lm(z ~ a + b + x + y, data = d)

  conf <- borg_conformal(model, d, target = "z", coords = c("x", "y"),
                          alpha = 0.1)

  expect_s3_class(conf, "borg_conformal")
  expect_true(all(c("prediction", "lower", "upper", "width") %in% names(conf)))
  expect_equal(nrow(conf), nrow(d))
  expect_true(all(conf$width > 0))
  expect_true(all(conf$upper > conf$lower))

  # Coverage should be approximately 90%
  coverage <- attr(conf, "coverage_estimate")
  expect_true(coverage > 0.75)  # generous bound for small sample

  # Check attributes
  expect_equal(attr(conf, "alpha"), 0.1)
  expect_equal(attr(conf, "nominal_coverage"), 0.9)
})

test_that("borg_conformal() works with new data", {
  d <- make_spatial_data()
  model <- lm(z ~ a + b, data = d)
  new <- data.frame(a = rnorm(30), b = rnorm(30),
                     x = runif(30), y = runif(30))

  conf <- borg_conformal(model, d, new = new, target = "z",
                          coords = c("x", "y"))

  expect_equal(nrow(conf), 30)
  expect_true(all(conf$width > 0))
})

test_that("borg_conformal() block_jackknife method works", {
  d <- make_spatial_data(n = 100)
  model <- lm(z ~ a + b, data = d)

  conf <- borg_conformal(model, d, target = "z", coords = c("x", "y"),
                          method = "block_jackknife", n_blocks = 5)

  expect_s3_class(conf, "borg_conformal")
  expect_equal(attr(conf, "method"), "block_jackknife")
})

test_that("borg_conformal() print works", {
  d <- make_spatial_data(n = 80)
  model <- lm(z ~ a + b, data = d)
  conf <- borg_conformal(model, d, target = "z")

  expect_output(print(conf), "BORG Conformal Prediction")
})


# borg_calibration() ------------------------------------------------------
test_that("borg_calibration() works for classification", {
  set.seed(42)
  probs <- runif(300)
  outcomes <- rbinom(300, 1, probs^0.8)

  cal <- borg_calibration(probs, outcomes)

  expect_s3_class(cal, "borg_calibration")
  expect_true(cal$ece >= 0)
  expect_true(cal$mce >= 0)
  expect_true(cal$brier_score >= 0 && cal$brier_score <= 1)
  expect_true(cal$type == "classification")
  expect_true(cal$assessment %in% c("well_calibrated", "moderate", "poorly_calibrated"))
  expect_true(nrow(cal$calibration_curve) > 0)
})

test_that("borg_calibration() works for regression", {
  set.seed(42)
  actual <- rnorm(200)
  predicted <- actual + rnorm(200, sd = 0.3)

  cal <- borg_calibration(predicted, actual, type = "regression")

  expect_s3_class(cal, "borg_calibration")
  expect_equal(cal$type, "regression")
  expect_true(!is.null(cal$coverage_curve))
  expect_true(cal$ece >= 0)
})

test_that("borg_calibration() validates inputs", {
  expect_error(borg_calibration(1:5, 1:3), "same length")
})

test_that("borg_calibration() print works", {
  cal <- borg_calibration(runif(100), rbinom(100, 1, 0.5))
  expect_output(print(cal), "BORG Calibration")
})


# borg_drift() ------------------------------------------------------------
test_that("borg_drift() detects shifted features", {
  set.seed(42)
  train <- data.frame(a = rnorm(200), b = rnorm(200), c = rnorm(200))
  deploy <- data.frame(a = rnorm(100, mean = 2), b = rnorm(100),
                         c = rnorm(100))

  drift <- borg_drift(train, deploy, n_perm = 20)

  expect_s3_class(drift, "borg_drift")
  expect_true(drift$n_shifted >= 1)  # at least 'a' should be detected
  expect_true(drift$feature_drift$shifted[drift$feature_drift$variable == "a"])
  expect_true(drift$classifier_auc > 0.5)
  expect_true(drift$overall_severity %in% c("none", "mild", "moderate", "severe"))
})

test_that("borg_drift() returns no shift for identical distributions", {
  set.seed(42)
  train <- data.frame(a = rnorm(200), b = rnorm(200))
  deploy <- data.frame(a = rnorm(200), b = rnorm(200))

  drift <- borg_drift(train, deploy, n_perm = 20)

  expect_true(drift$n_shifted == 0 || drift$overall_severity %in% c("none", "mild"))
})

test_that("borg_drift() print works", {
  train <- data.frame(a = rnorm(100), b = rnorm(100))
  deploy <- data.frame(a = rnorm(50, mean = 1), b = rnorm(50))
  drift <- borg_drift(train, deploy, n_perm = 10)
  expect_output(print(drift), "BORG Distribution Shift")
})


# borg_debias() -----------------------------------------------------------
test_that("borg_debias() removes spatial variance", {
  set.seed(42)
  d <- data.frame(
    x = runif(200, 0, 100), y = runif(200, 0, 100),
    temp = NA, elev = rnorm(200)
  )
  d$temp <- sin(d$x / 20) + cos(d$y / 20) + rnorm(200, sd = 0.3)
  d$z <- d$temp + d$elev + rnorm(200)

  db <- borg_debias(d, coords = c("x", "y"), target = "z")

  expect_s3_class(db, "borg_debias")
  expect_true(db$spatial_r2["temp"] > db$spatial_r2["elev"])
  expect_true(db$assessment %in% c("minimal", "moderate", "substantial"))
  expect_equal(nrow(db$data), 200)
})

test_that("borg_debias() keep_original works", {
  set.seed(42)
  d <- data.frame(x = runif(50), y = runif(50), a = rnorm(50))
  d$a <- d$a + sin(d$x * 5)

  db <- borg_debias(d, coords = c("x", "y"), keep_original = TRUE)

  expect_true("a_debiased" %in% names(db$data))
  expect_true("a" %in% names(db$data))
})

test_that("borg_debias() print works", {
  d <- data.frame(x = runif(50), y = runif(50), a = rnorm(50))
  db <- borg_debias(d, coords = c("x", "y"))
  expect_output(print(db), "BORG Spatial")
})


# borg_bootstrap() --------------------------------------------------------
test_that("borg_bootstrap() produces valid CIs", {
  d <- make_spatial_data(n = 100)
  model <- lm(z ~ a + b, data = d)

  boot <- borg_bootstrap(model, d, target = "z", coords = c("x", "y"),
                           n_boot = 30, n_blocks = 5)

  expect_s3_class(boot, "borg_bootstrap")
  expect_true(boot$ci_lower < boot$ci_upper)
  expect_true(boot$ci_lower <= boot$estimate)
  expect_true(boot$ci_upper >= boot$estimate)
  expect_equal(boot$conf_level, 0.95)
  expect_true(boot$se > 0)
  expect_equal(boot$method, "spatial_block")
})

test_that("borg_bootstrap() works without coords", {
  d <- make_spatial_data(n = 80)
  model <- lm(z ~ a + b, data = d)

  boot <- borg_bootstrap(model, d, target = "z", n_boot = 20)

  expect_s3_class(boot, "borg_bootstrap")
  expect_equal(boot$method, "random_block")
})

test_that("borg_bootstrap() print works", {
  d <- make_spatial_data(n = 60)
  model <- lm(z ~ a + b, data = d)
  boot <- borg_bootstrap(model, d, target = "z", n_boot = 10)
  expect_output(print(boot), "BORG Block Bootstrap")
})


# borg_disc_cv() ----------------------------------------------------------
test_that("borg_disc_cv() creates folds with buffer zones", {
  d <- make_spatial_data()

  cv <- borg_disc_cv(d, coords = c("x", "y"), target = "z",
                       radius = 15, v = 5)

  expect_s3_class(cv, "borg_disc_cv")
  expect_true(cv$n_folds >= 1)
  expect_true(all(cv$n_excluded > 0))  # buffer should exclude some points
  expect_true(all(cv$effective_training > 0))
  expect_true(all(cv$effective_training < 1))

  # Check fold structure
  fold <- cv$folds[[1]]
  expect_true(length(fold$train) > 0)
  expect_true(length(fold$test) > 0)
  expect_true(length(fold$excluded) > 0)

  # Train, test, excluded should not overlap
  expect_equal(length(intersect(fold$train, fold$test)), 0)
  expect_equal(length(intersect(fold$train, fold$excluded)), 0)
})

test_that("borg_disc_cv() print works", {
  d <- make_spatial_data(n = 100)
  cv <- borg_disc_cv(d, coords = c("x", "y"), radius = 10, v = 3)
  expect_output(print(cv), "Leave-Disc-Out")
})


# borg_shap() -------------------------------------------------------------
test_that("borg_shap() computes spatial SHAP values", {
  d <- make_spatial_data(n = 80)
  model <- lm(z ~ a + b, data = d)

  shap <- borg_shap(model, d, target = "z", coords = c("x", "y"),
                      explain_idx = 1:10, n_blocks = 5, n_samples = 20)

  expect_s3_class(shap, "borg_shap")
  expect_equal(nrow(shap$shap_values), 10)
  expect_equal(ncol(shap$shap_values), 2)  # a and b
  expect_true(all(c("a", "b") %in% colnames(shap$shap_values)))

  # Feature importance should rank 'a' higher (coeff = 2 vs -1)
  expect_true(shap$feature_importance["a"] > shap$feature_importance["b"])
  expect_equal(shap$method, "spatial_block")
})

test_that("borg_shap() print works", {
  d <- make_spatial_data(n = 50)
  model <- lm(z ~ a + b, data = d)
  shap <- borg_shap(model, d, target = "z", explain_idx = 1:5,
                      n_samples = 10)
  expect_output(print(shap), "BORG Spatial SHAP")
})


# borg_stability_map() ----------------------------------------------------
test_that("borg_stability_map() produces spatial output", {
  d <- make_spatial_data(n = 100)
  model <- lm(z ~ a + b, data = d)

  sm <- borg_stability_map(model, d, target = "z", coords = c("x", "y"),
                              v = 3)

  expect_s3_class(sm, "borg_stability_map")
  expect_equal(nrow(sm), 100)
  expect_true(all(c("pred_mean", "pred_sd", "pred_cv", "pred_range",
                      "x", "y") %in% names(sm)))
  expect_true(all(sm$pred_sd >= 0, na.rm = TRUE))
  expect_true(all(sm$n_folds_predicted > 0))
})

test_that("borg_stability_map() works with new prediction data", {
  d <- make_spatial_data(n = 80)
  model <- lm(z ~ a + b, data = d)
  new <- data.frame(x = runif(20, 0, 100), y = runif(20, 0, 100),
                     a = rnorm(20), b = rnorm(20))

  sm <- borg_stability_map(model, d, new = new, target = "z",
                              coords = c("x", "y"), v = 3)

  expect_equal(nrow(sm), 20)
})

test_that("borg_stability_map() print works", {
  d <- make_spatial_data(n = 60)
  model <- lm(z ~ a + b, data = d)
  sm <- borg_stability_map(model, d, target = "z", coords = c("x", "y"),
                              v = 3)
  expect_output(print(sm), "BORG Prediction Stability")
})
