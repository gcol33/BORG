# Ground-truth simulation for the BORG JOSS paper.
#
# The Meuse head-to-head (bench_spatial_cv.R) shows that the CV scheme changes
# the reported error, but real data has no ground truth, so it cannot say which
# estimate is honest. This simulation supplies the ground truth.
#
# Setup: a Gaussian random field y over the unit square (exponential covariance,
# range 0.25). Training points are sampled in six spatial CLUSTERS -- the common
# ecological sampling pattern -- leaving large unsampled gaps. An independent
# test set is drawn uniformly over the whole square; its RMSE is the true
# generalization error for mapping to new locations. We compare:
#   * random 5-fold CV on the clustered training data (the naive estimate),
#   * BORG spatial-block CV on the same data,
# against that independent-test ground truth. Random forest throughout, single
# threaded, median over 20 seeds.
#
# Output: paper/bench_sim_results.csv (summary), paper/bench_sim_perseed.csv (raw)
# Reproduce: Rscript paper/bench_simulation.R

suppressMessages({
  library(BORG)
  library(ranger)
})

# exponential-covariance Gaussian random field over supplied points
grf <- function(coords, range = 0.25, sigma = 1) {
  d <- as.matrix(dist(coords))
  K <- sigma^2 * exp(-d / range)
  L <- chol(K + diag(1e-8, nrow(K)))
  as.numeric(crossprod(L, rnorm(nrow(K))))
}

# n_train clustered points (6 centres) + n_test uniform points; one joint field
make_data <- function(seed, n_train = 300, n_test = 500, n_clusters = 6) {
  set.seed(seed)
  centres <- cbind(runif(n_clusters), runif(n_clusters))
  cl <- sample(n_clusters, n_train, replace = TRUE)
  tr <- cbind(
    pmin(pmax(centres[cl, 1] + rnorm(n_train, 0, 0.06), 0), 1),
    pmin(pmax(centres[cl, 2] + rnorm(n_train, 0, 0.06), 0), 1)
  )
  te <- cbind(runif(n_test), runif(n_test))
  xy <- rbind(tr, te)
  field <- grf(xy, range = 0.25, sigma = 1)
  y <- field + rnorm(nrow(xy), 0, 0.15)
  train <- data.frame(x = tr[, 1], y = tr[, 2], z = y[seq_len(n_train)])
  test  <- data.frame(x = te[, 1], y = te[, 2], z = y[n_train + seq_len(n_test)])
  list(train = train, test = test)
}

rmse <- function(a, b) sqrt(mean((a - b)^2))

fit_predict <- function(train, newdata, seed) {
  fit <- ranger(z ~ x + y, data = train, num.trees = 500,
                num.threads = 1, seed = seed)
  predict(fit, data = newdata)$predictions
}

cv_rmse <- function(train, folds, seed) {
  res <- numeric(0)
  for (f in folds) {
    p <- fit_predict(train[f$train, ], train[f$test, ], seed)
    res <- c(res, train$z[f$test] - p)
  }
  sqrt(mean(res^2))
}

seeds <- 1:20
truth <- random <- borgblk <- numeric(length(seeds))

for (i in seq_along(seeds)) {
  s <- seeds[i]
  dat <- make_data(s)
  tr <- dat$train

  # ground truth: independent uniform test set
  truth[i] <- rmse(dat$test$z, fit_predict(tr, dat$test, s))

  # naive random 5-fold CV
  set.seed(s)
  fid <- sample(rep_len(1:5, nrow(tr)))
  rfolds <- lapply(1:5, function(k) list(train = which(fid != k), test = which(fid == k)))
  random[i] <- cv_rmse(tr, rfolds, s)

  # BORG spatial-block CV (diagnose + enforce)
  set.seed(s)
  b <- borg(tr, coords = c("x", "y"), target = "z", v = 5)
  bfolds <- lapply(b$folds, function(f) list(train = f$train, test = f$test))
  borgblk[i] <- cv_rmse(tr, bfolds, s)
}

res <- data.frame(
  estimate    = c("independent-test (truth)", "random 5-fold CV", "BORG spatial-block CV"),
  rmse_median = c(median(truth), median(random), median(borgblk)),
  rmse_q25    = c(quantile(truth, .25), quantile(random, .25), quantile(borgblk, .25)),
  rmse_q75    = c(quantile(truth, .75), quantile(random, .75), quantile(borgblk, .75))
)
t <- median(truth)
res$error_vs_truth_pct <- round(100 * (res$rmse_median - t) / t, 1)

# how often BORG's diagnosis fires on this clustered, autocorrelated data
fired <- vapply(seeds, function(s) {
  d <- borg_diagnose(make_data(s)$train, coords = c("x", "y"), target = "z")
  d@dependency_type == "spatial"
}, logical(1))

# per-seed raw values, so the figure can show the full distribution over seeds
perseed <- data.frame(
  seed     = rep(seeds, times = 3),
  estimate = rep(c("independent-test (truth)", "random 5-fold CV",
                   "BORG spatial-block CV"), each = length(seeds)),
  rmse     = c(truth, random, borgblk)
)

write.csv(res, "paper/bench_sim_results.csv", row.names = FALSE)
write.csv(perseed, "paper/bench_sim_perseed.csv", row.names = FALSE)
cat(sprintf("BORG detected spatial dependency in %d / %d simulations\n", sum(fired), length(fired)))
print(res, row.names = FALSE)
