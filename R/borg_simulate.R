# ===========================================================================
# borg_simulate() — Synthetic leakage benchmarking
# ===========================================================================

#' Generate Synthetic Data with Known Leakage
#'
#' Creates datasets with controlled spatial autocorrelation, temporal
#' dependence, or target leakage. The true inflation magnitude is known,
#' enabling benchmarking of leakage detection methods and quantifying
#' how much performance metrics inflate under different CV strategies.
#'
#' @param n Integer. Number of observations. Default: 500.
#' @param type Character. Type of dependency to simulate. One of
#'   \code{"spatial"}, \code{"temporal"}, \code{"target_leak"},
#'   \code{"preprocessing_leak"}, \code{"combined"}, or
#'   \code{"independent"} (control). Default: \code{"spatial"}.
#' @param n_predictors Integer. Number of predictor variables.
#'   Default: 5.
#' @param signal_strength Numeric in \code{[0, 1]}. Fraction of
#'   variance explained by predictors. Default: 0.3.
#' @param autocorrelation Numeric in \code{[0, 1]}. Strength of
#'   spatial or temporal autocorrelation. 0 = none, 1 = very strong.
#'   Default: 0.5.
#' @param leak_strength Numeric in \code{[0, 1]}. For target leakage
#'   scenarios, how strongly the leaked feature correlates with the
#'   response. Default: 0.9.
#' @param grid_size Integer. For spatial data, side length of the
#'   spatial grid (total points = \code{n}, placed on a grid of
#'   approximately this resolution). Default: NULL (auto).
#' @param seed Integer. Random seed for reproducibility. Default: 42.
#'
#' @return A list with class \code{"borg_simulation"} containing:
#'   \describe{
#'     \item{data}{The generated data frame with predictors, response,
#'       and (if spatial/temporal) coordinate/time columns.}
#'     \item{true_r2}{The true R-squared if the model could perfectly
#'       recover the signal (upper bound for honest evaluation).}
#'     \item{type}{The dependency type used.}
#'     \item{params}{List of all generation parameters.}
#'     \item{leaked_vars}{Character vector of intentionally leaked
#'       variable names (for target_leak/preprocessing_leak types).}
#'     \item{coords}{Character vector of coordinate column names
#'       (if spatial).}
#'     \item{time_col}{Name of time column (if temporal).}
#'   }
#'
#' @details
#' \subsection{Simulation types}{
#' \describe{
#'   \item{\code{"spatial"}}{Predictors and response have Gaussian
#'     spatial autocorrelation on a 2D grid. Random CV inflates R2
#'     relative to spatial block CV.}
#'   \item{\code{"temporal"}}{Predictors and response are AR(1) time
#'     series. Random CV inflates metrics relative to temporal CV.}
#'   \item{\code{"target_leak"}}{One predictor is a noisy copy of
#'     the response (post-hoc information).}
#'   \item{\code{"preprocessing_leak"}}{Predictors are normalized on
#'     the full dataset, not within each fold. The data itself is
#'     independent but leakage occurs through shared statistics.}
#'   \item{\code{"combined"}}{Spatial autocorrelation plus one
#'     target-leaked predictor.}
#'   \item{\code{"independent"}}{No dependencies. Control scenario
#'     where random and blocked CV should yield similar results.}
#' }
#' }
#'
#' @examples
#' # Spatial leakage benchmark
#' sim <- borg_simulate(n = 300, type = "spatial", autocorrelation = 0.7)
#' str(sim$data)
#' sim$true_r2
#'
#' # Compare random vs spatial CV
#' \donttest{
#' cv_spatial <- borg_cv(sim$data, coords = sim$coords, target = "y")
#' }
#'
#' @seealso \code{\link{borg_cv}}, \code{\link{borg_compare_cv}}
#'
#' @export
borg_simulate <- function(n = 500L,
                          type = c("spatial", "temporal", "target_leak",
                                   "preprocessing_leak", "combined",
                                   "independent"),
                          n_predictors = 5L,
                          signal_strength = 0.3,
                          autocorrelation = 0.5,
                          leak_strength = 0.9,
                          grid_size = NULL,
                          seed = 42L) {

  type <- match.arg(type)

  if (!is.numeric(n) || n < 20) {
    stop("n must be a numeric value >= 20")
  }
  if (!is.numeric(signal_strength) || signal_strength < 0 || signal_strength > 1) {
    stop("signal_strength must be in [0, 1]")
  }
  if (!is.numeric(autocorrelation) || autocorrelation < 0 || autocorrelation > 1) {
    stop("autocorrelation must be in [0, 1]")
  }

  set.seed(seed)

  result <- switch(type,
    spatial     = .sim_spatial(n, n_predictors, signal_strength, autocorrelation, grid_size),
    temporal    = .sim_temporal(n, n_predictors, signal_strength, autocorrelation),
    target_leak = .sim_target_leak(n, n_predictors, signal_strength, leak_strength),
    preprocessing_leak = .sim_preprocess_leak(n, n_predictors, signal_strength),
    combined    = .sim_combined(n, n_predictors, signal_strength, autocorrelation, leak_strength, grid_size),
    independent = .sim_independent(n, n_predictors, signal_strength)
  )

  result$type <- type
  result$params <- list(
    n = n, n_predictors = n_predictors, signal_strength = signal_strength,
    autocorrelation = autocorrelation, leak_strength = leak_strength,
    seed = seed
  )

  class(result) <- "borg_simulation"
  result
}


# ---------------------------------------------------------------------------
# Internal simulation engines
# ---------------------------------------------------------------------------

#' @noRd
.sim_spatial <- function(n, p, signal, rho, grid_size) {
  if (is.null(grid_size)) grid_size <- ceiling(sqrt(n))

  # Place n points on a grid with jitter
  grid_x <- seq(0, 1, length.out = grid_size)
  grid_y <- seq(0, 1, length.out = grid_size)
  coords_grid <- expand.grid(x = grid_x, y = grid_y)
  idx <- sample(nrow(coords_grid), min(n, nrow(coords_grid)))
  coords <- coords_grid[idx, ]
  if (nrow(coords) < n) {
    extra <- n - nrow(coords)
    coords <- rbind(coords, data.frame(x = stats::runif(extra), y = stats::runif(extra)))
  }
  coords <- coords[seq_len(n), ]

  # Distance matrix -> exponential covariance
  d <- as.matrix(stats::dist(coords))
  range_param <- 0.1 + rho * 0.4  # effective range scales with autocorrelation
  Sigma <- (1 - 1e-6) * exp(-d / range_param) + diag(1e-6, n)

  # Cholesky factor for correlated fields
  L <- chol(Sigma)

  # Generate spatially autocorrelated predictors
  X <- matrix(stats::rnorm(n * p), nrow = n)
  X <- t(L) %*% X  # induce correlation
  colnames(X) <- paste0("x", seq_len(p))

  # True coefficients and response
  beta <- stats::rnorm(p)
  beta <- beta / sqrt(sum(beta^2))  # unit norm
  mu <- as.numeric(X %*% beta)
  signal_var <- stats::var(mu)
  noise_sd <- sqrt(signal_var * (1 - signal) / max(signal, 1e-8))
  noise <- as.numeric(t(L) %*% stats::rnorm(n)) * noise_sd
  y <- mu + noise

  true_r2 <- signal_var / (signal_var + noise_sd^2)

  data <- data.frame(coords, X, y = y)

  list(data = data, true_r2 = true_r2, leaked_vars = character(0),
       coords = c("x", "y"), time_col = NULL)
}

#' @noRd
.sim_temporal <- function(n, p, signal, rho) {
  # AR(1) predictors
  X <- matrix(0, nrow = n, ncol = p)
  for (j in seq_len(p)) {
    X[1, j] <- stats::rnorm(1)
    for (i in 2:n) {
      X[i, j] <- rho * X[i - 1, j] + sqrt(1 - rho^2) * stats::rnorm(1)
    }
  }
  colnames(X) <- paste0("x", seq_len(p))

  # True response with AR(1) noise
  beta <- stats::rnorm(p)
  beta <- beta / sqrt(sum(beta^2))
  mu <- as.numeric(X %*% beta)
  signal_var <- stats::var(mu)
  noise_sd <- sqrt(signal_var * (1 - signal) / max(signal, 1e-8))

  noise <- numeric(n)
  noise[1] <- stats::rnorm(1) * noise_sd
  for (i in 2:n) {
    noise[i] <- rho * noise[i - 1] + sqrt(1 - rho^2) * stats::rnorm(1) * noise_sd
  }
  y <- mu + noise

  true_r2 <- signal_var / (signal_var + noise_sd^2)
  time_vals <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = n)

  data <- data.frame(time = time_vals, X, y = y)

  list(data = data, true_r2 = true_r2, leaked_vars = character(0),
       coords = NULL, time_col = "time")
}

#' @noRd
.sim_target_leak <- function(n, p, signal, leak_strength) {
  X <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(X) <- paste0("x", seq_len(p))

  beta <- stats::rnorm(p)
  beta <- beta / sqrt(sum(beta^2))
  mu <- as.numeric(X %*% beta)
  signal_var <- stats::var(mu)
  noise_sd <- sqrt(signal_var * (1 - signal) / max(signal, 1e-8))
  y <- mu + stats::rnorm(n) * noise_sd

  # Leaked feature: noisy copy of y
  leaked <- leak_strength * y + (1 - leak_strength) * stats::rnorm(n) * stats::sd(y)

  true_r2 <- signal_var / (signal_var + noise_sd^2)

  data <- data.frame(X, leaked_feature = leaked, y = y)

  list(data = data, true_r2 = true_r2, leaked_vars = "leaked_feature",
       coords = NULL, time_col = NULL)
}

#' @noRd
.sim_preprocess_leak <- function(n, p, signal) {
  X <- matrix(stats::rnorm(n * p, mean = rep(1:p, each = n),
                            sd = rep(seq(0.5, 2, length.out = p), each = n)),
              nrow = n)
  colnames(X) <- paste0("x", seq_len(p))

  beta <- stats::rnorm(p)
  beta <- beta / sqrt(sum(beta^2))

  # Normalize on ALL data (this IS the leak)
  X_normed <- scale(X)
  mu <- as.numeric(X_normed %*% beta)
  signal_var <- stats::var(mu)
  noise_sd <- sqrt(signal_var * (1 - signal) / max(signal, 1e-8))
  y <- mu + stats::rnorm(n) * noise_sd

  true_r2 <- signal_var / (signal_var + noise_sd^2)

  # Store both raw and pre-normalized
  data <- data.frame(X_normed, y = y)
  colnames(data)[seq_len(p)] <- paste0("x", seq_len(p))

  list(data = data, true_r2 = true_r2,
       leaked_vars = paste0("x", seq_len(p)),
       coords = NULL, time_col = NULL,
       note = "All predictors were normalized on full data (preprocessing leak)")
}

#' @noRd
.sim_combined <- function(n, p, signal, rho, leak_strength, grid_size) {
  spatial <- .sim_spatial(n, p, signal, rho, grid_size)

  # Add a target-leaked feature
  y <- spatial$data$y
  leaked <- leak_strength * y + (1 - leak_strength) * stats::rnorm(n) * stats::sd(y)
  spatial$data$leaked_feature <- leaked
  spatial$leaked_vars <- "leaked_feature"

  spatial
}

#' @noRd
.sim_independent <- function(n, p, signal) {
  X <- matrix(stats::rnorm(n * p), nrow = n)
  colnames(X) <- paste0("x", seq_len(p))

  beta <- stats::rnorm(p)
  beta <- beta / sqrt(sum(beta^2))
  mu <- as.numeric(X %*% beta)
  signal_var <- stats::var(mu)
  noise_sd <- sqrt(signal_var * (1 - signal) / max(signal, 1e-8))
  y <- mu + stats::rnorm(n) * noise_sd

  true_r2 <- signal_var / (signal_var + noise_sd^2)

  data <- data.frame(X, y = y)

  list(data = data, true_r2 = true_r2, leaked_vars = character(0),
       coords = NULL, time_col = NULL)
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.borg_simulation <- function(x, ...) {
  cat("BORG Synthetic Benchmark\n")
  cat(sprintf("  Type: %s\n", x$type))
  cat(sprintf("  Observations: %d | Predictors: %d\n",
              nrow(x$data), x$params$n_predictors))
  cat(sprintf("  True R2: %.4f (signal_strength = %.2f)\n",
              x$true_r2, x$params$signal_strength))
  if (length(x$leaked_vars) > 0) {
    cat(sprintf("  Leaked variables: %s\n", paste(x$leaked_vars, collapse = ", ")))
  }
  if (!is.null(x$coords)) {
    cat(sprintf("  Coordinates: %s (autocorrelation = %.2f)\n",
                paste(x$coords, collapse = ", "), x$params$autocorrelation))
  }
  if (!is.null(x$time_col)) {
    cat(sprintf("  Time column: %s (autocorrelation = %.2f)\n",
                x$time_col, x$params$autocorrelation))
  }
  invisible(x)
}
