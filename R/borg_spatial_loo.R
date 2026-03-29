# ===========================================================================
# borg_spatial_loo() — Buffered leave-one-out cross-validation
# ===========================================================================

#' Buffered Leave-One-Out Cross-Validation
#'
#' For each observation, holds it out as the test set and removes all
#' training points within a buffer distance. This ensures spatial
#' independence between train and test in each iteration, following
#' the approach recommended by Roberts et al. (2017).
#'
#' @param data A data frame.
#' @param coords Character vector of length 2. Coordinate column names.
#' @param buffer Numeric. Exclusion buffer distance (in coordinate units).
#'   Training points within this distance of the test point are removed.
#'   If NULL, uses the estimated autocorrelation range from
#'   \code{\link{borg_diagnose}()}.
#' @param target Character. Response variable name (used for diagnosis
#'   if \code{buffer} is NULL).
#' @param diagnosis A \code{BorgDiagnosis} object. If NULL and
#'   \code{buffer} is NULL, runs \code{borg_diagnose()} to estimate
#'   the autocorrelation range.
#' @param max_iter Integer or NULL. Maximum number of LOO iterations.
#'   If NULL, uses all \code{n} observations. For large datasets,
#'   set this to subsample. Default: NULL.
#' @param seed Integer. Random seed for subsampling. Default: 42.
#' @param verbose Logical. Print progress. Default: FALSE.
#'
#' @return A list with class \code{"borg_spatial_loo"} containing:
#'   \describe{
#'     \item{folds}{List of length \code{n} (or \code{max_iter}), each
#'       with \code{train} and \code{test} integer index vectors.}
#'     \item{buffer}{Buffer distance used.}
#'     \item{n_excluded}{Integer vector: number of training points
#'       excluded per iteration.}
#'     \item{effective_train_size}{Integer vector: training set size
#'       per iteration after exclusion.}
#'   }
#'
#' @references
#' Roberts, D.R., et al. (2017). Cross-validation strategies for data
#' with temporal, spatial, hierarchical, or phylogenetic structure.
#' \emph{Ecography}, 40(8), 913-929. \doi{10.1111/ecog.02881}
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(50), y = runif(50), z = rnorm(50))
#' loo <- borg_spatial_loo(d, coords = c("x", "y"), buffer = 0.2)
#' length(loo$folds)  # 50 iterations
#' mean(loo$n_excluded)  # avg points excluded per iteration
#'
#' @seealso \code{\link{borg_cv}}, \code{\link{borg_thin}}
#'
#' @export
borg_spatial_loo <- function(data,
                             coords,
                             buffer = NULL,
                             target = NULL,
                             diagnosis = NULL,
                             max_iter = NULL,
                             seed = 42L,
                             verbose = FALSE) {

  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  if (length(coords) != 2 || !all(coords %in% names(data))) {
    stop("coords must be a character vector of 2 existing column names")
  }

  n <- nrow(data)
  x <- data[[coords[1]]]
  y <- data[[coords[2]]]

  # Determine buffer

  if (is.null(buffer)) {
    if (is.null(diagnosis)) {
      if (is.null(target)) {
        stop("Provide buffer, diagnosis, or target (to auto-estimate buffer)")
      }
      diagnosis <- borg_diagnose(data, coords = coords, target = target)
    }
    buffer <- diagnosis@spatial$range_estimate
    if (is.na(buffer)) {
      max_extent <- max(diff(range(x, na.rm = TRUE)),
                         diff(range(y, na.rm = TRUE)))
      buffer <- max_extent * 0.1
      if (verbose) message(sprintf("No range estimate; using 10%% of extent: %.2f", buffer))
    } else {
      if (verbose) message(sprintf("Using autocorrelation range as buffer: %.2f", buffer))
    }
  }

  # Compute distance matrix
  coord_mat <- cbind(x, y)
  dist_mat <- as.matrix(stats::dist(coord_mat))

  # Determine which iterations to run
  if (!is.null(max_iter) && max_iter < n) {
    set.seed(seed)
    iter_idx <- sort(sample.int(n, max_iter))
  } else {
    iter_idx <- seq_len(n)
  }

  # Build folds
  folds <- vector("list", length(iter_idx))
  n_excluded <- integer(length(iter_idx))
  effective_train_size <- integer(length(iter_idx))

  for (k in seq_along(iter_idx)) {
    i <- iter_idx[k]
    # All points within buffer of test point are excluded
    dists_to_i <- dist_mat[, i]
    excluded <- which(dists_to_i <= buffer & seq_len(n) != i)
    train_idx <- setdiff(seq_len(n), c(i, excluded))

    folds[[k]] <- list(train = train_idx, test = i)
    n_excluded[k] <- length(excluded)
    effective_train_size[k] <- length(train_idx)

    if (verbose && k %% 100 == 0) {
      message(sprintf("  LOO iteration %d/%d", k, length(iter_idx)))
    }
  }

  result <- list(
    folds = folds,
    buffer = buffer,
    n = n,
    n_iter = length(iter_idx),
    n_excluded = n_excluded,
    effective_train_size = effective_train_size
  )

  class(result) <- c("borg_spatial_loo", "borg_cv")
  result
}


#' @export
print.borg_spatial_loo <- function(x, ...) {
  cat("BORG Buffered Leave-One-Out CV\n")
  cat(sprintf("  Observations: %d | Iterations: %d\n", x$n, x$n_iter))
  cat(sprintf("  Buffer distance: %.4f\n", x$buffer))
  cat(sprintf("  Excluded per iteration: %.1f (mean), %d-%d (range)\n",
              mean(x$n_excluded), min(x$n_excluded), max(x$n_excluded)))
  cat(sprintf("  Effective train size: %.0f (mean)\n",
              mean(x$effective_train_size)))
  invisible(x)
}
