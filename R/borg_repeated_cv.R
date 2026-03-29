# ===========================================================================
# borg_repeated_cv() — Repeated blocked cross-validation
# ===========================================================================

#' Repeated Blocked Cross-Validation
#'
#' Repeats \code{\link{borg_cv}} multiple times with different random seeds
#' and aggregates the results. Repeated CV provides better variance
#' estimates and more stable performance metrics than a single CV run,
#' particularly important for spatial/temporal blocking where fold
#' assignment depends on the random seed.
#'
#' @param data A data frame.
#' @param repeats Integer. Number of repetitions. Default: 10.
#' @param v Integer. Number of folds per repetition. Default: 5.
#' @param seeds Integer vector of length \code{repeats}, or NULL (auto-generated).
#'   Controls fold assignment for each repetition.
#' @param aggregate Logical. If TRUE (default), returns aggregated metrics.
#'   If FALSE, returns all individual fold results.
#' @param ... Additional arguments passed to \code{\link{borg_cv}}.
#'
#' @return A list with class \code{"borg_repeated_cv"} containing:
#'   \describe{
#'     \item{folds}{List of length \code{repeats}, each element a list of
#'       \code{v} train/test fold pairs.}
#'     \item{repeats}{Number of repetitions.}
#'     \item{v}{Number of folds.}
#'     \item{strategy}{CV strategy used.}
#'     \item{seeds}{Seeds used for each repetition.}
#'     \item{diagnosis}{The \code{BorgDiagnosis} from the first run.}
#'   }
#'   Has \code{print()} and \code{autoplot()} methods.
#'
#' @details
#' Each repetition calls \code{borg_cv()} with a different seed, producing
#' different fold assignments. The diagnosis is computed once and reused
#' across repetitions for consistency.
#'
#' For aggregation with a model, use \code{\link{borg_fold_performance}()}
#' on each repetition's folds and combine the results.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   x = runif(200), y = runif(200), z = rnorm(200)
#' )
#' rcv <- borg_repeated_cv(d, repeats = 3, v = 5,
#'                         coords = c("x", "y"), target = "z")
#' rcv
#' length(rcv$folds)  # 3 repetitions
#' length(rcv$folds[[1]])  # 5 folds each
#'
#' @seealso \code{\link{borg_cv}}, \code{\link{borg_fold_performance}}
#'
#' @export
borg_repeated_cv <- function(data,
                             repeats = 10L,
                             v = 5L,
                             seeds = NULL,
                             aggregate = TRUE,
                             ...) {

  if (!is.numeric(repeats) || repeats < 2) {
    stop("repeats must be >= 2")
  }

  if (is.null(seeds)) {
    seeds <- sample.int(1e6, repeats)
  }
  if (length(seeds) != repeats) {
    stop(sprintf("seeds must have length %d (= repeats), got %d", repeats, length(seeds)))
  }

  # Run first iteration to get diagnosis
  dots <- list(...)
  set.seed(seeds[1])
  cv1 <- do.call(borg_cv, c(list(data = data, v = v), dots))
  diagnosis <- cv1$diagnosis
  strategy <- cv1$strategy

  # Subsequent iterations reuse diagnosis
  all_folds <- vector("list", repeats)
  all_folds[[1]] <- cv1$folds

  for (r in 2:repeats) {
    set.seed(seeds[r])
    cv_r <- do.call(borg_cv, c(list(data = data, v = v, diagnosis = diagnosis), dots))
    all_folds[[r]] <- cv_r$folds
  }

  result <- list(
    folds = all_folds,
    repeats = repeats,
    v = v,
    strategy = strategy,
    seeds = seeds,
    diagnosis = diagnosis,
    data = data
  )

  class(result) <- "borg_repeated_cv"
  result
}


#' @export
print.borg_repeated_cv <- function(x, ...) {
  cat("BORG Repeated Cross-Validation\n")
  cat(sprintf("  Strategy: %s\n", x$strategy))
  cat(sprintf("  Repetitions: %d x %d folds = %d total fits\n",
              x$repeats, x$v, x$repeats * x$v))

  # Fold size summary across all repeats
  all_test_sizes <- unlist(lapply(x$folds, function(rep_folds) {
    vapply(rep_folds, function(f) length(f$test), integer(1))
  }))
  cat(sprintf("  Test fold size: %d-%d (median %d)\n",
              min(all_test_sizes), max(all_test_sizes),
              as.integer(stats::median(all_test_sizes))))

  invisible(x)
}
