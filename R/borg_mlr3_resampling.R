# ===========================================================================
# ResamplingBorg — Native mlr3 Resampling subclass
# ===========================================================================

#' mlr3 Resampling for BORG Cross-Validation
#'
#' Creates a native \code{mlr3::Resampling} object from BORG
#' cross-validation folds, enabling direct use with mlr3 benchmarking
#' infrastructure.
#'
#' @param data A data frame.
#' @param folds A list of lists, each with \code{train} and \code{test}
#'   integer index vectors.
#' @param cv_obj A \code{borg_cv} object. If provided, \code{data} and
#'   \code{folds} are extracted from it.
#' @param id Character. Resampling identifier. Default: \code{"borg"}.
#'
#' @return An \code{mlr3::Resampling} R6 object that can be used
#'   directly with \code{mlr3::resample()}, \code{mlr3::benchmark()},
#'   and other mlr3 infrastructure.
#'
#' @details
#' The returned resampling is pre-instantiated — it contains fixed
#' train/test splits that respect BORG's spatial/temporal blocking.
#' Calling \code{$instantiate()} is not needed (and will not
#' overwrite the existing splits).
#'
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3", quietly = TRUE)) {
#'   d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#'   cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#'   resampling <- borg_to_mlr3(cv_obj = cv)
#'   resampling$iters  # number of folds
#' }
#' }
#'
#' @seealso \code{\link{borg_cv}}, \code{\link{borg_rset}}
#'
#' @export
borg_to_mlr3 <- function(data = NULL, folds = NULL, cv_obj = NULL,
                          id = "borg") {

  if (!requireNamespace("mlr3", quietly = TRUE)) {
    stop("Package 'mlr3' is required. Install with install.packages('mlr3').")
  }

  if (!is.null(cv_obj)) {
    if (!inherits(cv_obj, "borg_cv")) {
      stop("cv_obj must be a borg_cv object")
    }
    data <- cv_obj$data
    folds <- cv_obj$folds
  }

  if (is.null(data) || is.null(folds)) {
    stop("Provide either cv_obj or both data and folds")
  }

  n <- nrow(data)
  v <- length(folds)

  # Build train/test index lists
  train_sets <- lapply(folds, function(f) f$train)
  test_sets <- lapply(folds, function(f) f$test)

  # Create custom resampling via mlr3's ResamplingCustom
  resampling <- mlr3::rsmp("custom")
  resampling$id <- id

  # Instantiate with our pre-computed folds
  # ResamplingCustom$instantiate requires a task, but we can set directly
  task_dummy <- mlr3::TaskRegr$new(id = "borg_dummy",
                                    backend = data.frame(y = seq_len(n)),
                                    target = "y")
  resampling$instantiate(task_dummy,
                          train_sets = train_sets,
                          test_sets = test_sets)

  resampling
}
