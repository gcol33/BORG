# ===========================================================================
# borg_rset() — Native rsample rset output for tidymodels integration
# ===========================================================================

#' Convert BORG Folds to an rsample rset Object
#'
#' Creates a proper \code{rset} object from BORG cross-validation folds,
#' enabling direct use with \code{tune::tune_grid()},
#' \code{tune::fit_resamples()}, and other tidymodels infrastructure
#' without manual conversion.
#'
#' @param data A data frame. Required in all cases because \code{borg_cv}
#'   objects do not store the original data.
#' @param folds A list of lists, each with \code{train} and \code{test}
#'   integer index vectors. Typically from \code{\link{borg_cv}()$folds}.
#'   Not needed when \code{cv_obj} is provided.
#' @param cv_obj A \code{borg_cv} object. If provided, \code{folds} are
#'   extracted from it.
#'
#' @return An \code{rset} object (inheriting from \code{tbl_df}) compatible
#'   with the tidymodels ecosystem. Each row has an \code{rsplit} column
#'   containing train/test index information and an \code{id} column.
#'
#' @details
#' The returned object works directly with \code{tune::tune_grid()},
#' \code{tune::fit_resamples()}, \code{rsample::assessment()}, and
#' \code{rsample::analysis()}. The folds preserve BORG's spatial/temporal
#' blocking structure.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' rset <- borg_rset(data = d, cv_obj = cv)
#' class(rset)  # "borg_rset" "rset" "tbl_df" ...
#' }
#'
#' @seealso \code{\link{borg_cv}}, \code{\link{borg_trainControl}}
#'
#' @export
borg_rset <- function(data = NULL, folds = NULL, cv_obj = NULL) {

  if (!requireNamespace("rsample", quietly = TRUE)) {
    stop("Package 'rsample' is required for borg_rset(). Install it with install.packages('rsample').")
  }


  if (!is.null(cv_obj)) {
    if (!inherits(cv_obj, "borg_cv")) {
      stop("cv_obj must be a borg_cv object")
    }
    folds <- cv_obj$folds
  }

  if (is.null(data)) {
    stop("data is required (borg_cv objects do not store the original data)")
  }
  if (is.null(folds)) {
    stop("Provide either cv_obj or folds")
  }

  n <- nrow(data)
  v <- length(folds)

  # Build rsplit objects
  splits <- lapply(seq_along(folds), function(i) {
    fold <- folds[[i]]
    rsample::make_splits(
      x = list(analysis = fold$train, assessment = fold$test),
      data = data
    )
  })

  ids <- paste0("Fold", formatC(seq_len(v), width = nchar(v), flag = "0"))

  rset <- data.frame(id = ids, stringsAsFactors = FALSE)
  rset$splits <- splits

  # Set rset class hierarchy (rsample expects tbl_df)
  class(rset) <- c("borg_rset", "rset", "tbl_df", "tbl", "data.frame")

  # rsample expects these attributes
  attr(rset, "fingerprint") <- paste0("borg_", substr(digest_simple(data), 1, 8))

  rset
}


#' @noRd
digest_simple <- function(x) {
  # Lightweight fingerprint without requiring digest package
  raw <- paste(dim(x), collapse = "_")
  vals <- paste(utils::head(unlist(x), 20), collapse = "_")
  paste0(nchar(raw), "_", nchar(vals))
}
