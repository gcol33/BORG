#' @keywords internal
"_PACKAGE"

#' @title BORG: Bounded Outcome Risk Guard
#'
#' @description
#' BORG automatically detects and enforces valid model evaluation by identifying
#' information reuse between training and evaluation data. It guards against:
#'
#' \itemize{
#'   \item Data leakage through preprocessing (normalization, imputation, PCA)
#'   \item Look-ahead bias in temporal evaluation
#'   \item Spatial autocorrelation violations in block CV
#'   \item Target leakage through features derived from outcomes
#'   \item Train-test contamination through shared identifiers
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{borg}}}{Primary interface for guarding evaluation pipelines}
#'   \item{\code{\link{borg_diagnose}}}{Diagnose data dependency structure}
#'   \item{\code{\link{borg_cv}}}{Generate valid CV schemes based on diagnosis}
#'   \item{\code{\link{borg_inspect}}}{Inspect R objects for leakage signals}
#'   \item{\code{\link{borg_validate}}}{Validate a complete evaluation workflow}
#'   \item{\code{\link{borg_rewrite}}}{Automatically rewrite leaky pipelines}
#' }
#'
#' @section Risk Classification:
#' BORG classifies evaluation risks as:
#' \describe{
#'   \item{hard_violation}{Evaluation is fundamentally invalid. Must be blocked.
#'     Examples: preprocessing on full data, train-test ID overlap, target leakage.}
#'   \item{soft_inflation}{Results are biased but bounded. Performance estimates
#'     are misleading but model ranking may be preserved. Examples: insufficient

#'     spatial block size, post-hoc subgroup analysis.}
#' }
#'
#' @section Supported Frameworks:
#' BORG integrates with:
#' \itemize{
#'   \item \pkg{caret}: \code{trainControl}, \code{train}, \code{preProcess}
#'   \item \pkg{rsample}: \code{vfold_cv}, \code{initial_split}, \code{rolling_origin}
#'   \item \pkg{recipes}: \code{recipe}, \code{prep}, \code{bake}
#'   \item \pkg{mlr3}: \code{Task}, \code{Learner}, \code{Resampling}
#'   \item Base R: manual index-based splitting
#' }
#'
#' @section Options:
#' BORG respects the following options:
#' \describe{
#'   \item{\code{borg.auto_check}}{If TRUE, automatically validate splits when
#'     using supported frameworks. Default: FALSE.}
#'   \item{\code{borg.strict}}{If TRUE, throw errors on hard violations.
#'     If FALSE, return warnings. Default: TRUE.}
#'   \item{\code{borg.verbose}}{If TRUE, print diagnostic messages.
#'     Default: FALSE.}
#' }
#'
#' @name BORG-package
#' @aliases BORG
#' @useDynLib BORG, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods is new
#' @importFrom stats complete.cases cor median model.frame sd var
#' @importFrom utils head
#' @importFrom grDevices chull rgb
#' @importFrom graphics abline axis barplot legend lines mtext par plot plot.new plot.window rect text
NULL


#' Enable/Disable BORG Auto-Check Mode
#'
#' Configures BORG to automatically validate train/test splits when using
#' supported ML frameworks. When enabled, BORG will intercept common
#' modeling functions and validate indices before training proceeds.
#'
#' @param enable Logical. If TRUE, enable auto-check mode. If FALSE, disable.
#' @param strict Logical. If TRUE, throw errors on violations. If FALSE, warn.
#' @param verbose Logical. If TRUE, print diagnostic messages.
#'
#' @return Invisibly returns the previous state of auto-check options.
#'
#' @examples
#' # Enable auto-checking with strict mode
#' borg_auto_check(TRUE)
#'
#' # Disable auto-checking
#' borg_auto_check(FALSE)
#'
#' # Enable with warnings instead of errors
#' borg_auto_check(TRUE, strict = FALSE)
#'
#' @export
borg_auto_check <- function(enable = TRUE, strict = TRUE, verbose = FALSE) {
  old_opts <- list(
    borg.auto_check = getOption("borg.auto_check", FALSE),
    borg.strict = getOption("borg.strict", TRUE),
    borg.verbose = getOption("borg.verbose", FALSE)
  )

  options(
    borg.auto_check = enable,
    borg.strict = strict,
    borg.verbose = verbose
  )

  if (enable && verbose) {
    message("BORG auto-check enabled. Strict mode: ", strict)
  }

  invisible(old_opts)
}


#' Get Current BORG Options
#'
#' Returns the current state of BORG configuration options.
#'
#' @return A named list of current BORG options.
#'
#' @examples
#' borg_options()
#'
#' @export
borg_options <- function() {
  list(
    auto_check = getOption("borg.auto_check", FALSE),
    strict = getOption("borg.strict", TRUE),
    verbose = getOption("borg.verbose", FALSE)
  )
}
