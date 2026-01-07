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
#'   \item{\code{\link{borg_guard}}}{Primary interface for guarding evaluation pipelines}
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
#' @name BORG-package
#' @aliases BORG
#' @useDynLib BORG, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods is new
#' @importFrom stats complete.cases cor median sd var
#' @importFrom utils head
NULL
