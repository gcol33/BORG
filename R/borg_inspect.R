#' Inspect R Objects for Evaluation Risks
#'
#' `borg_inspect()` examines R objects for signals of information reuse that
#' would invalidate model evaluation. It returns a structured assessment of
#' detected risks.
#'
#' @param object An R object to inspect. Supported types include:
#'   \itemize{
#'     \item Preprocessing: \code{preProcess}, \code{recipe}, \code{prcomp}
#'     \item CV objects: \code{trainControl}, \code{rsplit}, \code{vfold_cv}
#'     \item Model objects: \code{train}, \code{lm}, \code{glm}
#'     \item Data frames with train/test split information
#'   }
#' @param train_idx Integer vector of training row indices. Required for
#'   data-level inspection.
#' @param test_idx Integer vector of test row indices. Required for
#'   data-level inspection.
#' @param data Optional data frame. Required when inspecting preprocessing
#'   objects to compare parameters against train-only statistics.
#' @param target Optional name of the target/outcome column. If provided,
#'   checks for target leakage (features highly correlated with target).
#' @param coords Optional character vector of coordinate column names.
#'   If provided, checks spatial separation between train and test.
#' @param ... Additional arguments passed to type-specific inspectors.
#'
#' @return A \code{\link{BorgRisk}} object containing:
#'   \describe{
#'     \item{risks}{List of detected risk objects}
#'     \item{n_hard}{Count of hard violations}
#'     \item{n_soft}{Count of soft inflation warnings}
#'     \item{is_valid}{TRUE if no hard violations detected}
#'   }
#'
#' @details
#' `borg_inspect()` dispatches to type-specific inspectors based on the class
#' of the input object. Each inspector looks for specific leakage patterns:
#'
#' \describe{
#'   \item{Preprocessing objects}{Checks if parameters (mean, sd, loadings)
#'     were computed on data that includes test indices}
#'   \item{CV objects}{Validates that train/test indices do not overlap and
#'     that grouping structure is respected}
#'   \item{Feature engineering}{Checks if encodings, embeddings, or derived
#'     features used test data during computation}
#' }
#'
#' @seealso
#' \code{\link{borg_validate}} for complete workflow validation,
#' \code{\link{borg}} for automated enforcement during evaluation.
#'
#' @examples
#' # Inspect a preprocessing object
#' data(mtcars)
#' train_idx <- 1:25
#' test_idx <- 26:32
#'
#' # BAD: preProcess fitted on full data (will detect leak)
#' pp_bad <- scale(mtcars[, -1])
#'
#' # GOOD: preProcess fitted on train only
#' pp_good <- scale(mtcars[train_idx, -1])
#'
#' @export
borg_inspect <- function(object, train_idx = NULL, test_idx = NULL, data = NULL,
                         target = NULL, coords = NULL, ...) {

 # ===========================================================================
 # Input validation
 # ===========================================================================

 if (missing(object)) {
   stop("'object' is required")
 }

 # Validate indices if provided
 if (!is.null(train_idx)) {
   if (!is.numeric(train_idx) && !is.integer(train_idx)) {
     stop("'train_idx' must be an integer vector")
   }
   train_idx <- as.integer(train_idx)
 }

 if (!is.null(test_idx)) {
   if (!is.numeric(test_idx) && !is.integer(test_idx)) {
     stop("'test_idx' must be an integer vector")
   }
   test_idx <- as.integer(test_idx)
 }

 # Check for index overlap (immediate hard violation)
 # Use C++ backend for fast set intersection
 if (!is.null(train_idx) && !is.null(test_idx)) {
   overlap_result <- tryCatch(
     checkIndexOverlap(train_idx, test_idx),
     error = function(e) {
       # Fallback to R if C++ not available
       overlap <- intersect(train_idx, test_idx)
       list(has_overlap = length(overlap) > 0,
            n_overlap = length(overlap),
            overlap_indices = overlap)
     }
   )

   if (overlap_result$has_overlap) {
     overlap_risk <- .new_risk(
       type = "index_overlap",
       severity = "hard_violation",
       description = sprintf(
         "Train and test indices overlap (%d shared indices). This invalidates evaluation.",
         overlap_result$n_overlap
       ),
       affected_indices = overlap_result$overlap_indices,
       source_object = "train_idx/test_idx"
     )

     return(.make_borg_risk(list(overlap_risk), train_idx, test_idx,
                            call = match.call()))
   }
 }

 # ===========================================================================
 # Dispatch to type-specific inspector
 # ===========================================================================

 risks <- list()

 # Dispatch based on object class and structure
 # Some packages (like caret) don't use proper S3/S4 classes
 obj_class <- class(object)[1]

 inspector_result <- if (obj_class == "preProcess") {
   .inspect_preProcess(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "recipe")) {
   .inspect_recipe(object, train_idx, test_idx, data, ...)
 } else if (obj_class == "prcomp") {
   .inspect_prcomp(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "rsplit")) {
   .inspect_rsplit(object, train_idx, test_idx, ...)
 } else if (inherits(object, "vfold_cv") || inherits(object, "rset")) {
   .inspect_vfold_cv(object, train_idx, test_idx, ...)
 } else if (inherits(object, "train")) {
   .inspect_caret_train(object, train_idx, test_idx, ...)
 } else if (obj_class == "lm") {
   .inspect_lm(object, train_idx, test_idx, data, ...)
 } else if (obj_class == "glm") {
   .inspect_glm(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "ranger")) {
   .inspect_ranger(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "xgb.Booster")) {
   .inspect_xgboost(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "lgb.Booster")) {
   .inspect_lightgbm(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "model_fit")) {
   .inspect_parsnip(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "workflow")) {
   .inspect_workflow(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "tune_results")) {
   .inspect_tune_results(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "Learner")) {
   .inspect_mlr3_learner(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "Resampling")) {
   .inspect_mlr3_resampling(object, train_idx, test_idx, data, ...)
 } else if (inherits(object, "Task")) {
   .inspect_mlr3_task(object, train_idx, test_idx, ...)
 } else if (inherits(object, "ResampleResult")) {
   .inspect_mlr3_resample_result(object, train_idx, test_idx, data, ...)
 } else if (is.data.frame(object)) {
   .inspect_data_frame(object, train_idx, test_idx,
                       target_col = target, spatial_cols = coords, ...)
 } else if (.is_trainControl(object)) {
   # caret::trainControl returns a list, detect by structure
   .inspect_trainControl(object, train_idx, test_idx, ...)
 } else {
   .inspect_generic(object, train_idx, test_idx, data, ...)
 }

 risks <- c(risks, inspector_result)

 # Additional nested CV leak check for train/tune objects
 if (inherits(object, "train") || inherits(object, "tune_results")) {
   nested_risks <- .inspect_nested_cv(object, train_idx, test_idx, data, ...)
   risks <- c(risks, nested_risks)
 }

 # ===========================================================================
 # Build result object
 # ===========================================================================

 .make_borg_risk(risks, train_idx, test_idx, call = match.call())
}

# Type-specific inspectors are in separate files:
#   inspect_utils.R          - %||%, .inspect_generic, .is_trainControl, .point_in_polygon
#   inspect_preprocessing.R  - preProcess, recipe, prcomp
#   inspect_resampling.R     - trainControl, rsplit, vfold_cv, rset, caret train
#   inspect_data.R           - data.frame (duplicates, target leakage, spatial)
#   inspect_models.R         - lm, glm, ranger, xgboost, lightgbm, parsnip, workflow, tune
#   inspect_mlr3.R           - mlr3 Learner, Resampling, Task, ResampleResult
