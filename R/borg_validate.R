#' Validate Complete Evaluation Workflow
#'
#' `borg_validate()` performs post-hoc validation of an entire evaluation
#' workflow, checking all components for information leakage.
#'
#' @param workflow A list containing the evaluation workflow components:
#'   \describe{
#'     \item{data}{The full dataset}
#'     \item{train_idx}{Integer vector of training indices}
#'     \item{test_idx}{Integer vector of test indices}
#'     \item{preprocess}{Optional preprocessing object(s)}
#'     \item{model}{The fitted model object}
#'     \item{predictions}{Model predictions on test data}
#'     \item{metrics}{Computed evaluation metrics}
#'   }
#' @param strict Logical. If TRUE, any hard violation causes an error.
#'   Default: FALSE (returns report only).
#'
#' @return A \code{\link{BorgRisk}} object containing a comprehensive
#'   assessment of the workflow.
#'
#' @details
#' `borg_validate()` inspects each component of an evaluation workflow:
#'
#' \enumerate{
#'   \item \strong{Split validation}: Checks train/test index isolation
#'   \item \strong{Preprocessing audit}: Traces preprocessing parameters to
#'     verify train-only origin
#'   \item \strong{Feature audit}: Checks for target leakage and proxy features
#'   \item \strong{Model audit}: Validates that model used only training data
#'   \item \strong{Threshold audit}: Checks if any thresholds were optimized
#'     on test data
#' }
#'
#' @seealso
#' \code{\link{borg}} for proactive enforcement,
#' \code{\link{borg_inspect}} for single-object inspection.
#'
#' @examples
#' \dontrun{
#' # Validate an existing workflow
#' result <- borg_validate(list(
#'   data = my_data,
#'   train_idx = train_idx,
#'   test_idx = test_idx,
#'   preprocess = my_recipe,
#'   model = my_model,
#'   predictions = preds,
#'   metrics = list(rmse = 0.5, mae = 0.3)
#' ))
#'
#' # Check validity
#' if (!result@is_valid) {
#'   print(result)  # Shows detailed risk report
#' }
#' }
#'
#' @export
borg_validate <- function(workflow, strict = FALSE) {

 # ===========================================================================
 # Input validation
 # ===========================================================================

 if (!is.list(workflow)) {
   stop("'workflow' must be a list")
 }

 required_fields <- c("data", "train_idx", "test_idx")
 missing <- setdiff(required_fields, names(workflow))
 if (length(missing) > 0) {
   stop(sprintf("'workflow' missing required fields: %s",
                paste(missing, collapse = ", ")))
 }

 data <- workflow$data
 train_idx <- as.integer(workflow$train_idx)
 test_idx <- as.integer(workflow$test_idx)

 # ===========================================================================
 # Collect all risks
 # ===========================================================================

 all_risks <- list()

 # 1. Validate train/test split
 overlap <- intersect(train_idx, test_idx)
 if (length(overlap) > 0) {
   all_risks <- c(all_risks, list(.new_risk(
     type = "index_overlap",
     severity = "hard_violation",
     description = sprintf(
       "Train and test indices overlap (%d shared indices)",
       length(overlap)
     ),
     affected_indices = overlap,
     source_object = "workflow$train_idx/test_idx"
   )))
 }

 # 2. Inspect data for duplicate rows
 data_risks <- .inspect_data_frame(data, train_idx, test_idx)
 all_risks <- c(all_risks, data_risks)

 # 3. Inspect preprocessing (if provided)
 if (!is.null(workflow$preprocess)) {
   preprocess <- workflow$preprocess

   # Handle single object or list of objects
   # A single preprocessing object (like preProcess) has a specific class,
   # while a list of such objects just has class "list"
   is_single_object <- inherits(preprocess, "preProcess") ||
                       inherits(preprocess, "recipe") ||
                       inherits(preprocess, "prcomp") ||
                       (length(class(preprocess)) == 1 && class(preprocess) != "list")

   if (is_single_object) {
     preprocess <- list(preprocess)
   }

   for (i in seq_along(preprocess)) {
     pp <- preprocess[[i]]
     pp_risks <- borg_inspect(pp, train_idx, test_idx, data)@risks
     all_risks <- c(all_risks, pp_risks)
   }
 }

 # 4. Inspect model (if provided)
 if (!is.null(workflow$model)) {
   model_risks <- borg_inspect(workflow$model, train_idx, test_idx)@risks
   all_risks <- c(all_risks, model_risks)
 }

 # 5. Check for target leakage in features
 if (is.data.frame(data)) {
   target_leak_risks <- .check_target_leakage(data, train_idx, test_idx, workflow)
   all_risks <- c(all_risks, target_leak_risks)
 }

 # 6. Check for feature engineering leakage
 if (is.data.frame(data)) {
   fe_risks <- .check_feature_engineering(data, train_idx, test_idx, workflow)
   all_risks <- c(all_risks, fe_risks)
 }

 # 7. Check threshold selection issues
 if (!is.null(workflow$thresholds)) {
   thresh_risks <- .check_threshold_selection(workflow)
   all_risks <- c(all_risks, thresh_risks)
 }

 # 8. Check spatial autocorrelation issues
 if (!is.null(workflow$spatial_cols) || !is.null(workflow$coords)) {
   spatial_risks <- .check_spatial_autocorrelation(data, train_idx, test_idx, workflow)
   all_risks <- c(all_risks, spatial_risks)
 }

 # 9. Check HPO validation issues
 if (!is.null(workflow$hpo)) {
   hpo_risks <- .check_hpo_validation(workflow)
   all_risks <- c(all_risks, hpo_risks)
 }

 # 10. Check rolling feature engineering leakage
 if (is.data.frame(data) && !is.null(workflow$time_col)) {
   rolling_risks <- .check_rolling_features(data, train_idx, test_idx, workflow)
   all_risks <- c(all_risks, rolling_risks)
 }

 # 11. Check nested CV leakage (hyperparameter tuning on outer test data)
 if (!is.null(workflow$model)) {
   nested_risks <- .inspect_nested_cv(workflow$model, train_idx, test_idx, data)
   all_risks <- c(all_risks, nested_risks)
 }
 if (!is.null(workflow$tune_results)) {
   tune_risks <- .inspect_nested_cv(workflow$tune_results, train_idx, test_idx, data)
   all_risks <- c(all_risks, tune_risks)
 }

 # ===========================================================================
 # Build result
 # ===========================================================================

 result <- .make_borg_risk(all_risks, train_idx, test_idx, call = match.call())

 # Strict mode: error on hard violations
 if (strict && !result@is_valid) {
   print(result)
   stop("BORG VALIDATION FAILED: Hard violations detected (see report above)")
 }

 result
}

# ---------------------------------------------------------------------------
# Internal helper functions extracted to separate modules:
#   validate_leakage.R  — .check_target_leakage(), .check_feature_engineering()
#   validate_spatial.R  — .check_spatial_autocorrelation()
#   validate_config.R   — .check_threshold_selection(), .check_hpo_validation()
# ---------------------------------------------------------------------------
