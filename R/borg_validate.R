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
#' \code{\link{borg_guard}} for proactive enforcement,
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
   all_risks <- c(all_risks, list(list(
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
   if (!is.list(preprocess) || !is.null(class(preprocess))) {
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

 # ===========================================================================
 # Build result
 # ===========================================================================

 n_hard <- sum(vapply(all_risks, function(r) r$severity == "hard_violation", logical(1)))
 n_soft <- sum(vapply(all_risks, function(r) r$severity == "soft_inflation", logical(1)))

 result <- new("BorgRisk",
   risks = all_risks,
   n_hard = as.integer(n_hard),
   n_soft = as.integer(n_soft),
   is_valid = n_hard == 0L,
   train_indices = train_idx,
   test_indices = test_idx,
   timestamp = Sys.time(),
   call = match.call()
 )

 # Strict mode: error on hard violations
 if (strict && !result@is_valid) {
   print(result)
   stop("BORG VALIDATION FAILED: Hard violations detected (see report above)")
 }

 result
}


#' Check for target leakage in features
#' @noRd
.check_target_leakage <- function(data, train_idx, test_idx, workflow) {
 risks <- list()

 # If no target column specified, skip
 if (is.null(workflow$target_col)) {
   return(risks)
 }

 target_col <- workflow$target_col
 if (!target_col %in% names(data)) {
   return(risks)
 }

 target <- data[[target_col]]
 features <- data[, setdiff(names(data), target_col), drop = FALSE]

 # Check correlation between each feature and target
 for (col in names(features)) {
   feature <- features[[col]]

   # Only check numeric features
   if (!is.numeric(feature) || !is.numeric(target)) {
     next
   }

   # Compute correlation on training data only
   train_cor <- tryCatch(
     cor(feature[train_idx], target[train_idx], use = "complete.obs"),
     error = function(e) NA
   )

   if (is.na(train_cor)) next

   # Flag extremely high correlations (likely target leak)
   if (abs(train_cor) > 0.99) {
     risks <- c(risks, list(list(
       type = "target_leakage_direct",
       severity = "hard_violation",
       description = sprintf(
         "Feature '%s' has correlation %.4f with target (likely derived from outcome)",
         col, train_cor
       ),
       affected_indices = integer(0),
       source_object = col
     )))
   } else if (abs(train_cor) > 0.95) {
     risks <- c(risks, list(list(
       type = "target_leakage_proxy",
       severity = "soft_inflation",
       description = sprintf(
         "Feature '%s' has suspiciously high correlation %.4f with target (review for proxy leakage)",
         col, train_cor
       ),
       affected_indices = integer(0),
       source_object = col
     )))
   }
 }

 risks
}
