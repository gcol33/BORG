#' BORG: Validate Model Evaluation
#'
#' The main entry point for BORG. Automatically detects the appropriate
#' validation method based on the input and returns a risk assessment.
#'
#' @param x An object to validate. Can be:
#'   \itemize{
#'     \item A \code{data.frame} - validates train/test split
#'     \item A preprocessing object (\code{preProcess}, \code{recipe}, \code{prcomp})
#'     \item A model object (\code{lm}, \code{glm}, \code{ranger}, \code{train})
#'     \item A CV object (\code{trainControl}, \code{vfold_cv}, \code{rset})
#'     \item A workflow or parsnip model
#'   }
#' @param train_idx Integer vector of training indices.
#' @param test_idx Integer vector of test indices.
#' @param data Optional data frame for context when inspecting models.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A \code{\link{BorgRisk}} object containing the risk assessment.
#'
#' @details
#' \code{borg()} is the recommended entry point for most users. It automatically
#' dispatches to the appropriate validation function:
#'
#' \itemize{
#'   \item Data frames → \code{\link{borg_inspect}()} with full validation
#'   \item Preprocessing/model objects → \code{\link{borg_inspect}()}
#'   \item Workflow lists → \code{\link{borg_validate}()}
#' }
#'
#' For fine-grained control, use the specific functions directly.
#'
#' @examples
#' # Validate a train/test split
#' data <- data.frame(x = 1:100, y = rnorm(100))
#' result <- borg(data, train_idx = 1:70, test_idx = 71:100)
#' result
#'
#' # Check a preprocessing object
#' pca <- prcomp(data[1:70, ], center = TRUE, scale. = TRUE)
#' borg(pca, train_idx = 1:70, test_idx = 71:100, data = data)
#'
#' # Check a model
#' model <- lm(y ~ x, data = data[1:70, ])
#' borg(model, train_idx = 1:70, test_idx = 71:100, data = data)
#'
#' @seealso
#' \code{\link{borg_guard}} for creating guarded evaluation contexts,
#' \code{\link{borg_inspect}} for detailed object inspection,
#' \code{\link{borg_validate}} for comprehensive workflow validation.
#'
#' @export
borg <- function(x, train_idx = NULL, test_idx = NULL, data = NULL, ...) {

  # Dispatch based on input type

  if (inherits(x, "BorgRisk")) {
    # Already a BorgRisk - just return it
    return(x)
  }

  # Check if x is a workflow list (for borg_validate)
  if (is.list(x) && !is.data.frame(x) &&
      all(c("data", "train_idx", "test_idx") %in% names(x))) {
    return(borg_validate(x, ...))
  }

  # For everything else, use borg_inspect
  if (is.data.frame(x)) {
    if (is.null(train_idx) || is.null(test_idx)) {
      stop("'train_idx' and 'test_idx' are required for data frame validation")
    }
  }

  borg_inspect(x, train_idx, test_idx, data = data, ...)
}
