#' @title BorgRisk S4 Class
#'
#' @description
#' Holds the result of \code{\link{borg_inspect}} or \code{\link{borg_validate}}:
#' a structured assessment of evaluation risks detected in a workflow or object.
#'
#' This class stores identified risks, their classification (hard violation vs
#' soft inflation), affected data indices, and recommended remediation actions.
#'
#' @name BorgRisk
#' @docType class
#' @aliases BorgRisk-class
#'
#' @slot risks A list of detected risk objects, each containing:
#'   \describe{
#'     \item{type}{Character string: risk category (e.g., "preprocessing_leak")}
#'     \item{severity}{Character string: "hard_violation" or "soft_inflation"}
#'     \item{description}{Character string: human-readable description}
#'     \item{affected_indices}{Integer vector: row/column indices affected}
#'     \item{source_object}{Character string: name of the leaky object}
#'   }
#' @slot n_hard Integer. Count of hard violations detected.
#' @slot n_soft Integer. Count of soft inflation risks detected.
#' @slot is_valid Logical. TRUE if no hard violations detected.
#' @slot train_indices Integer vector. Row indices in training set.
#' @slot test_indices Integer vector. Row indices in test set.
#' @slot timestamp POSIXct. When the inspection was performed.
#' @slot call Language object. The original call that triggered inspection.
#'
#' @seealso \code{\link{borg_inspect}}, \code{\link{borg_validate}}, \code{\link{borg}}
#'
#' @examples
#' # Create an empty BorgRisk object (no risks detected)
#' show(new("BorgRisk",
#'   risks = list(),
#'   n_hard = 0L,
#'   n_soft = 0L,
#'   is_valid = TRUE,
#'   train_indices = 1:80,
#'   test_indices = 81:100,
#'   timestamp = Sys.time(),
#'   call = quote(borg_inspect(x))
#' ))
#'
#' @importFrom methods new
#' @export
setClass(
 "BorgRisk",
 slots = list(
   risks         = "list",
   n_hard        = "integer",
   n_soft        = "integer",
   is_valid      = "logical",
   train_indices = "integer",
   test_indices  = "integer",
   timestamp     = "POSIXct",
   call          = "language"
 ),
 validity = function(object) {
   # Validate risk list structure
   for (i in seq_along(object@risks)) {
     risk <- object@risks[[i]]
     if (!is.list(risk)) {
       return(sprintf("risks[[%d]] must be a list", i))
     }
     required_fields <- c("type", "severity", "description")
     missing <- setdiff(required_fields, names(risk))
     if (length(missing) > 0) {
       return(sprintf("risks[[%d]] missing required fields: %s",
                      i, paste(missing, collapse = ", ")))
     }
     if (!risk$severity %in% c("hard_violation", "soft_inflation")) {
       return(sprintf("risks[[%d]]$severity must be 'hard_violation' or 'soft_inflation'", i))
     }
   }

   # Validate counts match
   n_hard_actual <- sum(vapply(object@risks, function(r) r$severity == "hard_violation", logical(1)))
   n_soft_actual <- sum(vapply(object@risks, function(r) r$severity == "soft_inflation", logical(1)))

   if (object@n_hard != n_hard_actual) {
     return("n_hard does not match count of hard_violation risks")
   }
   if (object@n_soft != n_soft_actual) {
     return("n_soft does not match count of soft_inflation risks")
   }

   # Validate is_valid consistency
   if (object@is_valid && object@n_hard > 0) {
     return("is_valid cannot be TRUE when hard violations exist")
   }

   # Note: We explicitly allow overlapping indices in BorgRisk when
   # is_valid = FALSE, because the overlap itself is being reported as a risk.
   # The BorgRisk object documents the detected problem.
   overlap <- intersect(object@train_indices, object@test_indices)
   if (length(overlap) > 0 && object@is_valid) {
     return("train_indices and test_indices overlap but is_valid=TRUE - this is inconsistent")
   }

   TRUE
 }
)


#' @rdname BorgRisk
#' @aliases show,BorgRisk-method
#' @importMethodsFrom methods show
#' @exportMethod show
#' @param object A \code{BorgRisk} object to be printed.
setMethod("show", "BorgRisk", function(object) {
 cat("BorgRisk Assessment\n")
 cat("===================\n\n")

 # Overall status
 if (object@is_valid) {
   cat("Status: VALID (no hard violations)\n")
 } else {
   cat("Status: INVALID (hard violations detected)\n")
 }

 cat(sprintf("  Hard violations:  %d\n", object@n_hard))
 cat(sprintf("  Soft inflations:  %d\n", object@n_soft))
 cat(sprintf("  Train indices:    %d rows\n", length(object@train_indices)))
 cat(sprintf("  Test indices:     %d rows\n", length(object@test_indices)))
 cat(sprintf("  Inspected at:     %s\n", format(object@timestamp, "%Y-%m-%d %H:%M:%S")))

 if (length(object@risks) == 0) {
   cat("\nNo risks detected.\n")
   return(invisible(NULL))
 }

 # Show hard violations first
 hard_risks <- Filter(function(r) r$severity == "hard_violation", object@risks)
 if (length(hard_risks) > 0) {
   cat("\n--- HARD VIOLATIONS (must fix) ---\n")
   for (i in seq_along(hard_risks)) {
     r <- hard_risks[[i]]
     cat(sprintf("\n[%d] %s\n", i, r$type))
     cat(sprintf("    %s\n", r$description))
     if (!is.null(r$source_object)) {
       cat(sprintf("    Source: %s\n", r$source_object))
     }
     if (!is.null(r$affected_indices) && length(r$affected_indices) > 0) {
       n_affected <- length(r$affected_indices)
       if (n_affected <= 5) {
         cat(sprintf("    Affected: %s\n", paste(r$affected_indices, collapse = ", ")))
       } else {
         cat(sprintf("    Affected: %d indices (first 5: %s)\n",
                     n_affected, paste(r$affected_indices[1:5], collapse = ", ")))
       }
     }
   }
 }

 # Show soft inflations
 soft_risks <- Filter(function(r) r$severity == "soft_inflation", object@risks)
 if (length(soft_risks) > 0) {
   cat("\n--- SOFT INFLATIONS (warnings) ---\n")
   for (i in seq_along(soft_risks)) {
     r <- soft_risks[[i]]
     cat(sprintf("\n[%d] %s\n", i, r$type))
     cat(sprintf("    %s\n", r$description))
     if (!is.null(r$source_object)) {
       cat(sprintf("    Source: %s\n", r$source_object))
     }
   }
 }

 invisible(NULL)
})


#' Coerce BorgRisk to Data Frame
#'
#' @description Converts a \code{BorgRisk} object into a data frame of detected risks.
#'
#' @name as.data.frame.BorgRisk
#' @method as.data.frame BorgRisk
#' @export
#'
#' @param x A \code{BorgRisk} object.
#' @param row.names Optional row names for the output data frame.
#' @param optional Logical. Passed to \code{data.frame()}.
#' @param ... Additional arguments passed to \code{data.frame()}.
#'
#' @return A data frame where each row corresponds to a detected risk. Columns are:
#'   \code{type}, \code{severity}, \code{description}, \code{source_object},
#'   \code{n_affected}.
#'
#' @seealso \code{\link{BorgRisk}}
as.data.frame.BorgRisk <- function(x, row.names = NULL, optional = FALSE, ...) {
 n <- length(x@risks)
 if (n == 0) {
   return(data.frame(
     type = character(0),
     severity = character(0),
     description = character(0),
     source_object = character(0),
     n_affected = integer(0),
     stringsAsFactors = FALSE
   ))
 }

 df <- data.frame(
   type = vapply(x@risks, function(r) r$type, character(1)),
   severity = vapply(x@risks, function(r) r$severity, character(1)),
   description = vapply(x@risks, function(r) r$description, character(1)),
   source_object = vapply(x@risks, function(r) r$source_object %||% NA_character_, character(1)),
   n_affected = vapply(x@risks, function(r) length(r$affected_indices %||% integer(0)), integer(1)),
   stringsAsFactors = FALSE
 )

 if (!is.null(row.names)) {
   rownames(df) <- row.names
 }

 df
}

# Null-coalescing operator (internal)
`%||%` <- function(x, y) if (is.null(x)) y else x
