# ===========================================================================
# borg_literature_check() — Methods text scanner for leakage patterns
# ===========================================================================

#' Scan Methods Text for Common Leakage Patterns
#'
#' Analyzes a methods section (as text) for descriptions of evaluation
#' practices that commonly lead to data leakage. Useful for reviewing
#' papers, teaching, or auditing your own methods description.
#'
#' @param text Character string. The methods section text to analyze.
#'   Can be a single string or character vector (lines).
#' @param context Character. What kind of data the methods describe.
#'   One of \code{"spatial"}, \code{"temporal"}, \code{"general"},
#'   or \code{"auto"} (default, tries to detect from text).
#' @param strict Logical. If TRUE, flags borderline practices too.
#'   Default: FALSE.
#'
#' @return A list with class \code{"borg_lit_check"} containing:
#'   \describe{
#'     \item{flags}{Data frame with columns: pattern, severity
#'       (\code{"high"}, \code{"medium"}, \code{"low"}), matched_text,
#'       explanation, and recommendation.}
#'     \item{n_flags}{Total number of issues found.}
#'     \item{detected_context}{The data context detected or provided.}
#'     \item{summary}{One-line summary.}
#'   }
#'
#' @details
#' The scanner looks for textual patterns that describe evaluation
#' practices known to cause leakage. It does NOT parse actual code or
#' data — it analyzes the \emph{description} of methods.
#'
#' \subsection{Patterns checked}{
#' \describe{
#'   \item{Random CV on spatial data}{Any mention of random/stratified
#'     k-fold CV when spatial coordinates are mentioned.}
#'   \item{Preprocessing before splitting}{Normalization, PCA, or
#'     feature selection described before train/test splitting.}
#'   \item{No spatial blocking}{Spatial data evaluated without spatial
#'     CV or buffer zones.}
#'   \item{Feature selection on full data}{Variable selection or
#'     importance computed before CV.}
#'   \item{Temporal look-ahead}{Random splits on time series or
#'     panel data.}
#'   \item{No embargo}{Temporal CV without gap/embargo between
#'     train and test.}
#' }
#' }
#'
#' @examples
#' methods_text <- "We used 10-fold cross-validation to evaluate
#'   species distribution models. Predictors were normalized and
#'   reduced via PCA before model fitting. Occurrence records
#'   were collected across 50 sites in Europe."
#'
#' result <- borg_literature_check(methods_text)
#' result
#'
#' @seealso \code{\link{borg_explain_risk}}, \code{\link{borg_report}}
#'
#' @export
borg_literature_check <- function(text,
                                  context = c("auto", "spatial", "temporal",
                                              "general"),
                                  strict = FALSE) {

  context <- match.arg(context)

  if (is.character(text) && length(text) > 1) {
    text <- paste(text, collapse = " ")
  }

  text_lower <- tolower(text)

  # Detect context if auto
  if (context == "auto") {
    context <- .detect_context(text_lower)
  }

  # Run pattern matching
  patterns <- .leakage_patterns(context, strict)
  flags <- list()

  for (pat in patterns) {
    matches <- regmatches(text_lower, gregexpr(pat$regex, text_lower, perl = TRUE))[[1]]
    if (length(matches) > 0) {
      # Check for negation/exception patterns
      if (!is.null(pat$except)) {
        except_hit <- any(grepl(pat$except, text_lower, perl = TRUE))
        if (except_hit) next
      }
      flags <- c(flags, list(data.frame(
        pattern = pat$name,
        severity = pat$severity,
        matched_text = substr(matches[1], 1, 80),
        explanation = pat$explanation,
        recommendation = pat$recommendation,
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(flags) > 0) {
    flag_df <- do.call(rbind, flags)
    # Sort by severity
    sev_order <- c("high" = 1, "medium" = 2, "low" = 3)
    flag_df <- flag_df[order(sev_order[flag_df$severity]), ]
    rownames(flag_df) <- NULL
  } else {
    flag_df <- data.frame(
      pattern = character(0), severity = character(0),
      matched_text = character(0), explanation = character(0),
      recommendation = character(0), stringsAsFactors = FALSE
    )
  }

  n_high <- sum(flag_df$severity == "high")
  n_med <- sum(flag_df$severity == "medium")
  summary_text <- if (nrow(flag_df) == 0) {
    "No leakage patterns detected in the methods text."
  } else {
    sprintf("Found %d potential issues (%d high, %d medium).",
            nrow(flag_df), n_high, n_med)
  }

  result <- list(
    flags = flag_df,
    n_flags = nrow(flag_df),
    detected_context = context,
    summary = summary_text
  )

  class(result) <- "borg_lit_check"
  result
}


# ---------------------------------------------------------------------------
# Internal: context detection
# ---------------------------------------------------------------------------

#' @noRd
.detect_context <- function(text) {
  spatial_keywords <- c("spatial", "geographic", "coordinates", "latitude",
                         "longitude", "species distribution", "occurrence",
                         "site", "location", "raster", "remote sensing",
                         "gis", "utm", "epsg")
  temporal_keywords <- c("time series", "temporal", "forecast", "year",
                          "monthly", "daily", "panel data", "longitudinal",
                          "date", "season")

  n_spatial <- sum(vapply(spatial_keywords, function(k) grepl(k, text, fixed = TRUE), logical(1)))
  n_temporal <- sum(vapply(temporal_keywords, function(k) grepl(k, text, fixed = TRUE), logical(1)))

  if (n_spatial >= 2 && n_spatial > n_temporal) return("spatial")
  if (n_temporal >= 2 && n_temporal > n_spatial) return("temporal")
  if (n_spatial >= 1 && n_temporal >= 1) return("spatial")  # default to stricter
  "general"
}


# ---------------------------------------------------------------------------
# Internal: leakage pattern definitions
# ---------------------------------------------------------------------------

#' @noRd
.leakage_patterns <- function(context, strict) {
  patterns <- list()

  # Random CV on spatial data
  if (context == "spatial") {
    patterns <- c(patterns, list(list(
      name = "random_cv_spatial",
      regex = "(?:random|stratified)\\s+(?:\\d+[- ]fold|k[- ]fold|cross[- ]validation)",
      except = "spatial\\s+(?:block|buffer|leave|blocked)",
      severity = "high",
      explanation = "Random or stratified k-fold CV on spatial data ignores spatial autocorrelation.",
      recommendation = "Use spatial blocking, buffered LOO, or KNNDM CV."
    )))
  }

  # Random CV on temporal data
  if (context == "temporal") {
    patterns <- c(patterns, list(list(
      name = "random_cv_temporal",
      regex = "(?:random|stratified)\\s+(?:\\d+[- ]fold|k[- ]fold|cross[- ]validation)",
      except = "temporal\\s+(?:block|expanding|sliding|purged)",
      severity = "high",
      explanation = "Random CV on temporal data causes look-ahead bias.",
      recommendation = "Use temporal blocking with embargo, or expanding/sliding window CV."
    )))
  }

  # Preprocessing before split (all contexts)
  patterns <- c(patterns, list(list(
    name = "preprocess_before_split",
    regex = "(?:normali[sz]|standardi[sz]|pca|principal component|feature select|variable select)\\w*\\s+(?:before|prior to|then)\\s+(?:split|fold|cross|train)",
    except = NULL,
    severity = "high",
    explanation = "Preprocessing fitted on full data leaks test-set information.",
    recommendation = "Apply preprocessing inside each CV fold (e.g., using recipes)."
  )))

  # PCA/normalization without mentioning within-fold
  patterns <- c(patterns, list(list(
    name = "global_preprocessing",
    regex = "(?:all|entire|full|whole)\\s+(?:data|dataset|sample)\\s+(?:was|were)\\s+(?:normali[sz]|standardi[sz]|scaled|transformed|reduced)",
    except = NULL,
    severity = "high",
    explanation = "Preprocessing on the full dataset before splitting leaks test information.",
    recommendation = "Fit preprocessing only on training data within each CV fold."
  )))

  # Feature selection before CV
  patterns <- c(patterns, list(list(
    name = "feature_selection_before_cv",
    regex = "(?:feature|variable|predictor)\\s+(?:selection|screening|importance|ranking)\\s+(?:was|were)\\s+(?:performed|conducted|applied|computed)\\s+(?:before|prior|outside|separately)",
    except = "within|inside|each fold",
    severity = "high",
    explanation = "Feature selection outside CV uses test-fold information to choose predictors.",
    recommendation = "Perform feature selection inside each CV fold (nested CV or borg_forward_selection)."
  )))

  # No spatial blocking mentioned for spatial data
  if (context == "spatial") {
    patterns <- c(patterns, list(list(
      name = "no_spatial_blocking",
      regex = "\\d+[- ]fold\\s+cross[- ]validation",
      except = "spatial|block|buffer|leave.location|llo|knndm",
      severity = "medium",
      explanation = "CV is mentioned without spatial blocking for spatial data.",
      recommendation = "Consider spatial block CV, buffered LOO, or KNNDM."
    )))
  }

  # No embargo for temporal data
  if (context == "temporal") {
    patterns <- c(patterns, list(list(
      name = "no_embargo",
      regex = "temporal\\s+(?:cross|cv|validation|split|block)",
      except = "embargo|gap|buffer|purge",
      severity = "medium",
      explanation = "Temporal CV without an embargo period between train and test.",
      recommendation = "Add an embargo period (e.g., borg_cv with embargo argument)."
    )))
  }

  # Borderline / strict-only patterns
  if (strict) {
    patterns <- c(patterns, list(list(
      name = "single_holdout",
      regex = "(?:single|one)\\s+(?:train|hold)[- ]?(?:out|test)\\s+split",
      except = NULL,
      severity = "low",
      explanation = "Single train/test split has high variance in performance estimates.",
      recommendation = "Use repeated CV for more stable estimates."
    )))

    patterns <- c(patterns, list(list(
      name = "no_test_mentioned",
      regex = "model\\s+(?:was|were)\\s+(?:fitted|trained|built)\\s+(?:on|using)\\s+(?:the|all)\\s+(?:data|available)",
      except = "cross|validation|test|holdout|split",
      severity = "medium",
      explanation = "Model appears to be trained on all data with no evaluation split.",
      recommendation = "Hold out a test set or use cross-validation."
    )))
  }

  patterns
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.borg_lit_check <- function(x, ...) {
  cat("BORG Literature Check\n")
  cat(sprintf("  Context: %s | Flags: %d\n", x$detected_context, x$n_flags))
  cat(sprintf("  %s\n\n", x$summary))

  if (x$n_flags > 0) {
    for (i in seq_len(nrow(x$flags))) {
      f <- x$flags[i, ]
      marker <- switch(f$severity,
        high   = "!!",
        medium = "! ",
        low    = "? "
      )
      cat(sprintf("  %s [%s] %s\n", marker, toupper(f$severity), f$pattern))
      cat(sprintf("     %s\n", f$explanation))
      cat(sprintf("     Fix: %s\n\n", f$recommendation))
    }
  }
  invisible(x)
}
