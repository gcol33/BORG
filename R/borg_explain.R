# ===========================================================================
# borg_explain_risk() — Plain-language risk interpreter with action plan
# ===========================================================================

#' Explain Risks in Plain Language with Actionable Recommendations
#'
#' Takes a \code{\link{BorgRisk}} or \code{borg_result} object and
#' returns a prioritized, human-readable action plan. Each risk is
#' translated into what went wrong, why it matters, how to fix it,
#' and the expected inflation magnitude.
#'
#' @param x A \code{\link{BorgRisk}} or \code{borg_result} object.
#' @param style Character. Output style: \code{"console"} (default,
#'   prints to terminal), \code{"markdown"} (returns markdown string),
#'   or \code{"list"} (returns structured list).
#' @param verbose Logical. If \code{TRUE}, include background explanations.
#'   Default: \code{FALSE}.
#'
#' @return Depending on \code{style}:
#'   \describe{
#'     \item{"console"}{Invisible \code{x}, prints explanation.}
#'     \item{"markdown"}{Character string with markdown-formatted
#'       explanation.}
#'     \item{"list"}{A list of per-risk explanation objects, each with
#'       fields: \code{risk_type}, \code{severity}, \code{plain_english},
#'       \code{why_it_matters}, \code{how_to_fix}, \code{inflation_est}.}
#'   }
#'
#' @examples
#' \donttest{
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' result <- borg(d, coords = c("x", "y"), target = "z",
#'                train_idx = 1:70, test_idx = 71:100)
#' borg_explain_risk(result)
#' }
#'
#' @seealso \code{\link{borg}}, \code{\link{BorgRisk}},
#'   \code{\link{borg_assimilate}}
#'
#' @export
borg_explain_risk <- function(x,
                              style = c("console", "markdown", "list"),
                              verbose = FALSE) {
  style <- match.arg(style)

  # Extract risks
  risks <- .extract_risks(x)

  if (length(risks) == 0) {
    if (style == "console") {
      cat("No risks detected. Your evaluation pipeline looks clean.\n")
      return(invisible(x))
    }
    if (style == "markdown") return("No risks detected.")
    return(list())
  }

  # Sort by severity: hard_inflation > soft_inflation > info
  severity_order <- c("hard_inflation" = 1, "soft_inflation" = 2,
                       "possible_inflation" = 3, "info" = 4)
  ord <- order(vapply(risks, function(r) {
    severity_order[r$severity] %||% 5
  }, numeric(1)))
  risks <- risks[ord]

  # Generate explanations
  explanations <- lapply(risks, function(r) {
    .explain_single_risk(r, verbose = verbose)
  })

  if (style == "list") return(explanations)

  # Format output
  lines <- .format_explanations(explanations, style = style)

  if (style == "console") {
    cat(lines, sep = "\n")
    return(invisible(x))
  }

  # markdown
  paste(lines, collapse = "\n")
}


# ---------------------------------------------------------------------------
# Internal: extract risks from various input types
# ---------------------------------------------------------------------------

#' @noRd
.extract_risks <- function(x) {
  if (inherits(x, "BorgRisk")) {
    return(x@risks)
  }
  if (inherits(x, "borg_result")) {
    if (!is.null(x$risks) && inherits(x$risks, "BorgRisk")) {
      return(x$risks@risks)
    }
    if (!is.null(x$risks) && is.list(x$risks)) {
      return(x$risks)
    }
  }
  if (is.list(x) && all(vapply(x, is.list, logical(1)))) {
    return(x)  # already a risk list
  }
  stop("x must be a BorgRisk, borg_result, or list of risk objects")
}


# ---------------------------------------------------------------------------
# Internal: explain a single risk
# ---------------------------------------------------------------------------

#' @noRd
.explain_single_risk <- function(risk, verbose = FALSE) {
  type <- risk$type %||% "unknown"
  severity <- risk$severity %||% "unknown"
  desc <- risk$description %||% ""
  n_affected <- length(risk$affected_indices %||% integer(0))

  lookup <- .risk_explanation_table()
  entry <- lookup[[type]]

  if (is.null(entry)) {
    # Fallback: use the description as-is
    entry <- list(
      plain_english = desc,
      why_it_matters = "This could bias your evaluation metrics.",
      how_to_fix = "Review the flagged issue and apply appropriate corrections.",
      inflation_est = "Unknown"
    )
  }

  # Interpolate affected count
  plain <- gsub("\\{n\\}", as.character(n_affected), entry$plain_english)
  if (nchar(desc) > 0 && !grepl(substr(desc, 1, 30), plain, fixed = TRUE)) {
    plain <- paste0(plain, " (", desc, ")")
  }

  result <- list(
    risk_type = type,
    severity = severity,
    severity_label = .severity_label(severity),
    plain_english = plain,
    why_it_matters = entry$why_it_matters,
    how_to_fix = entry$how_to_fix,
    inflation_est = entry$inflation_est,
    n_affected = n_affected
  )

  if (verbose) {
    result$background <- entry$background %||% NULL
  }

  result
}


# ---------------------------------------------------------------------------
# Internal: risk explanation lookup table
# ---------------------------------------------------------------------------

#' @noRd
.risk_explanation_table <- function() {
  list(
    spatial_autocorrelation = list(
      plain_english = "Your train and test data are spatially autocorrelated -- nearby points share information.",
      why_it_matters = "Random CV lets nearby points leak between train and test, inflating apparent accuracy. The model memorizes local patterns that don't generalize.",
      how_to_fix = "Use borg_cv() to generate spatially blocked folds, or add a spatial buffer with the buffer argument.",
      inflation_est = "Typically 5-30% R2 inflation depending on autocorrelation range.",
      background = "Spatial autocorrelation means observations close in space are more similar. Random CV treats all observations as exchangeable, but spatial neighbors carry redundant information."
    ),
    temporal_autocorrelation = list(
      plain_english = "Your data has temporal autocorrelation -- consecutive time points are correlated.",
      why_it_matters = "Random CV allows future observations to train the model, creating look-ahead bias.",
      how_to_fix = "Use borg_cv() with a time column, or use strategy = 'temporal_block'. Set an appropriate embargo period.",
      inflation_est = "Typically 10-40% metric inflation for strongly autocorrelated series.",
      background = "Temporal leakage is the #1 cause of overly optimistic forecasting models (Kaufman et al. 2012)."
    ),
    train_test_overlap = list(
      plain_english = "{n} observations appear in BOTH training and test sets.",
      why_it_matters = "The model has already seen these test points during training -- metrics are meaningless for those rows.",
      how_to_fix = "Ensure train_idx and test_idx have no overlap. Use borg_cv() to generate proper splits.",
      inflation_est = "Severe: metrics reflect memorization, not generalization."
    ),
    target_leakage = list(
      plain_english = "A predictor is suspiciously correlated with the target (possible target leakage).",
      why_it_matters = "If a feature encodes the outcome (directly or via a proxy), the model cheats.",
      how_to_fix = "Remove the flagged variable and verify it was measured BEFORE the outcome.",
      inflation_est = "Can inflate R2 by 50-90% -- often looks like a perfect model."
    ),
    preprocessing_leak = list(
      plain_english = "Preprocessing (normalization, PCA, imputation) was fitted on data including test observations.",
      why_it_matters = "Test-set statistics leaked into the preprocessing, making train/test less independent.",
      how_to_fix = "Use borg_assimilate() to rewrite preprocessing, or use recipes::recipe() inside CV folds.",
      inflation_est = "Usually 2-10% inflation, but can be larger with PCA or target encoding."
    ),
    spatial_proximity = list(
      plain_english = "{n} test points are within the autocorrelation range of training points.",
      why_it_matters = "Spatial neighbors in train/test sets share information even if indices don't overlap.",
      how_to_fix = "Increase the spatial buffer distance or use borg_cv() with spatial blocking.",
      inflation_est = "5-25% depending on spatial range relative to extent."
    ),
    sdm_random_partition = list(
      plain_english = "Species distribution model uses random spatial partitioning.",
      why_it_matters = "Random partitions ignore spatial autocorrelation in species occurrence data.",
      how_to_fix = "Use borg_to_enmeval() or borg_to_biomod2() for spatially structured partitions.",
      inflation_est = "SDM studies commonly show 15-30% AUC inflation with random partitions."
    ),
    nested_cv_leak = list(
      plain_english = "Inner and outer CV folds share observations.",
      why_it_matters = "Hyperparameter tuning sees test-fold data, invalidating the outer evaluation.",
      how_to_fix = "Ensure inner folds are strict subsets of the outer training fold.",
      inflation_est = "Depends on hyperparameter sensitivity; can be subtle but systematic."
    ),
    perfect_separation = list(
      plain_english = "A categorical predictor perfectly separates the target classes.",
      why_it_matters = "This usually indicates an ID column, date stamp, or other data artifact.",
      how_to_fix = "Remove the flagged variable -- it encodes the outcome, not a real predictor.",
      inflation_est = "Near-100% accuracy with no real predictive power."
    )
  )
}


# ---------------------------------------------------------------------------
# Internal: formatting helpers
# ---------------------------------------------------------------------------

#' @noRd
.severity_label <- function(severity) {
  switch(severity,
    hard_inflation = "CRITICAL",
    soft_inflation = "WARNING",
    possible_inflation = "NOTE",
    info = "INFO",
    toupper(severity)
  )
}

#' @noRd
.format_explanations <- function(explanations, style = "console") {
  if (style == "markdown") {
    lines <- c("## Risk Explanation & Action Plan\n")
    for (i in seq_along(explanations)) {
      e <- explanations[[i]]
      lines <- c(lines,
        sprintf("### %d. [%s] %s\n", i, e$severity_label, e$risk_type),
        sprintf("**What:** %s\n", e$plain_english),
        sprintf("**Why it matters:** %s\n", e$why_it_matters),
        sprintf("**Fix:** %s\n", e$how_to_fix),
        sprintf("**Expected inflation:** %s\n", e$inflation_est),
        ""
      )
    }
    return(lines)
  }

  # Console style
  lines <- c("", sprintf("  BORG Risk Explanation (%d issues)", length(explanations)),
             paste(rep("=", 50), collapse = ""), "")
  for (i in seq_along(explanations)) {
    e <- explanations[[i]]
    prefix <- switch(e$severity_label,
      CRITICAL = "!!",
      WARNING  = "! ",
      NOTE     = "? ",
      "  "
    )
    lines <- c(lines,
      sprintf("  %s %d. [%s] %s", prefix, i, e$severity_label, e$risk_type),
      sprintf("     What: %s", e$plain_english),
      sprintf("     Why:  %s", e$why_it_matters),
      sprintf("     Fix:  %s", e$how_to_fix),
      sprintf("     Inflation: %s", e$inflation_est),
      ""
    )
  }
  lines
}
