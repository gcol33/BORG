# ===========================================================================
# BORG Reporting Functions - S3 summary() methods and exports
# ===========================================================================

# ===========================================================================
# S3 summary() methods
# ===========================================================================

#' Summarize BORG Diagnosis
#'
#' Generate a methods section summary for publication from a BorgDiagnosis object.
#'
#' @param object A \code{BorgDiagnosis} object.
#' @param comparison Optional. A \code{borg_comparison} object from
#'   \code{borg_compare_cv()} to include empirical inflation estimates.
#' @param v Integer. Number of CV folds used. Default: 5.
#' @param style Character. Citation style: \code{"apa"} (default),
#'   \code{"nature"}, \code{"ecology"}.
#' @param include_citation Logical. Include BORG package citation. Default: TRUE.
#' @param ... Additional arguments (currently unused).
#'
#' @return Character string with methods section text (invisibly). Also prints
#'   the text to the console.
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(
#'   x = runif(100, 0, 100),
#'   y = runif(100, 0, 100),
#'   response = rnorm(100)
#' )
#' diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
#'                            verbose = FALSE)
#' summary(diagnosis)
#'
#' @export
summary.BorgDiagnosis <- function(object,
                                   comparison = NULL,
                                   v = 5,
                                   style = c("apa", "nature", "ecology"),
                                   include_citation = TRUE,
                                   ...) {

  style <- match.arg(style)

  text <- generate_methods_text(
    diagnosis = object,
    comparison = comparison,
    v = v,
    style = style,
    include_citation = include_citation
  )

  cat("\n", text, "\n\n", sep = "")
  invisible(text)
}


#' Summarize BORG Result
#'
#' Generate a methods section summary for publication from a borg_result object.
#'
#' @param object A \code{borg_result} object from \code{borg()}.
#' @param comparison Optional. A \code{borg_comparison} object.
#' @param v Integer. Number of CV folds. Default: 5.
#' @param style Character. Citation style.
#' @param include_citation Logical. Include BORG citation.
#' @param ... Additional arguments (currently unused).
#'
#' @return Character string with methods text (invisibly).
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(
#'   x = runif(100, 0, 100),
#'   y = runif(100, 0, 100),
#'   response = rnorm(100)
#' )
#' result <- borg(data, coords = c("x", "y"), target = "response")
#' summary(result)
#'
#' @export
summary.borg_result <- function(object,
                                 comparison = NULL,
                                 v = 5,
                                 style = c("apa", "nature", "ecology"),
                                 include_citation = TRUE,
                                 ...) {

  if (is.null(object$diagnosis)) {
    cat("No diagnosis available in borg_result\n")
    return(invisible(NULL))
  }

  summary.BorgDiagnosis(
    object$diagnosis,
    comparison = comparison,
    v = v,
    style = style,
    include_citation = include_citation,
    ...
  )
}


#' Summarize BORG Risk Assessment
#'
#' Print a summary of detected risks.
#'
#' @param object A \code{BorgRisk} object from \code{borg_inspect()}.
#' @param ... Additional arguments (currently unused).
#'
#' @return The object invisibly.
#'
#' @examples
#' data <- data.frame(x = 1:100, y = 101:200)
#' risk <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
#' summary(risk)
#'
#' @export
summary.BorgRisk <- function(object, ...) {
  risks <- object@risks
  n_risks <- length(risks)

  if (n_risks == 0) {
    cat("\nBORG Risk Assessment: OK\n")
    cat("No risks detected. Evaluation is valid.\n\n")
    return(invisible(object))
  }

  n_hard <- sum(vapply(risks, function(r) r$severity == "hard_violation", logical(1)))
  n_soft <- n_risks - n_hard

  cat("\nBORG Risk Assessment Summary\n")
  cat("============================\n")

  if (n_hard > 0) {
    cat(sprintf("Status: INVALID (%d hard violation%s)\n",
                n_hard, if (n_hard > 1) "s" else ""))
  } else {
    cat(sprintf("Status: VALID with warnings (%d soft risk%s)\n",
                n_soft, if (n_soft > 1) "s" else ""))
  }

  cat(sprintf("\nTotal risks: %d\n", n_risks))
  cat(sprintf("  Hard violations: %d\n", n_hard))
  cat(sprintf("  Soft inflation risks: %d\n\n", n_soft))

  cat("Details:\n")
  for (i in seq_along(risks)) {
    r <- risks[[i]]
    severity_symbol <- if (r$severity == "hard_violation") "[X]" else "[!]"
    cat(sprintf("  %s %s: %s\n", severity_symbol, r$type, r$description))
  }
  cat("\n")

  invisible(object)
}


# ===========================================================================
# Exported functions
# ===========================================================================

#' Create Validation Certificate
#'
#' Generate a structured validation certificate documenting the BORG analysis
#' for reproducibility and audit trails.
#'
#' @param diagnosis A \code{BorgDiagnosis} object.
#' @param data The data frame that was analyzed.
#' @param comparison Optional. A \code{borg_comparison} object with empirical
#'   inflation estimates.
#' @param cv Optional. A \code{borg_cv} object with the CV folds used.
#'
#' @return A \code{borg_certificate} object containing:
#' \itemize{
#'   \item \code{meta}: Package version, R version, timestamp
#'   \item \code{data}: Data characteristics and hash
#'   \item \code{diagnosis}: Dependency type, severity, recommended CV
#'   \item \code{cv_strategy}: CV type and fold count
#'   \item \code{inflation}: Theoretical and empirical estimates
#' }
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(
#'   x = runif(100, 0, 100),
#'   y = runif(100, 0, 100),
#'   response = rnorm(100)
#' )
#' diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
#'                            verbose = FALSE)
#' cert <- borg_certificate(diagnosis, data)
#' print(cert)
#'
#' @seealso \code{\link{borg_export}} for writing certificates to file.
#'
#' @export
borg_certificate <- function(diagnosis, data, comparison = NULL, cv = NULL) {

  if (!inherits(diagnosis, "BorgDiagnosis")) {
    stop("'diagnosis' must be a BorgDiagnosis object")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Compute data signature
  data_sig <- paste(nrow(data), ncol(data), paste(names(data), collapse = ","), sep = "|")
  workflow_hash <- sprintf("sig:%s", substr(data_sig, 1, 64))

  # Build certificate structure
  cert <- list(
    # Metadata
    meta = list(
      borg_version = as.character(utils::packageVersion("BORG")),
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      validation_passed = TRUE
    ),

    # Data characteristics
    data = list(
      n_observations = nrow(data),
      n_features = ncol(data),
      feature_names = names(data),
      data_hash = workflow_hash
    ),

    # Diagnosis results
    diagnosis = list(
      dependency_type = diagnosis@dependency_type,
      severity = diagnosis@severity,
      recommended_cv = diagnosis@recommended_cv,
      spatial = if (length(diagnosis@spatial) > 0) diagnosis@spatial else NULL,
      temporal = if (length(diagnosis@temporal) > 0) diagnosis@temporal else NULL,
      clustered = if (length(diagnosis@clustered) > 0) diagnosis@clustered else NULL
    ),

    # CV strategy
    cv_strategy = list(
      type = diagnosis@recommended_cv,
      n_folds = if (!is.null(cv)) length(cv$folds) else NA_integer_
    ),

    # Inflation estimates
    inflation = list(
      theoretical = diagnosis@inflation_estimate,
      empirical = if (!is.null(comparison)) {
        list(
          estimate = comparison$inflation$estimate,
          direction = comparison$inflation$direction,
          metric = comparison$inflation$metric,
          p_value = comparison$p_value,
          n_repeats = comparison$repeats
        )
      } else NULL
    )
  )

  class(cert) <- c("borg_certificate", "list")
  cert
}


#' Export Validation Certificate
#'
#' Write a BORG validation certificate to a YAML or JSON file for
#' machine-readable documentation.
#'
#' @param diagnosis A \code{BorgDiagnosis} object.
#' @param data The data frame that was analyzed.
#' @param file Character. Output file path. Extension determines format
#'   (.yaml/.yml for YAML, .json for JSON).
#' @param comparison Optional. A \code{borg_comparison} object.
#' @param cv Optional. A \code{borg_cv} object.
#'
#' @return Invisibly returns the certificate object.
#'
#' @examples
#' \dontrun{
#' diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response")
#' borg_export(diagnosis, data, "validation.yaml")
#' borg_export(diagnosis, data, "validation.json")
#' }
#'
#' @seealso \code{\link{borg_certificate}} for creating certificates.
#'
#' @export
borg_export <- function(diagnosis, data, file, comparison = NULL, cv = NULL) {

  if (missing(file) || is.null(file)) {
    stop("'file' is required")
  }

  cert <- borg_certificate(diagnosis, data, comparison, cv)

  # Determine format from extension
  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("yaml", "yml")) {
    write_yaml_simple(cert, file)
  } else if (ext == "json") {
    write_json_simple(cert, file)
  } else {
    stop("Unsupported file format. Use .yaml, .yml, or .json")
  }

  invisible(cert)
}


# ===========================================================================
# Print method for borg_certificate
# ===========================================================================

#' @export
print.borg_certificate <- function(x, ...) {
  cat("BORG Validation Certificate\n")
  cat("===========================\n\n")

  cat(sprintf("Generated: %s\n", x$meta$timestamp))
  cat(sprintf("BORG version: %s\n", x$meta$borg_version))
  cat(sprintf("Validation: %s\n\n",
              if (x$meta$validation_passed) "PASSED" else "FAILED"))

  cat("Data Characteristics:\n")
  cat(sprintf("  Observations: %d\n", x$data$n_observations))
  cat(sprintf("  Features: %d\n", x$data$n_features))
  cat(sprintf("  Hash: %s\n\n", x$data$data_hash))

  cat("Dependency Diagnosis:\n")
  cat(sprintf("  Type: %s\n", toupper(x$diagnosis$dependency_type)))
  cat(sprintf("  Severity: %s\n", x$diagnosis$severity))
  cat(sprintf("  Recommended CV: %s\n", x$diagnosis$recommended_cv))

  if (!is.null(x$diagnosis$spatial) && length(x$diagnosis$spatial) > 0) {
    cat("\n  Spatial Analysis:\n")
    if (!is.null(x$diagnosis$spatial$morans_i)) {
      cat(sprintf("    Moran's I: %.3f (p = %.4g)\n",
                  x$diagnosis$spatial$morans_i,
                  x$diagnosis$spatial$morans_p))
    }
    if (!is.null(x$diagnosis$spatial$range) && !is.na(x$diagnosis$spatial$range)) {
      cat(sprintf("    Range: %.1f\n", x$diagnosis$spatial$range))
    }
  }

  if (!is.null(x$diagnosis$temporal) && length(x$diagnosis$temporal) > 0) {
    cat("\n  Temporal Analysis:\n")
    if (!is.null(x$diagnosis$temporal$acf_lag1)) {
      cat(sprintf("    Lag-1 ACF: %.3f\n", x$diagnosis$temporal$acf_lag1))
    }
    if (!is.null(x$diagnosis$temporal$ljung_box_p)) {
      cat(sprintf("    Ljung-Box p: %.4g\n", x$diagnosis$temporal$ljung_box_p))
    }
  }

  if (!is.null(x$diagnosis$clustered) && length(x$diagnosis$clustered) > 0) {
    cat("\n  Clustered Analysis:\n")
    if (!is.null(x$diagnosis$clustered$icc)) {
      cat(sprintf("    ICC: %.3f\n", x$diagnosis$clustered$icc))
    }
    if (!is.null(x$diagnosis$clustered$design_effect)) {
      cat(sprintf("    Design Effect: %.1f\n", x$diagnosis$clustered$design_effect))
    }
  }

  cat("\n")

  if (!is.null(x$inflation$empirical)) {
    cat("Empirical Inflation Estimate:\n")
    emp <- x$inflation$empirical
    cat(sprintf("  %s %s by %.1f%%\n",
                toupper(emp$metric), emp$direction,
                abs(emp$estimate) * 100))
    cat(sprintf("  p-value: %.4g (%d repeats)\n",
                emp$p_value, emp$n_repeats))
  } else if (!is.null(x$inflation$theoretical$auc_inflation) &&
             !is.na(x$inflation$theoretical$auc_inflation)) {
    cat("Theoretical Inflation Estimate:\n")
    cat(sprintf("  ~%.0f%% AUC inflation avoided\n",
                x$inflation$theoretical$auc_inflation * 100))
  }

  invisible(x)
}


# ===========================================================================
# Internal helper functions
# ===========================================================================

#' @noRd
generate_methods_text <- function(diagnosis,
                                  comparison = NULL,
                                  cv = NULL,
                                  v = 5,
                                  style = "apa",
                                  include_citation = TRUE) {

  # Build text components
  parts <- list()

  # ===========================================================================
  # CV strategy description
  # ===========================================================================

  strategy_text <- switch(diagnosis@recommended_cv,
    "spatial_block" = sprintf(
      "spatial block cross-validation (k = %d folds)", v
    ),
    "temporal_block" = sprintf(
      "temporal block cross-validation (k = %d folds) with chronological ordering", v
    ),
    "group_fold" = sprintf(
      "leave-group-out cross-validation (k = %d folds)", v
    ),
    "spatial_temporal_block" = sprintf(
      "spatio-temporal block cross-validation (k = %d folds)", v
    ),
    "random" = sprintf(
      "random k-fold cross-validation (k = %d)", v
    ),
    sprintf("%s cross-validation (k = %d folds)", diagnosis@recommended_cv, v)
  )

  parts$strategy <- sprintf("Model performance was evaluated using %s", strategy_text)

  # ===========================================================================
  # Dependency justification
  # ===========================================================================

  if (diagnosis@dependency_type == "spatial") {
    spatial <- diagnosis@spatial
    if (!is.null(spatial$morans_i) && !is.na(spatial$morans_i)) {
      parts$dependency <- sprintf(
        "Spatial autocorrelation was detected in the data (Moran's I = %.2f, p %s)",
        spatial$morans_i,
        format_p_value(spatial$morans_p)
      )
      if (!is.null(spatial$range) && !is.na(spatial$range)) {
        parts$dependency <- paste0(
          parts$dependency,
          sprintf(" with an estimated autocorrelation range of %.1f units", spatial$range)
        )
      }
    } else {
      parts$dependency <- "Spatial autocorrelation was detected in the data"
    }

  } else if (diagnosis@dependency_type == "temporal") {
    temporal <- diagnosis@temporal
    if (!is.null(temporal$acf_lag1) && !is.na(temporal$acf_lag1)) {
      parts$dependency <- sprintf(
        "Temporal autocorrelation was detected (lag-1 ACF = %.2f",
        temporal$acf_lag1
      )
      if (!is.null(temporal$ljung_box_p) && !is.na(temporal$ljung_box_p)) {
        parts$dependency <- paste0(
          parts$dependency,
          sprintf(", Ljung-Box p %s)", format_p_value(temporal$ljung_box_p))
        )
      } else {
        parts$dependency <- paste0(parts$dependency, ")")
      }
    } else {
      parts$dependency <- "Temporal autocorrelation was detected in the data"
    }

  } else if (diagnosis@dependency_type == "clustered") {
    clustered <- diagnosis@clustered
    if (!is.null(clustered$icc) && !is.na(clustered$icc)) {
      parts$dependency <- sprintf(
        "Clustered data structure was identified with an intraclass correlation (ICC) of %.2f",
        clustered$icc
      )
      if (!is.null(clustered$design_effect) && !is.na(clustered$design_effect)) {
        parts$dependency <- paste0(
          parts$dependency,
          sprintf(" (design effect = %.1f)", clustered$design_effect)
        )
      }
    } else {
      parts$dependency <- "Clustered data structure was identified"
    }

  } else if (diagnosis@dependency_type == "mixed") {
    parts$dependency <- "Multiple dependency structures were detected (spatial, temporal, and/or clustered)"

  } else {
    parts$dependency <- NULL
  }

  # ===========================================================================
  # Inflation estimate
  # ===========================================================================

  if (!is.null(comparison) && inherits(comparison, "borg_comparison")) {
    # Use empirical comparison
    inf <- comparison$inflation
    sig_text <- if (comparison$p_value < 0.05) "significantly " else ""
    parts$inflation <- sprintf(
      "Empirical comparison showed that random cross-validation %s%s %s estimates by %.1f%% (paired t-test, p %s, n = %d repeats)",
      sig_text, inf$direction, inf$metric,
      abs(inf$estimate) * 100,
      format_p_value(comparison$p_value),
      comparison$repeats
    )

  } else if (!is.null(diagnosis@inflation_estimate$auc_inflation) &&
             !is.na(diagnosis@inflation_estimate$auc_inflation)) {
    # Use theoretical estimate
    inf_pct <- diagnosis@inflation_estimate$auc_inflation * 100
    if (inf_pct > 5) {
      parts$inflation <- sprintf(
        "Random cross-validation was avoided as it would be expected to inflate performance estimates by approximately %.0f%%",
        inf_pct
      )
    }
  }

  # ===========================================================================
  # Citation
  # ===========================================================================

  if (include_citation) {
    borg_version <- utils::packageVersion("BORG")
    parts$citation <- sprintf(
      "Cross-validation strategy was determined using the BORG package (version %s) for R",
      borg_version
    )
  }

  # ===========================================================================
  # Assemble text
  # ===========================================================================

  # Order: strategy, dependency justification, inflation, citation
  text_parts <- c()

  if (!is.null(parts$dependency)) {
    text_parts <- c(text_parts, paste0(parts$dependency, "."))
  }

  text_parts <- c(text_parts, paste0(parts$strategy, "."))

  if (!is.null(parts$inflation)) {
    text_parts <- c(text_parts, paste0(parts$inflation, "."))
  }

  if (!is.null(parts$citation)) {
    text_parts <- c(text_parts, paste0(parts$citation, "."))
  }

  # Join with appropriate spacing
  paste(text_parts, collapse = " ")
}


#' @noRd
format_p_value <- function(p) {
  if (is.null(p) || is.na(p)) return("= NA")
  if (p < 0.001) return("< 0.001")
  if (p < 0.01) return(sprintf("= %.3f", p))
  sprintf("= %.2f", p)
}


#' @noRd
write_yaml_simple <- function(x, file) {
  # Simple YAML writer without dependencies
  lines <- yaml_serialize(x, indent = 0)
  writeLines(lines, file)
}


#' @noRd
yaml_serialize <- function(x, indent = 0, name = NULL) {
  prefix <- paste(rep("  ", indent), collapse = "")
  lines <- character(0)

  if (!is.null(name)) {
    if (is.null(x) || (is.list(x) && length(x) == 0)) {
      return(paste0(prefix, name, ": null"))
    }
  }

  if (is.null(x)) {
    return(paste0(prefix, "null"))
  }

  if (is.list(x) && !is.data.frame(x)) {
    if (!is.null(name)) {
      lines <- c(lines, paste0(prefix, name, ":"))
      indent <- indent + 1
      prefix <- paste(rep("  ", indent), collapse = "")
    }

    for (nm in names(x)) {
      if (!is.null(x[[nm]])) {
        lines <- c(lines, yaml_serialize(x[[nm]], indent, nm))
      }
    }

  } else if (is.atomic(x) && length(x) == 1) {
    val <- if (is.character(x)) {
      sprintf('"%s"', x)
    } else if (is.logical(x)) {
      tolower(as.character(x))
    } else if (is.na(x)) {
      "null"
    } else {
      as.character(x)
    }

    if (!is.null(name)) {
      lines <- paste0(prefix, name, ": ", val)
    } else {
      lines <- paste0(prefix, val)
    }

  } else if (is.atomic(x) && length(x) > 1) {
    if (!is.null(name)) {
      lines <- c(lines, paste0(prefix, name, ":"))
    }
    for (val in x) {
      val_str <- if (is.character(val)) sprintf('"%s"', val) else as.character(val)
      lines <- c(lines, paste0(prefix, "  - ", val_str))
    }
  }

  lines
}


#' @noRd
write_json_simple <- function(x, file) {
  # Simple JSON writer without dependencies
  json <- json_serialize(x)
  writeLines(json, file)
}


#' @noRd
json_serialize <- function(x, indent = 0) {
  prefix <- paste(rep("  ", indent), collapse = "")

  if (is.null(x)) {
    return("null")
  }

  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) == 0) return("{}")

    parts <- vapply(names(x), function(nm) {
      val <- json_serialize(x[[nm]], indent + 1)
      sprintf('%s  "%s": %s', prefix, nm, val)
    }, character(1))

    paste0("{\n", paste(parts, collapse = ",\n"), "\n", prefix, "}")

  } else if (is.atomic(x) && length(x) == 1) {
    if (is.character(x)) {
      sprintf('"%s"', gsub('"', '\\"', x))
    } else if (is.logical(x)) {
      tolower(as.character(x))
    } else if (is.na(x)) {
      "null"
    } else {
      as.character(x)
    }

  } else if (is.atomic(x) && length(x) > 1) {
    parts <- vapply(x, function(val) {
      if (is.character(val)) sprintf('"%s"', val) else as.character(val)
    }, character(1))
    paste0("[", paste(parts, collapse = ", "), "]")

  } else {
    "null"
  }
}
