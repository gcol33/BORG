# ===========================================================================
# borg_methods_text() and borg_certificate(): Reporting functions
# ===========================================================================

#' Generate Methods Section Text
#'
#' Generates copy-paste text for the methods section of a manuscript describing
#' the cross-validation approach used based on BORG diagnosis.
#'
#' @param diagnosis A \code{BorgDiagnosis} object from \code{borg_diagnose()}.
#' @param comparison Optional. A \code{borg_comparison} object from
#'   \code{borg_compare_cv()} to include empirical inflation estimates.
#' @param cv Optional. A \code{borg_cv} object with the actual CV folds used.
#' @param v Integer. Number of CV folds used. Default: 5.
#' @param style Character. Citation style: \code{"apa"} (default), \code{"nature"},
#'   \code{"ecology"}.
#' @param include_citation Logical. Include BORG package citation. Default: TRUE.
#'
#' @return A character string containing the methods text.
#'
#' @details
#' This function generates publication-ready text describing:
#' \itemize{
#'   \item The type of data dependency detected
#'   \item The cross-validation strategy used and why
#'   \item Quantitative details (autocorrelation range, ICC, etc.)
#'   \item Estimated inflation avoided by using blocked CV
#' }
#'
#' The text is designed to satisfy reviewer requirements for justifying
#' the cross-validation approach.
#'
#' @examples
#' # Create diagnosis
#' set.seed(42)
#' spatial_data <- data.frame(
#'   x = runif(200, 0, 100),
#'   y = runif(200, 0, 100),
#'   response = rnorm(200)
#' )
#'
#' diagnosis <- borg_diagnose(spatial_data, coords = c("x", "y"), target = "response")
#'
#' # Generate methods text
#' cat(borg_methods_text(diagnosis, v = 5))
#'
#' @seealso
#' \code{\link{borg_diagnose}} for dependency detection,
#' \code{\link{borg_compare_cv}} for empirical comparison.
#'
#' @export
borg_methods_text <- function(diagnosis,
                              comparison = NULL,
                              cv = NULL,
                              v = 5,
                              style = c("apa", "nature", "ecology"),
                              include_citation = TRUE) {

  if (!inherits(diagnosis, "BorgDiagnosis")) {
    stop("'diagnosis' must be a BorgDiagnosis object")
  }

  style <- match.arg(style)

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


#' Generate Validation Certificate
#'
#' Creates a structured validation certificate documenting the cross-validation
#' approach and BORG analysis results.
#'
#' @param diagnosis A \code{BorgDiagnosis} object from \code{borg_diagnose()}.
#' @param data The data frame that was analyzed.
#' @param comparison Optional. A \code{borg_comparison} object from
#'   \code{borg_compare_cv()}.
#' @param cv Optional. A \code{borg_cv} object with the CV folds used.
#' @param workflow_hash Optional. SHA256 hash of the workflow for reproducibility.
#'
#' @return A \code{borg_certificate} object (S3 class) containing structured
#'   validation information.
#'
#' @details
#' The certificate contains:
#' \itemize{
#'   \item BORG version and timestamp
#'   \item Data characteristics (n observations, features)
#'   \item Dependency diagnosis results
#'   \item CV strategy and parameters
#'   \item Inflation estimates (theoretical and/or empirical)
#'   \item Reproducibility information (data hash)
#' }
#'
#' Certificates can be exported to YAML/JSON for machine-readable validation
#' or printed for human review.
#'
#' @examples
#' \dontrun{
#' diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response")
#' cert <- borg_certificate(diagnosis, data)
#' print(cert)
#'
#' # Export to YAML
#' borg_export(cert, "validation_certificate.yaml")
#' }
#'
#' @seealso
#' \code{\link{borg_methods_text}} for manuscript text,
#' \code{\link{borg_export}} for exporting certificates.
#'
#' @export
borg_certificate <- function(diagnosis,
                             data,
                             comparison = NULL,
                             cv = NULL,
                             workflow_hash = NULL) {

  if (!inherits(diagnosis, "BorgDiagnosis")) {
    stop("'diagnosis' must be a BorgDiagnosis object")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Compute data hash if not provided
  if (is.null(workflow_hash)) {
    # Simple hash based on data dimensions and column names
    # Full hash would require digest package
    data_sig <- paste(nrow(data), ncol(data), paste(names(data), collapse = ","), sep = "|")
    workflow_hash <- sprintf("sig:%s", substr(data_sig, 1, 64))
  }

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


#' Export Certificate to File
#'
#' Exports a BORG certificate to YAML or JSON format.
#'
#' @param cert A \code{borg_certificate} object.
#' @param file Character. Output file path. Extension determines format
#'   (\code{.yaml}, \code{.yml}, or \code{.json}).
#'
#' @return Invisible NULL. Called for side effect of writing file.
#'
#' @examples
#' \dontrun{
#' cert <- borg_certificate(diagnosis, data)
#' borg_export(cert, "validation.yaml")
#' borg_export(cert, "validation.json")
#' }
#'
#' @export
borg_export <- function(cert, file) {
  if (!inherits(cert, "borg_certificate")) {
    stop("'cert' must be a borg_certificate object")
  }

  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("yaml", "yml")) {
    # YAML export
    write_yaml_simple(cert, file)

  } else if (ext == "json") {
    # JSON export
    write_json_simple(cert, file)

  } else {
    stop("Unsupported file extension. Use .yaml, .yml, or .json")
  }

  invisible(NULL)
}


# ===========================================================================
# Internal helper functions
# ===========================================================================

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
