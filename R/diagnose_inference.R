# Severity, CV strategy, and inflation inference for borg_diagnose()

#' Determine Overall Severity (Internal)
#' @noRd
determine_severity <- function(spatial, temporal, clustered) {
  severities <- c()

  # Spatial severity

  if (spatial$detected) {
    if (!is.na(spatial$morans_i) && spatial$morans_i > 0.5) {
      severities <- c(severities, "severe")
    } else if (!is.na(spatial$morans_i) && spatial$morans_i > 0.2) {
      severities <- c(severities, "moderate")
    } else if (spatial$detected) {
      severities <- c(severities, "moderate")
    }
  }

  # Temporal severity
  if (temporal$detected) {
    if (!is.na(temporal$acf_lag1) && abs(temporal$acf_lag1) > 0.7) {
      severities <- c(severities, "severe")
    } else if (!is.na(temporal$acf_lag1) && abs(temporal$acf_lag1) > 0.4) {
      severities <- c(severities, "moderate")
    } else if (temporal$detected) {
      severities <- c(severities, "moderate")
    }
  }

  # Clustered severity
  if (clustered$detected) {
    if (!is.na(clustered$icc) && clustered$icc > 0.3) {
      severities <- c(severities, "severe")
    } else if (!is.na(clustered$icc) && clustered$icc > 0.1) {
      severities <- c(severities, "moderate")
    } else if (clustered$detected) {
      severities <- c(severities, "moderate")
    }
  }

  if (length(severities) == 0) return("none")
  if ("severe" %in% severities) return("severe")
  if ("moderate" %in% severities) return("moderate")
  "none"
}


#' Recommend CV Strategy (Internal)
#' @noRd
recommend_cv_strategy <- function(spatial, temporal, clustered) {
  has_spatial <- spatial$detected
  has_temporal <- temporal$detected
  has_clustered <- clustered$detected

  # Priority order for combinations
  if (has_spatial && has_temporal) {
    return("spatial_temporal")
  }
  if (has_spatial && has_clustered) {
    return("spatial_group")
  }
  if (has_temporal && has_clustered) {
    return("temporal_group")
  }
  if (has_spatial) {
    return("spatial_block")
  }
  if (has_temporal) {
    return("temporal_block")
  }
  if (has_clustered) {
    return("group_fold")
  }

  "random"
}


#' Estimate Inflation from Random CV (Internal)
#' @noRd
estimate_inflation <- function(spatial, temporal, clustered, n) {
  # Base inflation estimates from literature
  # Roberts et al. (2017), Valavi et al. (2019), Meyer et al. (2019)

  inflation_factors <- c()
  confidence <- "high"

  if (spatial$detected) {
    if (!is.na(spatial$effective_n)) {
      # Inflation proportional to effective sample size reduction
      eff_ratio <- spatial$effective_n / n
      spatial_inflation <- max(0, 1 - eff_ratio) * 0.5  # Up to 50% from spatial
      inflation_factors <- c(inflation_factors, spatial_inflation)
    } else if (!is.na(spatial$morans_i)) {
      # Rough estimate from Moran's I
      spatial_inflation <- spatial$morans_i * 0.4
      inflation_factors <- c(inflation_factors, spatial_inflation)
      confidence <- "medium"
    }
  }

  if (temporal$detected) {
    if (!is.na(temporal$acf_lag1)) {
      # ACF-based inflation estimate
      temporal_inflation <- abs(temporal$acf_lag1) * 0.35
      inflation_factors <- c(inflation_factors, temporal_inflation)
    }
  }

  if (clustered$detected) {
    if (!is.na(clustered$design_effect)) {
      # DEFF-based inflation
      deff <- clustered$design_effect
      clustered_inflation <- max(0, (deff - 1) / deff) * 0.4
      inflation_factors <- c(inflation_factors, clustered_inflation)
    } else if (!is.na(clustered$icc)) {
      clustered_inflation <- clustered$icc * 0.3
      inflation_factors <- c(inflation_factors, clustered_inflation)
      confidence <- "medium"
    }
  }

  if (length(inflation_factors) == 0) {
    return(list(
      auc_inflation = NA_real_,
      rmse_deflation = NA_real_,
      confidence = "high",
      basis = "no_dependency_detected"
    ))
  }

  # Combine inflation factors (not simply additive)
  # Use max + fraction of others
  total_inflation <- max(inflation_factors) +
    sum(inflation_factors[-which.max(inflation_factors)]) * 0.5

  total_inflation <- min(total_inflation, 0.6)  # Cap at 60%

  list(
    auc_inflation = total_inflation,
    rmse_deflation = total_inflation * 0.8,  # RMSE typically less affected
    confidence = confidence,
    basis = paste(c(
      if (spatial$detected) "spatial_autocorrelation",
      if (temporal$detected) "temporal_autocorrelation",
      if (clustered$detected) "clustered_structure"
    ), collapse = "+")
  )
}
