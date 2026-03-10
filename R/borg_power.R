# Statistical Power Analysis After Blocking
# Estimates power loss when switching from random to blocked CV

#' Estimate Statistical Power After Blocking
#'
#' Computes how much statistical power is lost when switching from random
#' to blocked cross-validation. Reports effective sample size, minimum
#' detectable effect size, and whether the dataset is large enough.
#'
#' @param data A data frame.
#' @param diagnosis A \code{\link{BorgDiagnosis}} object. If NULL, computed
#'   automatically from the other arguments.
#' @param coords Character vector of length 2 for spatial coordinates.
#' @param time Character string for the time column.
#' @param groups Character string for the grouping column.
#' @param target Character string for the response variable.
#' @param alpha Significance level. Default: 0.05.
#' @param power Target power. Default: 0.80.
#' @param effect_size Numeric. Expected effect size (Cohen's d for
#'   continuous, OR for binary). If NULL, reports minimum detectable
#'   effect size instead.
#' @param verbose Logical. Print progress messages. Default: FALSE.
#'
#' @return An object of class \code{"borg_power"} containing:
#' \describe{
#'   \item{n_actual}{Total number of observations}
#'   \item{n_effective}{Effective sample size after accounting for dependencies}
#'   \item{design_effect}{Variance inflation factor from dependencies}
#'   \item{power_random}{Statistical power under random CV}
#'   \item{power_blocked}{Statistical power under blocked CV}
#'   \item{power_loss}{Absolute power loss (power_random - power_blocked)}
#'   \item{min_detectable_effect}{Minimum detectable effect at target power}
#'   \item{min_detectable_effect_random}{Same, under random CV (for comparison)}
#'   \item{sufficient}{Logical. Is the dataset large enough at target power?}
#'   \item{recommendation}{Character. Human-readable recommendation.}
#'   \item{diagnosis}{The BorgDiagnosis used}
#' }
#'
#' @details
#' When data have spatial, temporal, or clustered dependencies, blocked CV
#' reduces the effective sample size. This function quantifies that reduction
#' using the design effect (DEFF):
#'
#' \deqn{n_{eff} = n / DEFF}
#'
#' The design effect is computed from:
#' \itemize{
#'   \item \strong{Spatial}: Moran's I and the ratio of autocorrelation range
#'     to study extent (Griffith, 2005)
#'   \item \strong{Temporal}: ACF lag-1 autocorrelation
#'     (\eqn{DEFF \approx (1 + \rho) / (1 - \rho)})
#'   \item \strong{Clustered}: ICC and mean cluster size
#'     (\eqn{DEFF = 1 + (m - 1) \times ICC})
#' }
#'
#' For mixed dependencies, design effects are combined multiplicatively.
#'
#' @examples
#' # Clustered data
#' clustered_data <- data.frame(
#'   site = rep(1:20, each = 10),
#'   value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
#' )
#'
#' pw <- borg_power(clustered_data, groups = "site", target = "value")
#' print(pw)
#'
#' @seealso \code{\link{borg_diagnose}}, \code{\link{borg_cv}}
#'
#' @export
borg_power <- function(data,
                       diagnosis = NULL,
                       coords = NULL,
                       time = NULL,
                       groups = NULL,
                       target = NULL,
                       alpha = 0.05,
                       power = 0.80,
                       effect_size = NULL,
                       verbose = FALSE) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  n <- nrow(data)

  # Run diagnosis if needed
  if (is.null(diagnosis)) {
    diagnosis <- borg_diagnose(data,
                                coords = coords,
                                time = time,
                                groups = groups,
                                target = target,
                                verbose = verbose)
  }

  if (!inherits(diagnosis, "BorgDiagnosis")) {
    stop("'diagnosis' must be a BorgDiagnosis object")
  }

  # Compute design effect from each dependency source
  deff_components <- list()

  # Spatial DEFF
  if (diagnosis@spatial$detected) {
    spatial <- diagnosis@spatial
    if (!is.na(spatial$effective_n) && spatial$effective_n > 0) {
      deff_spatial <- n / spatial$effective_n
    } else if (!is.na(spatial$morans_i)) {
      rho <- max(0, min(1, spatial$morans_i))
      deff_spatial <- (1 + rho) / (1 - rho)
    } else {
      deff_spatial <- 1
    }
    deff_components$spatial <- max(1, deff_spatial)
  }

  # Temporal DEFF
  if (diagnosis@temporal$detected) {
    temporal <- diagnosis@temporal
    if (!is.na(temporal$acf_lag1)) {
      rho <- max(0, min(0.99, abs(temporal$acf_lag1)))
      deff_temporal <- (1 + rho) / (1 - rho)
    } else {
      deff_temporal <- 1
    }
    deff_components$temporal <- max(1, deff_temporal)
  }

  # Clustered DEFF
  if (diagnosis@clustered$detected) {
    clustered <- diagnosis@clustered
    if (!is.na(clustered$design_effect)) {
      deff_components$clustered <- max(1, clustered$design_effect)
    } else if (!is.na(clustered$icc)) {
      m <- mean(clustered$cluster_sizes)
      deff_components$clustered <- max(1, 1 + (m - 1) * clustered$icc)
    }
  }

  # Combine design effects multiplicatively for mixed dependencies
  if (length(deff_components) == 0) {
    deff <- 1
  } else if (length(deff_components) == 1) {
    deff <- deff_components[[1]]
  } else {
    # Multiplicative combination (conservative)
    deff <- Reduce(`*`, deff_components)
    # Cap at reasonable maximum
    deff <- min(deff, n / 10)
  }

  n_eff <- n / deff

  # Power calculations using normal approximation
  # For a two-sample comparison with n_eff total
  z_alpha <- stats::qnorm(1 - alpha / 2)

  if (!is.null(effect_size)) {
    # Compute power for the given effect size
    power_random <- .compute_power(n, effect_size, alpha)
    power_blocked <- .compute_power(n_eff, effect_size, alpha)
    min_effect_blocked <- .min_detectable_effect(n_eff, alpha, power)
    min_effect_random <- .min_detectable_effect(n, alpha, power)
  } else {
    # Compute minimum detectable effect sizes
    power_random <- power  # By definition at MDE
    power_blocked <- power
    min_effect_blocked <- .min_detectable_effect(n_eff, alpha, power)
    min_effect_random <- .min_detectable_effect(n, alpha, power)
    effect_size <- NULL
  }

  # Is dataset sufficient?
  sufficient <- n_eff >= 20  # Minimum for meaningful inference

  # Generate recommendation
  recommendation <- .power_recommendation(
    n, n_eff, deff, power_random, power_blocked,
    min_effect_blocked, min_effect_random, diagnosis, sufficient
  )

  result <- list(
    n_actual = n,
    n_effective = n_eff,
    design_effect = deff,
    deff_components = deff_components,
    power_random = power_random,
    power_blocked = power_blocked,
    power_loss = if (!is.null(effect_size)) power_random - power_blocked else NA_real_,
    min_detectable_effect = min_effect_blocked,
    min_detectable_effect_random = min_effect_random,
    effect_size_ratio = min_effect_blocked / min_effect_random,
    alpha = alpha,
    target_power = power,
    sufficient = sufficient,
    recommendation = recommendation,
    diagnosis = diagnosis
  )
  class(result) <- c("borg_power", "list")
  result
}


#' Compute power for a given n, effect size, alpha (normal approximation)
#' @noRd
.compute_power <- function(n, d, alpha) {
  # Two-sample z-test approximation
  # n is total sample size, split evenly
  n_per_group <- n / 2
  z_alpha <- stats::qnorm(1 - alpha / 2)
  ncp <- d * sqrt(n_per_group / 2)  # Non-centrality parameter
  stats::pnorm(ncp - z_alpha) + stats::pnorm(-ncp - z_alpha)
}

#' Minimum detectable effect size at given power
#' @noRd
.min_detectable_effect <- function(n, alpha, power) {
  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_beta <- stats::qnorm(power)
  n_per_group <- max(n / 2, 1)
  (z_alpha + z_beta) / sqrt(n_per_group / 2)
}

#' Generate power recommendation text
#' @noRd
.power_recommendation <- function(n, n_eff, deff, power_random, power_blocked,
                                   min_effect_blocked, min_effect_random,
                                   diagnosis, sufficient) {
  lines <- character(0)

  if (deff <= 1.05) {
    lines <- c(lines, "No meaningful power loss from blocking. Random and blocked CV have equivalent power.")
    return(paste(lines, collapse = "\n"))
  }

  lines <- c(lines, sprintf(
    "Blocking reduces effective sample size from %d to %.0f (DEFF = %.1f).",
    n, n_eff, deff
  ))

  if (!is.na(power_random) && !is.na(power_blocked) && power_random != power_blocked) {
    lines <- c(lines, sprintf(
      "Power drops from %.0f%% (random CV) to %.0f%% (blocked CV) for the same effect size.",
      power_random * 100, power_blocked * 100
    ))
  }

  lines <- c(lines, sprintf(
    "Minimum detectable effect: d = %.2f (blocked) vs d = %.2f (random).",
    min_effect_blocked, min_effect_random
  ))

  if (!sufficient) {
    lines <- c(lines, "WARNING: Effective sample size < 20. Dataset may be too small for reliable inference with blocked CV.")
    lines <- c(lines, "Consider collecting more data or using a simpler blocking strategy.")
  } else if (deff > 5) {
    lines <- c(lines, "Large design effect. Consider whether a coarser blocking strategy could reduce power loss.")
  }

  # Blocking is still required
  lines <- c(lines, sprintf(
    "Despite power loss, blocked CV (%s) is required for valid inference.",
    diagnosis@recommended_cv
  ))

  paste(lines, collapse = "\n")
}


#' @export
print.borg_power <- function(x, ...) {
  cat("BORG Power Analysis\n")
  cat("====================\n\n")

  cat(sprintf("Sample size:      %d observations\n", x$n_actual))
  cat(sprintf("Effective size:   %.0f (after blocking)\n", x$n_effective))
  cat(sprintf("Design effect:    %.2f\n", x$design_effect))

  if (length(x$deff_components) > 0) {
    cat("  Components:\n")
    for (nm in names(x$deff_components)) {
      cat(sprintf("    %-12s  %.2f\n", nm, x$deff_components[[nm]]))
    }
  }

  cat(sprintf("\nTarget power:     %.0f%%\n", x$target_power * 100))
  cat(sprintf("Alpha:            %.3f\n", x$alpha))

  if (!is.na(x$power_loss)) {
    cat(sprintf("\nPower (random CV):  %.1f%%\n", x$power_random * 100))
    cat(sprintf("Power (blocked CV): %.1f%%\n", x$power_blocked * 100))
    cat(sprintf("Power loss:         %.1f%%\n", x$power_loss * 100))
  }

  cat(sprintf("\nMin detectable effect (blocked): d = %.3f\n", x$min_detectable_effect))
  cat(sprintf("Min detectable effect (random):  d = %.3f\n", x$min_detectable_effect_random))
  cat(sprintf("Effect size ratio:               %.2fx\n", x$effect_size_ratio))

  cat(sprintf("\nSufficient data:  %s\n", if (x$sufficient) "YES" else "NO"))

  cat(sprintf("\n%s\n", x$recommendation))

  invisible(x)
}
