# ===========================================================================
# Tests for borg_power()
# ===========================================================================

test_that("borg_power returns correct structure", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
  )

  result <- borg_power(data, groups = "site", target = "value")

  expect_s3_class(result, "borg_power")
  expect_true(is.numeric(result$n_actual))
  expect_true(is.numeric(result$n_effective))
  expect_true(is.numeric(result$design_effect))
  expect_true(is.numeric(result$min_detectable_effect))
  expect_true(is.logical(result$sufficient))
  expect_true(is.character(result$recommendation))
  expect_s4_class(result$diagnosis, "BorgDiagnosis")
})


test_that("borg_power detects power loss from clustering", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:10, each = 20),
    value = rep(rnorm(10, sd = 5), each = 20) + rnorm(200, sd = 0.5)
  )

  result <- borg_power(data, groups = "site", target = "value")

  # With strong clustering, DEFF > 1 and n_effective < n_actual
  expect_gt(result$design_effect, 1)
  expect_lt(result$n_effective, result$n_actual)
  # MDE should be larger with blocking
  expect_gt(result$min_detectable_effect, result$min_detectable_effect_random)
})


test_that("borg_power with no dependencies returns DEFF ~1", {
  set.seed(42)
  data <- data.frame(
    x = rnorm(200),
    y = rnorm(200)
  )

  result <- borg_power(data, target = "y")

  expect_lte(result$design_effect, 1.1)
  expect_gte(result$n_effective, result$n_actual * 0.9)
})


test_that("borg_power accepts pre-computed diagnosis", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
  )

  diag <- borg_diagnose(data, groups = "site", target = "value")
  result <- borg_power(data, diagnosis = diag)

  expect_s3_class(result, "borg_power")
  expect_identical(result$diagnosis, diag)
})


test_that("borg_power computes power with effect_size", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
  )

  result <- borg_power(data, groups = "site", target = "value",
                        effect_size = 0.5)

  expect_false(is.na(result$power_loss))
  expect_gte(result$power_random, result$power_blocked)
})


test_that("borg_power prints without error", {
  set.seed(42)
  data <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
  )

  result <- borg_power(data, groups = "site", target = "value")
  expect_output(print(result), "BORG Power Analysis")
  expect_output(print(result), "Design effect")
})


test_that("borg_power validates inputs", {
  expect_error(borg_power("not a data frame"), "'data' must be a data frame")
  expect_error(
    borg_power(data.frame(x = 1:10), diagnosis = "not a diagnosis"),
    "'diagnosis' must be a BorgDiagnosis object"
  )
})
