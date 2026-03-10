# ===========================================================================
# Tests for borg_rewrite()
# ===========================================================================

test_that("borg_rewrite validates required arguments", {
  # Non-list workflow
  expect_error(
    borg_rewrite("not a list"),
    "'workflow' must be a list"
  )

  # Invalid risks object
  expect_error(
    borg_rewrite(list(data = data.frame(x = 1:10), train_idx = 1:5, test_idx = 6:10),
                 risks = "not a BorgRisk"),
    "'risks' must be a BorgRisk object"
  )
})


test_that("borg_rewrite returns correct structure", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  result <- borg_rewrite(workflow)

  expect_true(is.list(result))
  expect_true("workflow" %in% names(result))
  expect_true("fixed" %in% names(result))
  expect_true("unfixable" %in% names(result))
  expect_true("report" %in% names(result))

  expect_s4_class(result$report, "BorgRisk")
})


test_that("borg_rewrite passes through clean workflow", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  result <- borg_rewrite(workflow)

  expect_true(result$report@is_valid)
  expect_equal(length(result$fixed), 0)
  expect_equal(length(result$unfixable), 0)
})


test_that("borg_rewrite identifies unfixable index overlap", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:6,
    test_idx = 5:10  # overlap at 5, 6
  )

  result <- borg_rewrite(workflow)

  expect_false(result$report@is_valid)
  expect_true("index_overlap" %in% result$unfixable)
})


test_that("borg_rewrite identifies unfixable duplicate rows", {
  workflow <- list(
    data = data.frame(
      x = c(1, 2, 3, 4, 5, 1, 2, 3),  # rows 6-8 duplicate rows 1-3
      y = c(10, 20, 30, 40, 50, 10, 20, 30)
    ),
    train_idx = 1:5,
    test_idx = 6:8
  )

  result <- borg_rewrite(workflow)

  expect_true("duplicate_rows" %in% result$unfixable)
})


test_that("borg_rewrite identifies unfixable target leakage", {
  set.seed(42)
  n <- 100
  target <- rnorm(n)
  leaked_feature <- target + rnorm(n, sd = 0.001)

  workflow <- list(
    data = data.frame(
      y = target,
      x1 = rnorm(n),
      x2 = leaked_feature
    ),
    train_idx = 1:70,
    test_idx = 71:100,
    target_col = "y"
  )

  result <- borg_rewrite(workflow)

  expect_true("target_leakage_direct" %in% result$unfixable)
})


test_that("borg_rewrite accepts pre-computed risks", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:6,
    test_idx = 5:10  # overlap
  )

  # Pre-compute risks
  risks <- borg_validate(workflow)

  # Pass to rewrite

  result <- borg_rewrite(workflow, risks = risks)

  expect_true("index_overlap" %in% result$unfixable)
})


test_that("borg_rewrite fix parameter filters risk types", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100, mean = 10, sd = 5),
    x2 = rnorm(100, mean = 50, sd = 20)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # Create leaky preProcess
  pp_bad <- caret::preProcess(data, method = c("center", "scale"))

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pp_bad
  )

  # With fix = "all" (default), attempt to fix preprocessing
  result_all <- borg_rewrite(workflow, fix = "all")

  # With fix = "thresholds", skip preprocessing fix
  result_thresholds <- borg_rewrite(workflow, fix = "thresholds")

  expect_s4_class(result_all$report, "BorgRisk")
  expect_s4_class(result_thresholds$report, "BorgRisk")

  # fix = "all" should actually fix the preprocessing leak
  expect_true("preprocessing_leak" %in% result_all$fixed)
})


test_that("borg_rewrite fixes caret preProcess leak", {
  skip_if_not_installed("caret")

  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100, mean = 10, sd = 5),
    x2 = rnorm(100, mean = 50, sd = 20)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # Leaky: preProcess fitted on full data
  pp_bad <- caret::preProcess(data, method = c("center", "scale"))

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pp_bad
  )

  result <- borg_rewrite(workflow)

  # Should be fixed

  expect_true("preprocessing_leak" %in% result$fixed)
  expect_false("preprocessing_leak" %in% result$unfixable)

  # The rewritten preProcess should be different from the original
  new_pp <- result$workflow$preprocess
  expect_s3_class(new_pp, "preProcess")

  # New preProcess should be based on train data stats
  train_data <- data[train_idx, ]
  expect_equal(new_pp$mean[["x1"]], mean(train_data$x1), tolerance = 1e-10)
  expect_equal(new_pp$mean[["x2"]], mean(train_data$x2), tolerance = 1e-10)
})


test_that("borg_rewrite fixes PCA leak (base R prcomp)", {
  set.seed(42)
  data <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # Leaky: PCA on full data
  pca_bad <- prcomp(data, center = TRUE, scale. = TRUE)

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pca_bad
  )

  result <- borg_rewrite(workflow)

  expect_true("preprocessing_leak" %in% result$fixed)

  # New PCA should use train-only stats
  new_pca <- result$workflow$preprocess
  expect_s3_class(new_pca, "prcomp")
  expect_equal(new_pca$center[["x1"]], mean(data$x1[train_idx]), tolerance = 1e-10)
})


test_that("borg_rewrite fixes threshold on test data", {
  set.seed(42)
  n <- 100
  data <- data.frame(
    x = rnorm(n),
    y = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  train_idx <- 1:70
  test_idx <- 71:100

  model <- glm(y ~ x, data = data[train_idx, ], family = binomial())

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    model = model,
    target_col = "y",
    thresholds = list(
      value = 0.5,
      optimized_on = "test",
      used_test_predictions = TRUE
    )
  )

  result <- borg_rewrite(workflow)

  expect_true("threshold_leak" %in% result$fixed)
  expect_equal(result$workflow$thresholds$optimized_on, "train")
  expect_false(result$workflow$thresholds$used_test_predictions)
})


test_that("borg_rewrite re-validates after rewriting", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  result <- borg_rewrite(workflow)

  # The report should reflect the state after any rewrites
  expect_s4_class(result$report, "BorgRisk")
  expect_true(inherits(result$report@timestamp, "POSIXct"))
})


test_that("borg_rewrite handles workflow with model", {
  # Simple lm model - should pass through without issues
  set.seed(42)
  data <- data.frame(x = 1:20, y = 2 * (1:20) + rnorm(20))
  train_idx <- 1:15
  test_idx <- 16:20

  model <- lm(y ~ x, data = data[train_idx, ])

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    model = model
  )

  result <- borg_rewrite(workflow)

  expect_true(is.list(result))
  expect_s4_class(result$report, "BorgRisk")
})


# ===========================================================================
# Edge-case tests for high-severity gaps
# ===========================================================================

test_that("borg_rewrite handles workflow with missing data gracefully", {
  # Exercise the path where .get_train_data returns NULL because both
 # data and train_idx are NULL. We construct a workflow that has the

  # required fields for borg_validate (post-rewrite) but where the rewriter
  # cannot extract training data.
  set.seed(42)
  data <- data.frame(x = 1:10, y = 11:20)

  # Create a workflow where train_idx is present (for borg_validate) but
  # where we simulate the rewriter encountering a risk it cannot fix.
  # We pre-compute a BorgRisk with a normalization_leak and pass a workflow
  # whose data is a scaled matrix without center/scale attributes and no
  # caret preProcess, so the normalization rewriter falls through to the
  # fallback path and returns success = FALSE.
  workflow <- list(
    data = data,
    train_idx = 1:5,
    test_idx = 6:10
  )

  # Construct a BorgRisk with normalization_leak
  fake_risk <- new("BorgRisk",
    risks = list(list(
      type = "normalization_leak",
      severity = "hard_violation",
      description = "Normalization fitted on full data",
      affected_indices = integer(0),
      source_object = "scale"
    )),
    n_hard = 1L,
    n_soft = 0L,
    is_valid = FALSE,
    train_indices = 1:5,
    test_indices = 6:10,
    timestamp = Sys.time(),
    call = quote(borg_validate(x))
  )

  # The normalization rewriter checks for preProcess or scaled matrix.
  # This workflow has neither, so it returns success=FALSE -> unfixable.
  expect_no_error(result <- borg_rewrite(workflow, risks = fake_risk))
  expect_true("normalization_leak" %in% result$unfixable)
  expect_s4_class(result$report, "BorgRisk")
})


test_that("borg_rewrite handles unknown risk type", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  # Create a BorgRisk with a fake/unknown risk type
  fake_risk <- new("BorgRisk",
    risks = list(list(
      type = "unknown_risk_xyz",
      severity = "hard_violation",
      description = "Some unknown risk type for testing",
      affected_indices = integer(0),
      source_object = "test"
    )),
    n_hard = 1L,
    n_soft = 0L,
    is_valid = FALSE,
    train_indices = 1:5,
    test_indices = 6:10,
    timestamp = Sys.time(),
    call = quote(borg_validate(x))
  )

  # borg_rewrite should not error on unknown risk types

  expect_no_error(result <- borg_rewrite(workflow, risks = fake_risk))

  # Unknown risk type is neither in rewritable_types nor unfixable_types,
  # so the switch returns NULL and it gets skipped entirely. It won't
  # appear in fixed or unfixable.
  expect_true(is.list(result))
  expect_s4_class(result$report, "BorgRisk")
  # The unknown type should NOT appear in fixed (it wasn't rewritten)
  expect_false("unknown_risk_xyz" %in% result$fixed)
})


test_that("borg_rewrite handles normalization with zero-variance column", {
  skip_if_not_installed("caret")

  set.seed(42)
  n <- 100
  data <- data.frame(
    x_normal = rnorm(n, mean = 10, sd = 5),
    x_constant = rep(42, n)  # zero variance
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # Leaky: preProcess fitted on full data (including zero-variance column)
  # caret warns about zero-variance columns — expected, not a BORG issue
  pp_bad <- suppressWarnings(
    caret::preProcess(data, method = c("center", "scale"))
  )

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    preprocess = pp_bad
  )

  # Should not error even though one column has zero variance
  # Suppress caret's zero-variance warnings from the rewriter re-fitting
  expect_no_error(result <- suppressWarnings(borg_rewrite(workflow)))
  expect_true(is.list(result))
  expect_s4_class(result$report, "BorgRisk")
})


test_that("borg_rewrite handles threshold optimization with identical predictions", {
  set.seed(42)
  n <- 100

  # Create data where model predictions will have near-zero variance.
  # Use a target that is nearly perfectly balanced and a feature with no

  # signal, so that glm predictions cluster around 0.5.
  data <- data.frame(
    x = rep(0, n),  # no signal at all
    y = factor(rep(c("A", "B"), each = n / 2))
  )
  train_idx <- 1:70
  test_idx <- 71:100

  # glm with a zero-variance predictor produces identical predictions
  # (intercept only). suppress the "fitted probabilities" warning.
  model <- suppressWarnings(
    glm(y ~ x, data = data[train_idx, ], family = binomial())
  )

  # Verify predictions are indeed constant
  train_preds <- suppressWarnings(
    predict(model, newdata = data[train_idx, ], type = "response")
  )
  expect_equal(length(unique(round(train_preds, 10))), 1)

  workflow <- list(
    data = data,
    train_idx = train_idx,
    test_idx = test_idx,
    model = model,
    target_col = "y",
    thresholds = list(
      value = 0.5,
      optimized_on = "test",
      used_test_predictions = TRUE
    )
  )

  # Create threshold_leak risk manually
  threshold_risk <- new("BorgRisk",
    risks = list(list(
      type = "threshold_leak",
      severity = "hard_violation",
      description = "Threshold optimized on test data",
      affected_indices = integer(0),
      source_object = "thresholds"
    )),
    n_hard = 1L,
    n_soft = 0L,
    is_valid = FALSE,
    train_indices = train_idx,
    test_indices = test_idx,
    timestamp = Sys.time(),
    call = quote(borg_validate(x))
  )

  # The threshold rewriter should handle identical predictions without error.
  # All predictions are ~0.5 (single unique value), so thresholds_to_try
  # has one element and the Youden's J loop runs exactly once.
  expect_no_error(
    result <- suppressWarnings(borg_rewrite(workflow, risks = threshold_risk))
  )
  expect_true(is.list(result))
  expect_s4_class(result$report, "BorgRisk")
  # With constant predictions, the rewriter should still succeed (it picks
  # a threshold; Youden's J is just 0 for all candidates)
  expect_true("threshold_leak" %in% result$fixed)
  expect_equal(result$workflow$thresholds$optimized_on, "train")
})


test_that("borg_rewrite handles empty risks (clean workflow)", {
  workflow <- list(
    data = data.frame(x = 1:10, y = 11:20),
    train_idx = 1:5,
    test_idx = 6:10
  )

  # Create a BorgRisk with zero risks (clean)
  clean_risk <- new("BorgRisk",
    risks = list(),
    n_hard = 0L,
    n_soft = 0L,
    is_valid = TRUE,
    train_indices = 1:5,
    test_indices = 6:10,
    timestamp = Sys.time(),
    call = quote(borg_validate(x))
  )

  expect_no_error(result <- borg_rewrite(workflow, risks = clean_risk))

  # Should return unchanged workflow
  expect_identical(result$workflow$data, workflow$data)
  expect_identical(result$workflow$train_idx, workflow$train_idx)
  expect_identical(result$workflow$test_idx, workflow$test_idx)

  # No fixes needed, no unfixable
  expect_equal(length(result$fixed), 0)
  expect_equal(length(result$unfixable), 0)

  # Post-rewrite report should still be valid
  expect_s4_class(result$report, "BorgRisk")
  expect_true(result$report@is_valid)
})
