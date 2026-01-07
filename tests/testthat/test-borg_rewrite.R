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

  # Both should still have the same report since stubs return success = FALSE
  expect_s4_class(result_all$report, "BorgRisk")
  expect_s4_class(result_thresholds$report, "BorgRisk")
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
