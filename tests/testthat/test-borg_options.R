# ===========================================================================
# Tests for BORG options and auto-check
# ===========================================================================

test_that("borg_options returns default values", {
  # Reset options first
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)

  opts <- borg_options()

  expect_equal(opts$auto_check, FALSE)
  expect_equal(opts$strict, TRUE)
  expect_equal(opts$verbose, FALSE)
})

test_that("borg_auto_check enables auto-check mode", {
  # Reset options
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)

  # Enable
  old <- borg_auto_check(TRUE)

  expect_equal(getOption("borg.auto_check"), TRUE)
  expect_equal(getOption("borg.strict"), TRUE)
  expect_equal(getOption("borg.verbose"), FALSE)

  # Check old values returned

  expect_equal(old$borg.auto_check, FALSE)

  # Disable
  borg_auto_check(FALSE)
  expect_equal(getOption("borg.auto_check"), FALSE)

  # Cleanup
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)
})

test_that("borg_auto_check handles strict and verbose", {
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)

  borg_auto_check(TRUE, strict = FALSE, verbose = FALSE)

  expect_equal(getOption("borg.strict"), FALSE)
  expect_equal(getOption("borg.verbose"), FALSE)

  # Cleanup
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)
})

test_that("borg_auto_check verbose mode prints message", {
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)

  expect_message(
    borg_auto_check(TRUE, verbose = TRUE),
    "BORG auto-check enabled"
  )

  # Cleanup
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)
})

test_that("borg_options reflects current settings", {
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)

  borg_auto_check(TRUE, strict = FALSE, verbose = TRUE)

  opts <- borg_options()

  expect_equal(opts$auto_check, TRUE)
  expect_equal(opts$strict, FALSE)
  expect_equal(opts$verbose, TRUE)

  # Cleanup
  options(borg.auto_check = NULL, borg.strict = NULL, borg.verbose = NULL)
})
