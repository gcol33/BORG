# ===========================================================================
# Tests for BorgRisk S4 class
# ===========================================================================

test_that("BorgRisk constructor validates risk structure", {
 # Valid construction
 expect_s4_class(
   new("BorgRisk",
     risks = list(),
     n_hard = 0L,
     n_soft = 0L,
     is_valid = TRUE,
     train_indices = 1:5,
     test_indices = 6:10,
     timestamp = Sys.time(),
     call = quote(test())
   ),
   "BorgRisk"
 )

 # Risk with missing required fields
 expect_error(
   new("BorgRisk",
     risks = list(list(type = "test")),  # missing severity, description
     n_hard = 0L,
     n_soft = 0L,
     is_valid = TRUE,
     train_indices = 1:5,
     test_indices = 6:10,
     timestamp = Sys.time(),
     call = quote(test())
   ),
   "missing required fields"
 )

 # Invalid severity value
 expect_error(
   new("BorgRisk",
     risks = list(list(type = "test", severity = "invalid", description = "desc")),
     n_hard = 0L,
     n_soft = 0L,
     is_valid = TRUE,
     train_indices = 1:5,
     test_indices = 6:10,
     timestamp = Sys.time(),
     call = quote(test())
   ),
   "must be 'hard_violation' or 'soft_inflation'"
 )
})


test_that("BorgRisk validates count consistency", {
 # n_hard doesn't match actual hard violations
 expect_error(
   new("BorgRisk",
     risks = list(list(
       type = "test",
       severity = "hard_violation",
       description = "desc"
     )),
     n_hard = 0L,  # Should be 1
     n_soft = 0L,
     is_valid = FALSE,
     train_indices = 1:5,
     test_indices = 6:10,
     timestamp = Sys.time(),
     call = quote(test())
   ),
   "n_hard does not match"
 )
})


test_that("BorgRisk validates is_valid consistency", {
 # is_valid = TRUE but hard violations exist
 expect_error(
   new("BorgRisk",
     risks = list(list(
       type = "test",
       severity = "hard_violation",
       description = "desc"
     )),
     n_hard = 1L,
     n_soft = 0L,
     is_valid = TRUE,  # Should be FALSE
     train_indices = 1:5,
     test_indices = 6:10,
     timestamp = Sys.time(),
     call = quote(test())
   ),
   "is_valid cannot be TRUE when hard violations exist"
 )
})


test_that("BorgRisk validates index non-overlap consistency", {
 # Overlapping train/test indices with is_valid = TRUE should error
 # (the overlap should have been detected and is_valid should be FALSE)
 expect_error(
   new("BorgRisk",
     risks = list(),
     n_hard = 0L,
     n_soft = 0L,
     is_valid = TRUE,
     train_indices = 1:6,
     test_indices = 5:10,  # overlaps at 5, 6
     timestamp = Sys.time(),
     call = quote(test())
   ),
   "overlap.*is_valid=TRUE"
 )

 # But overlapping indices WITH is_valid = FALSE is allowed
 # (this represents a detected overlap being reported)
 obj <- new("BorgRisk",
   risks = list(list(
     type = "index_overlap",
     severity = "hard_violation",
     description = "Overlap detected"
   )),
   n_hard = 1L,
   n_soft = 0L,
   is_valid = FALSE,
   train_indices = 1:6,
   test_indices = 5:10,
   timestamp = Sys.time(),
   call = quote(test())
 )
 expect_s4_class(obj, "BorgRisk")
 expect_false(obj@is_valid)
})


test_that("BorgRisk show method works", {
 obj <- new("BorgRisk",
   risks = list(
     list(
       type = "test_hard",
       severity = "hard_violation",
       description = "A hard violation",
       source_object = "test_obj"
     ),
     list(
       type = "test_soft",
       severity = "soft_inflation",
       description = "A soft inflation"
     )
   ),
   n_hard = 1L,
   n_soft = 1L,
   is_valid = FALSE,
   train_indices = 1:5,
   test_indices = 6:10,
   timestamp = Sys.time(),
   call = quote(test())
 )

 expect_output(show(obj), "BorgRisk Assessment")
 expect_output(show(obj), "INVALID")
 expect_output(show(obj), "Hard violations:  1")
 expect_output(show(obj), "HARD VIOLATIONS")
 expect_output(show(obj), "SOFT INFLATIONS")
})


test_that("BorgRisk show method handles empty risks", {
 obj <- new("BorgRisk",
   risks = list(),
   n_hard = 0L,
   n_soft = 0L,
   is_valid = TRUE,
   train_indices = 1:5,
   test_indices = 6:10,
   timestamp = Sys.time(),
   call = quote(test())
 )

 expect_output(show(obj), "VALID")
 expect_output(show(obj), "No risks detected")
})


test_that("as.data.frame.BorgRisk works", {
 obj <- new("BorgRisk",
   risks = list(
     list(
       type = "type1",
       severity = "hard_violation",
       description = "desc1",
       source_object = "obj1",
       affected_indices = 1:3
     ),
     list(
       type = "type2",
       severity = "soft_inflation",
       description = "desc2"
     )
   ),
   n_hard = 1L,
   n_soft = 1L,
   is_valid = FALSE,
   train_indices = 1:5,
   test_indices = 6:10,
   timestamp = Sys.time(),
   call = quote(test())
 )

 df <- as.data.frame(obj)

 expect_s3_class(df, "data.frame")
 expect_equal(nrow(df), 2)
 expect_equal(ncol(df), 5)
 expect_equal(df$type, c("type1", "type2"))
 expect_equal(df$severity, c("hard_violation", "soft_inflation"))
 expect_equal(df$n_affected, c(3L, 0L))
})


test_that("as.data.frame.BorgRisk handles empty risks", {
 obj <- new("BorgRisk",
   risks = list(),
   n_hard = 0L,
   n_soft = 0L,
   is_valid = TRUE,
   train_indices = 1:5,
   test_indices = 6:10,
   timestamp = Sys.time(),
   call = quote(test())
 )

 df <- as.data.frame(obj)

 expect_s3_class(df, "data.frame")
 expect_equal(nrow(df), 0)
})
