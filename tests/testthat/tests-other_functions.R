
# c2 ---------------------------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - c2", {
  expect_identical(c2(FALSE, 1L, 1.5), c(0, 1.0, 1.5))
  expect_identical(c2(Sys.Date(), Sys.time()), c(as.POSIXct(Sys.Date()), as.POSIXct(Sys.time())))
  expect_identical(c2(factor("a"), factor("b")), factor(c("a", "b")))
  expect_identical(c2(name = 1), c(name = 1))
})

test_that("Examples - c2 with name specification", {
  expect_identical(
    c2(name = 1:3, .name_spec = "{outer}_{inner}"),
    c(name_1 = 1, name_2 = 2, name_3 = 3)
  )
})

test_that("Examples - c2 errors on invalid named inputs", {
  expect_error(c2(name = 1:3))
})



# pany -------------------------------------------------------------------------

# Tests:
# - Assert examples

a <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
b <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)

test_that("Examples - Default behavior treats missings like | and &", {
  expect_identical(pany(a, b), a | b)
  expect_identical(pall(a, b), a & b)
})

test_that("Examples - Remove missings from the computation", {
  expect_identical(pany(a, b, .na = FALSE), (a & !is.na(a)) | (b & !is.na(b)))
  expect_identical(pall(a, b, .na = TRUE), (a | is.na(a)) & (b | is.na(b)))
})

test_that("Examples - Check for missings in parallel", {
  expect_identical(pany_na(a, b), c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_identical(pall_na(a, b), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})



# reduce_predicate -------------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - reduce_predicate", {
  expect_true(reduce_predicate(list(1L, 1.0, 3), `==`, .op = "or"))
  expect_false(reduce_predicate(list(1L, 1.0, 3), `==`, .op = "and"))
})

test_that("Examples - accumulate_predicate", {
  expect_identical(accumulate_predicate(list(1L, 1.0, 3), `==`, .op = "and"), c(TRUE, FALSE))
  expect_identical(accumulate_predicate(list(1L, 1.0, 3), identical, .op = "and"), c(FALSE, FALSE))
})



# check_installed2 -------------------------------------------------------------

# Tests:
# - Assert examples (no examples)
