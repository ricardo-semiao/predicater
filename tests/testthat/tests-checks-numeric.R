
# predicates-infinite ----------------------------------------------------------

# Tests:
# - Assert examples

x <- c(1, Inf, -Inf, NaN, NA)

test_that("Examples - The default tests", {
  expect_identical(are_finite(x), c(TRUE, FALSE, FALSE, FALSE, NA))
  expect_identical(are_inf(x), c(FALSE, TRUE, TRUE, FALSE, NA))
  expect_identical(are_nan(x), c(FALSE, FALSE, FALSE, TRUE, NA))
})

test_that("Examples - For all, the NA value's result can be controlled", {
  expect_identical(are_finite(x, na = FALSE), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(are_nan(x, na = TRUE), c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("Examples - We can consider only +Inf or -Inf", {
  expect_identical(are_inf(x, signs = "+"), c(FALSE, TRUE, FALSE, FALSE, NA))
})

test_that("Examples - Errors for non-numeric objects", {
  expect_error(are_finite(list(1, 2)))
})

test_that("Examples - The is_* predicates test scalars", {
  expect_true(is_finite(1))
  expect_false(is_finite(1:10))
  expect_true(all(are_finite(1:10)))
})



# is_integer_like --------------------------------------------------------------

# Tests:
# - Assert examples

x <- c(1.0, NA, 1.0 + 1e-15, 1.0 + 1e-6, NaN, -Inf, 1e200)

test_that("Examples - Default test", {
  expect_identical(
    are_integer_like(x),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_false(is_integer_like(x))
})

test_that("Examples - Changing NA interpretation", {
  expect_identical(
    are_integer_like(x, na = NA),
    c(TRUE, NA, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("Examples - Adding tolerance", {
  expect_identical(
    are_integer_like(x, tol = sqrt(.Machine$double.eps)),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    are_integer_like(x, tol = 1e-5),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("Examples - unbounded mode allows Inf, NaN, and out-of-integer-range values", {
  expect_identical(
    are_integer_like(x, mode = "unbounded"),
    c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("Examples - Adding tolerance, all pass, and finally is_integer_like() returns TRUE", {
  expect_identical(
    are_integer_like(x, mode = "unbounded", tol = 1e-5),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_true(is_integer_like(x, mode = "unbounded", tol = 1e-5))
})

test_that("Examples - are_integer_like() fails for non-numeric objects, while is_integer_like() returns FALSE", {
  expect_error(are_integer_like(list(1L, 2L)))
  expect_false(is_integer_like(list(1L, 2L)))
})

test_that("Examples - To test for a single integer-like value, use the n argument", {
  expect_true(is_integer_like(1L, n = 1))
  expect_false(is_integer_like(1:2, n = 1))
})

test_that("Examples - Integer vectors always return TRUE for NA values", {
  expect_identical(are_integer_like(c(1L, NA_integer_), na = NA), c(TRUE, TRUE))
})
