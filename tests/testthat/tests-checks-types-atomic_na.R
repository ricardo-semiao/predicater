
# predicates-atomic ------------------------------------------------------------

# Tests:
# - Assert examples (no examples)



# predicates-true-false --------------------------------------------------------

# Tests:
# - Assert examples

x <- c(TRUE, FALSE, NA)

test_that("Examples - Vectorized tests", {
  expect_identical(are_true(x), c(TRUE, FALSE, FALSE))
  expect_identical(are_true(x, na = NA), c(TRUE, FALSE, NA))
  expect_identical(are_false(x), c(FALSE, TRUE, FALSE))
  expect_identical(are_false(x, na = NA), c(FALSE, TRUE, NA))
  expect_identical(are_true(logical(0)), logical(0))
})

test_that("Examples - Scalar tests", {
  expect_false(is_true2(c(TRUE, TRUE)))
  expect_false(is_true2(TRUE))
  expect_false(is_true2(NA))
  expect_identical(is_true2(NA, na = NA), NA)
  expect_true(is_bool2(TRUE))
  expect_true(is_bool2(FALSE))
  expect_false(is_bool2(NA))
  expect_identical(is_bool2(NA, na = NA), NA)
})


test_that("Examples - Error for non-logical vectors", {
  expect_error(are_true(1:3))
})



# is_na2 -----------------------------------------------------------------------

# Tests:
# - Assert examples

x <- c(1, Inf, -Inf, NaN, NA)

test_that("Examples - are_na2 is vectorized", {
  expect_identical(are_na2(x), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(are_na2(x, nan = TRUE), c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(are_na2(x, nan = NA), c(FALSE, FALSE, FALSE, NA, TRUE))
})

test_that("Examples - Errors for non-atomic objects", {
  expect_error(are_na2(list(NA, NA)))
})

test_that("Examples - is_na2 test for a scalar NA value", {
  expect_false(is_na2(c(NA, NA)))
  expect_true(is_na2(NA))
  expect_true(is_na2(NaN, nan = TRUE))
  expect_true(all(are_na2(c(NA, NA))))
})

test_that("Examples - Accept only NAs of specific types", {
  expect_false(is_na2(NA_integer_, types = c("logical", "character")))
})

test_that("Examples - Use n to test for a single NA value", {
  expect_true(is_na2(NA, n = 1))
})
