
# is_sorted --------------------------------------------------------------------

# Tests:
# - Assert documented examples

test_that("Examples - Control the order of the check with `order` argument", {
  expect_true(is_sorted(1:5))
  expect_false(is_sorted(1:5, order = "desc"))
})

test_that("Examples - Control the handling of NA values with `na.rm` argument", {
  expect_true(is_sorted(c(1, 2, NA, 4), na.rm = TRUE))
})

test_that("Examples - Control the strictness of the order with `strictly` argument", {
  expect_false(is_sorted(c(1, 2, 2, 4), strictly = TRUE))
})



# is_matching_set --------------------------------------------------------------

# Tests:
# - Assert documented examples

x <- c(5, 7, 5, 9, 9)

test_that("Examples - Default mode checks if all values in `x` are in `yes` and not in `no`", {
  expect_true(is_matching_set(x, yes = 1:10))
  expect_false(is_matching_set(x, yes = 1:8))
  expect_false(is_matching_set(x, yes = 1:10, no = 5))
})

test_that("Examples - Mode `any` passes even if there are values in `x` that are not in `yes`", {
  expect_true(is_matching_set(x, yes = 1:6, mode = "any"))
})

test_that("Examples - For mode `only`, all values in `x` must be in `yes` and vice versa", {
  expect_false(is_matching_set(x, yes = 1:10, mode = "only"))
  expect_true(is_matching_set(x, yes = c(5, 7, 9), mode = "only"))
})

test_that("Examples - `yes` can be NULL to test only `no` (independent of mode)", {
  expect_true(is_matching_set(x, no = 11))
  expect_true(is_matching_set(x))
})



# any_duplicated ---------------------------------------------------------------

# Tests:
# - Assert documented examples

x <- c(10, 10, 20, 30, 30, 40)

test_that("Examples - any_duplicated", {
  expect_false(any_duplicated(1:10))
  expect_true(any_duplicated(c(1, 1:10)))
  expect_identical(are_duplicated(x), c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(duplicated(x), c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
})
