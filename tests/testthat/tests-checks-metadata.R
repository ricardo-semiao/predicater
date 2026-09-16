
# is_empty2 --------------------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - is_empty2", {
  expect_true(is_empty2(list()))
  expect_false(is_empty2(list(1)))
  expect_false(is_empty2(list(NULL)))
  expect_true(is_empty2(character(0)))
  expect_false(is_empty2(letters))
})


test_that("Examples - Only works for collections", {
  expect_error(is_empty2(sum), "`x` is not a collection.")
})


test_that("Examples - But can be made to work with NULL", {
  expect_true(is_empty2(NULL, null = TRUE))
})
