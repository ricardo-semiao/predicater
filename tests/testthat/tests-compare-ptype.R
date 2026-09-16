
# is_ptype ---------------------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - is_ptype", {
  expect_true(is_ptype(1:10, integer()))
  expect_false(is_ptype(1:10, integer(9)))
  expect_true(is_ptype(matrix(1:9, 3, 3), integer()))
  expect_false(is_ptype(matrix(1:9, 3, 3), integer(), dim = "=="))
})

test_that("Examples - is_ptype with factor levels", {
  expect_true(is_ptype(
    factor(c("a", "b")), factor(levels = c("a", "b", "c"))
  ))
  expect_false(is_ptype(
    factor(c("a", "b")), factor(levels = c("a", "b", "c")),
    levels = "=="
  ))
})

test_that("Examples - is_ptype_list", {
  schema <- list(a = integer(1), b = data.frame(), c = double())
  expect_true(is_ptype_list(
    list(a = 1L, b = mtcars, c = rnorm(sample(1:10))),
    schema
  ))
})
