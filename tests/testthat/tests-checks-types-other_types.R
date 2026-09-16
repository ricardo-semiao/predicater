
# predicates-other-types -------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - is_type", {
  expect_true(is_type(1:10, "integer"))
  expect_true(is_type(NULL, "NULL"))
})



# functions --------------------------------------------------------------------

# Tests:
# - Assert examples (no examples)



# collections ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- list(1, 2, 3)
y <- rlang::env(a = 1, b = 2)

test_that("Examples - is_collection", {
  expect_true(is_collection(list(1, 2, 3)))
  expect_true(is_collection(rlang::env(a = 1, b = 2), n = 2))
  expect_false(is_collection(NULL))
  expect_true(is_collection(NULL, null = TRUE))
  expect_true(rlang::is_empty(NULL))
})
