
# predicates-language ----------------------------------------------------------

# Tests:
# - Assert examples

test_that("Examples - is_syntactic_literal", {
  expect_true(is_syntactic_literal(1))
  expect_true(is_syntactic_literal("a"))
  expect_true(is_syntactic_literal(NULL))
})

test_that("Examples - is_symbol2", {
  expect_true(is_symbol2(quote(x)))
  expect_false(is_symbol2(quote(x), name = "y"))
  expect_false(is_symbol2(rlang::expr(), empty = FALSE))
})

test_that("Examples - is_language", {
  expect_true(is_language(quote(x + 1)))
  expect_true(is_language(quote(f(x))))
  expect_true(is_language(quote(if (TRUE) 1 else 2)))
})

test_that("Examples - is_expression2", {
  expect_true(is_expression2(expression(1, x, x + 1)))
  expect_false(is_expression2(rlang::exprs(1, x, x + 1)))
})

x <- 1

test_that("Examples - is_code", {
  expect_true(is_code(x))
  expect_false(is_code(x, literal = FALSE))
})
