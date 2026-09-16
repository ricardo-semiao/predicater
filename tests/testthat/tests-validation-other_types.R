
# assert_null ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- integer(0)

test_that("Examples - assert_null snapshot", {
  expect_snapshot(try(assert_null(x)))
})



# assert_function --------------------------------------------------------------

# Tests:
# - Assert examples

x <- function(a, b = 1, ...) {
  a + b
}
args <- list(
  mode = "closure",
  args_names = c("a", "b"), 
  fn_env = rlang::global_env(),
  dots = TRUE,
  sentinels = c("null"),
  custom = NULL
)

test_that("Examples - test_function", {
  expect_false(do.call(test_function, c(list(x), args)))
})

test_that("Examples - assert_function snapshot", {
  expect_snapshot(try(assert_function(x, !!!args, short_circuit = FALSE)))
})
