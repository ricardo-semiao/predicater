# test_integer -----------------------------------------------------------------

# Tests:
# - Assert examples

x <- c(1.0, 2.0, 3.0 + 1e-100, 4.0, Inf)

args <- list(
  mode = "unbounded",
  mode_tol = sqrt(.Machine$double.eps),
  len = c(1, 10),
  n_na = 0,
  n_dup = 0,
  n_nan = 0,
  n_inf = 0,
  range = c(0, 10),
  set = list(no = c(0)),
  sentinels = c("null"),
  sorted = "desc",
  custom = NULL
)

test_that("Examples - test_integer", {
  expect_false(do.call(test_integer, c(list(x), args)))
})

test_that("Examples - assert_integer_like snapshot", {
  expect_snapshot(try(do.call(assert_integer_like, c(list(x), args, short_circuit = FALSE))))
})



# test_double ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- c(1L, 2L, 5L, 10L)

args <- list(
  mode = "numeric",
  len = c(1, 10),
  n_na = 0,
  n_dup = 0,
  n_nan = 0,
  n_inf = 0,
  range = c(1, 100),
  set = list(yes = c(1L, 2L, 3L)),
  sentinels = c("null"),
  sorted = "asc",
  custom = NULL
)

test_that("Examples - test_double", {
  expect_false(do.call(test_double, c(list(x), args)))
})

test_that("Examples - assert_double snapshot", {
  expect_snapshot(try(do.call(assert_double, c(list(x), args, short_circuit = FALSE))))
})
