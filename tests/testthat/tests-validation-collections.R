# test_list --------------------------------------------------------------------

# Tests:
# - Assert examples

x <- list(a = 1, b = NULL, c = integer(0), d = 1)

args <- list(
  mode = "list",
  len = c(1, 10),
  n_null = 0,
  n_empty = c(0, 1),
  n_dup = 0,
  sentinels = c("null"),
  custom = NULL,
  custom_map = \(elt) !is_na2(elt)
)

test_that("Examples - test_list", {
  expect_false(do.call(test_list, c(list(x), args)))
})

test_that("Examples - assert_list snapshot", {
  expect_snapshot(try(do.call(assert_list, c(list(x), args, short_circuit = FALSE))))
})



# test_environment --------------------------------------------------------------

# Tests:
# - Assert examples

x <- rlang::new_environment(list(a = 1, b = 2), parent = rlang::global_env())

args <- list(
  len = c(1, 5),
  env_has = c("a", "b"),
  env_sees = "__x__",
  parents = rlang::global_env(),
  namespace = TRUE,
  sentinels = c("null"),
  custom = NULL,
  custom_map = \(val) is.numeric(val)
)

test_that("Examples - test_environment", {
  expect_false(do.call(test_environment, c(list(x), args)))
})

test_that("Examples - assert_environment snapshot", {
  expect_snapshot(try(do.call(assert_environment, c(list(x), args, short_circuit = FALSE))))
})



# test_vector ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- list(1, 2, NA, 2, NULL)

args <- list(
  mode = "list",
  len = c(1, 10),
  n_na = 0,
  n_null = c(0, 1),
  n_empty = 0,
  n_dup = 0,
  sentinels = c("null"),
  custom = NULL,
  custom_map = \(elt) length(elt) <= 1
)

test_that("Examples - test_vector", {
  expect_false(do.call(test_vector, c(list(x), args)))
})

test_that("Examples - assert_vector snapshot", {
  expect_snapshot(try(do.call(assert_vector, c(list(x), args, short_circuit = FALSE))))
})
