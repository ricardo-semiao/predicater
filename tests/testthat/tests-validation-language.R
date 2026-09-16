# test_symbol ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- quote(my_var)

args <- list(
  n_char = c(1, 10),
  valid = TRUE,
  empty = FALSE,
  env_has = rlang::global_env(),
  env_seen = NULL,
  sentinels = c("null"),
  custom = NULL
)

test_that("Examples - test_symbol", {
  expect_false(rlang::exec(test_symbol, x = quote(my_var), !!!args))
})

test_that("Examples - assert_symbol snapshot", {
  expect_snapshot(try(rlang::exec(assert_symbol, x = quote(my_var), !!!c(args, short_circuit = FALSE))))
})



# test_language ----------------------------------------------------------------

# Tests:
# - Assert examples

x <- quote(rlang::env(a = 1, b = 2))

args <- list(
  name = "fn",
  ns = "otherpkg",
  n_args = c(1, 5),
  arg_names = c("a", "b"),
  simple = FALSE,
  valid = TRUE,
  sentinels = c("null"),
  custom = NULL
)

test_that("Examples - test_language", {
  expect_false(rlang::exec(test_language, !!!c(list(x), args)))
})

test_that("Examples - assert_language snapshot", {
  expect_snapshot(try(rlang::exec(assert_language, !!!c(list(x), args, short_circuit = FALSE))))
})



# test_code --------------------------------------------------------------------

# Tests:
# - Assert examples

x <- 42L

args <- list(
  sym = FALSE,
  lang = FALSE,
  literal = TRUE,
  valid = TRUE,
  empty = FALSE,
  sentinels = c("null"),
  custom = \(x) x > 100
)

test_that("Examples - test_code", {
  expect_false(rlang::exec(test_code, !!!c(list(x), args)))
})

test_that("Examples - assert_code snapshot", {
  expect_snapshot(try(rlang::exec(assert_code, !!!c(list(x), args, short_circuit = FALSE))))
})
