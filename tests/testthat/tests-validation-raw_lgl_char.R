# test_logical -----------------------------------------------------------------

# Tests:
# - Assert examples

x <- c(TRUE, FALSE, NA, TRUE)

args <- list(
  len = c(1, 10),
  n_na = 0,
  n_true = c(1, 2),
  sentinels = c("null"),
  custom = NULL
)

test_that("Examples - test_logical", {
  expect_false(do.call(test_logical, c(list(x), args)))
})

test_that("Examples - assert_logical snapshot", {
  expect_snapshot(try(do.call(assert_logical, c(list(x), args, short_circuit = FALSE))))
})



# test_character ---------------------------------------------------------------

# Tests:
# - Assert examples

x <- c("apple", "banana", "cherry", "banana")

args <- list(
  len = c(1, 10),
  n_na = 0,
  n_dup = 0,
  n_char = c(1, 10),
  set = list(no = c("date")),
  match = list(
    yes = "^[a-z]+$",
    no = "x"
  ),
  sorted = FALSE,
  sentinels = c("null"),
  custom = NULL
)

test_that("Examples - test_character", {
  expect_false(do.call(test_character, c(list(x), args)))
})

test_that("Examples - assert_character snapshot", {
  expect_snapshot(try(do.call(assert_character, c(list(x), args))))
})
