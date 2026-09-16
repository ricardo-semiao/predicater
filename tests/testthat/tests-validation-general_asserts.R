# assert_from_msg --------------------------------------------------------------

# Tests:
# - Simple msg function

msg_fun <- function(x) {
  if (is.numeric(x) && !anyNA(x)) {
    TRUE
  } else {
    "x must be numeric and have no missing values"
  }
}

x <- c(1, 2, NA)

test_that("Simple msg function", {
  expect_snapshot(try(assert_from_msg(msg_fun, x)))
})



# assert_from_error ------------------------------------------------------------

# Tests:
# - simple error function

err_fun <- function(x) {
  if (is.numeric(x) && !anyNA(x)) {
    TRUE
  } else {
    stop("x must be numeric and have no missing values")
  }
}

x <- c(1, 2, NA)

test_that("Simple error function", {
  expect_snapshot(try(assert_from_error(err_fun, x)))
})



# assert_ptype -----------------------------------------------------------------

# Tests:
# - Assert example

test_that("Examples - assert_ptype snapshot", {
  expect_snapshot(try({
    f <- \(x) assert_ptype(numeric(), x)
    f("a")
  }))
})



# assert_predicate -------------------------------------------------------------

# Tests:
# - Assert example

test_that("Examples - assert_predicate snapshot", {
  expect_snapshot(try({
    f <- \(x) assert_predicate(\(x) all(x > 0), x)
    f(c(-1, 0, 1))
  }))
})



# test_multiple ----------------------------------------------------------------

# Tests:
# - Example

x <- list(a = 1:2, b = 3:4)

args <- list(
  types = c("integer", "list"),
  list = list(
    custom_map = \(elt) is_integer(elt, 1)
  )
)

test_that("Examples - test_multiple", {
  expect_false(do.call(test_multiple, c(list(x), args)))
})

test_that("Examples - assert_multiple snapshot", {
  expect_snapshot(try(do.call(assert_multiple, c(list(x), args))))
})
