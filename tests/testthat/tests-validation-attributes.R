
# test_names -------------------------------------------------------------------

# Tests:
# - Assert examples

x <- rlang::set_names(1:6, c("a", "b", "c", NA, "", ""))

args <- list(
  n_na = 0,
  n_dup = NULL,
  n_empty = c(0, -1),
  n_invalid = c(0, Inf),
  set = list(yes = c("a", "b"), no = c("d", "e")),
  how = "names",
  empty = FALSE,
  sentinels = c("null"),
  custom = \(x) isTRUE(all(nchar(x) == 1))
)

test_that("Examples - test_names", {
  expect_false(do.call(test_names, c(list(x), args)))
})

test_that("Examples - assert_names snapshot", {
  expect_snapshot(try(do.call(assert_names, c(list(x), args, short_circuit = FALSE))))
})



# test_matrix ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- matrix(
  1:6, nrow = 2, ncol = 3,
  dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
)

args <- list(
  n_dims = 2,
  dims_shape = list(2, c(1, Inf)),
  names_apply = list(
    1 ~ list(n_na = 0, n_dup = 0),
    2 ~ list(set = list(no = c("c4")))
  ),
  how = "dim",
  sentinels = c("null"),
  custom = \(x) is.matrix(x),
  custom_apply = list(1 ~ \(row) sum(row) > 10)
)

test_that("Examples - test_matrix", {
  expect_false(do.call(test_matrix, c(list(x), args)))
})

test_that("Examples - assert_matrix snapshot", {
  expect_snapshot(try(do.call(assert_matrix, c(list(x), args, short_circuit = FALSE))))
})



# test_class -------------------------------------------------------------------

# Tests:
# - Assert examples

x <- structure(
  list(a = 1),
  class = c("another_class", "custom_df", "data.frame")
)

args <- list(
  classes = list(
    all = c("custom_df", "data.frame"),
    none = "matrix"
  ),
  tests_char = list(n_na = 0, n_dup = 0),
  how = "class",
  sentinels = c("null"),
  custom = \(x) has_dim(x)
)

test_that("Examples - test_class", {
  expect_false(do.call(test_class, c(list(x), args)))
})

test_that("Examples - assert_class snapshot", {
  expect_snapshot(try(do.call(assert_class, c(list(x), args, short_circuit = FALSE))))
})



# test_object ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- structure(
  list(a = 1),
  class = c("my_s3_class")
)

args <- list(
  oo_system = "S3",
  s4_bit = FALSE,
  tests_class = list(
    classes = list(all = c("my_s3_class", "another_class"))
  ),
  sentinels = c("null"),
  custom = \(x) is_list(x)
)

test_that("Examples - test_object", {
  expect_false(do.call(test_object, c(list(x), args)))
})

test_that("Examples - assert_object snapshot", {
  expect_snapshot(try(do.call(assert_object, c(list(x), args, short_circuit = FALSE))))
})
