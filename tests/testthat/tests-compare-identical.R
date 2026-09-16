
# identical2 -------------------------------------------------------------------

# Tests:
# - Assert examples

x <- structure(c(1, 2, 3), a = "a", b = "b", c = "c")
y <- structure(c(3, 2, 1), b = "b", a = "a", c = "d")

test_that("Examples - identical2", {
  expect_false(identical2(x, y))
})

test_that("Examples - Pre-sort data and ignore \"c\" attribute", {
  expect_true(identical2(
    x, y,
    ord_data = FALSE,
    ignore_attrs = list(exact = c("c"))
  ))
})

test_that("Examples - Consider attribute order", {
  expect_false(identical2(
    x, y,
    ord_data = FALSE,
    ignore_attrs = list(exact = c("c")),
    ord_attrs = TRUE
  ))
})

test_that("Examples - No sorting but accept up to 2.1 numerical absolute tolerance", {
  expect_true(identical2(
    x, y,
    tol_type = "abs",
    tol = 2.1,
    ignore_attrs = list(exact = c("c"))
  ))
})

test_that("Examples - Understanding where the differences are", {
  expect_identical(
    identical_flag(x, y),
    list(
      .data = c(FALSE, TRUE, FALSE),
      .attrs = list(a = list(.data = TRUE), b = list(.data = TRUE), c = list(.data = FALSE))
    )
  )
})
