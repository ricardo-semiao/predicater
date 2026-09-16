# assert_null ------------------------------------------------------------------

# Tests:
# - Assert examples

x <- integer(0)

test_that("Examples - assert_null snapshot", {
  expect_snapshot(try(assert_null(x)))
})
