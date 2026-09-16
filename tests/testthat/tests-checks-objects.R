
# predicates-objects -----------------------------------------------------------

# Tests:
# - Assert examples

xbase <- 1:10
xs3 <- structure(1:10, class = "my_s3")
xs4_int <- methods::setClass("my_s4_int", contains = "integer")(1:10)
xs4_s4 <- methods::setClass("my_s4_s4", slots = c(x = "integer"))(x = 1:10)

test_that("Examples - If has class, should have the object bit set, and vice versa", {
  expect_false(has_class(xbase))
  expect_true(has_class(xs3))
  expect_true(has_object_bit(xs3))
  expect_true(has_class(xs4_int))
})

test_that("Examples - S4 objects have the S4 bit set", {
  expect_false(has_s4_bit(xs3))
  expect_true(has_s4_bit(xs4_int))
})

test_that("Examples - is_s4 test for the typeof() \"S4\"", {
  expect_false(is_s4(xs4_int))
  expect_true(is_s4(xs4_s4))
})

test_that("Examples - is_object() test for the typeof() \"object\"", {
  suppressWarnings(class(xs4_s4) <- c("new_class", class(xs4_s4)))
  expect_true(is_object(xs4_s4))
})

test_that("Examples - is_object_like() is similar to checking object bit and class attribute", {
  expect_false(is_object_like(xbase))
  expect_true(is_object_like(xs4_int))
  attr(xs4_int, "class") <- NULL
  expect_true(has_s4_bit(xs4_int))
  expect_false(is_object_like(xs4_int, bad = FALSE))
})

test_that("Examples - has_class() checks for invalid values in the class attribute", {
  class(xs3) <- c("a", "b", "a")
  expect_false(has_class(xs3, invalid = "false"))
})



# object_system ---------------------------------------------------------------

# Tests:
# - Assert examples

x <- 1:10
xs4 <- methods::setClass("my_s4_int_2", contains = "integer")(1:10)

test_that("Examples - object_system", {
  expect_identical(object_system(1:10), "base")
  expect_identical(object_system(factor(letters)), "S3")
  expect_identical(object_system(xs4), "S4")
  expect_true(is_system(xs4, "S4"))
})
