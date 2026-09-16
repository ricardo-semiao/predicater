
# is_attrs_allowed -------------------------------------------------------------

# Tests:
# - Assert documented examples

test_that("Examples - is_attrs_allowed", {
  expect_true(is_attrs_allowed(mtcars))
  expect_false(is_attrs_allowed(NULL))
})



# attr2 ------------------------------------------------------------------------

# Tests:
# - Assert documented examples

x <- structure(1, long_name = 2)

test_that("Examples - attr2", {
  expect_identical(attr(x, "long"), 2)
  expect_null(attr2(x, "long"))
})



# attributes-filter ------------------------------------------------------------

# Tests:
# - Assert documented examples

x <- structure(
  1:3, a = 1, b = 2, c = 3,
  names = c("X", "Y", "Z"), dim = c(1, 3), class = "myclass",
  levels = c("aa", "bb", "cc")
)

test_that("Examples - Default removal", {
  expect_identical(attrs_keep(x), 1:3)
  expect_identical(attrs_rmv(x), x)
})


test_that("Examples - Abbreviation, exact, and regex filtering", {
  expect_identical(
    attrs_keep(x, abbr = "ncd"),
    structure(1:3, names = c("X", "Y", "Z"), dim = c(1, 3), class = "myclass")
  )
  expect_identical(
    attrs_rmv(x, exact = c("levels", "b")),
    structure(
      1:3, a = 1, c = 3,
      names = c("X", "Y", "Z"), dim = c(1, 3), class = "myclass"
    )
  )
  expect_identical(
    attrs_keep(x, match = "^[a-c]$"),
    structure(1:3, a = 1, b = 2, c = 3)
  )
})


test_that("Examples - Testing for attributes", {
  expect_true(has_attrs_any(x, "c", exact = "levels"))
  expect_false(has_attrs_any(x, "c", exact = "levels", disallow = "dim"))
  expect_true(has_attrs_all(x, exact = c("a", "b", "c")))
  expect_false(has_attrs_all(x, exact = c("a", "b", "c", "d")))
  expect_false(has_attrs_only(x, exact = c("a", "b", "c")))
})



# has_names_valid --------------------------------------------------------------

# Tests:
# - Assert documented examples

x <- structure(
  1:6,
  dim = c(2, 3, 1),
  names = paste0("i", 1:6),
  dimnames = list(c("a", "a"), c("x", "y", "z"), "_bad"),
  class = c("classA", "", NA, "classB", "classA")
)

test_that("Examples - has_names_valid", {
  expect_true(has_names_valid(mtcars))
})

test_that("Examples - NA, empty, and duplicate names return FALSE by default", {
  expect_false(has_names_valid(rlang::set_names(1:3, c("a", "b", NA))))
  expect_true(has_names_valid(rlang::set_names(1:3, c("a", "b", NA)), na = TRUE))
  expect_false(has_names_valid(rlang::set_names(1:3, c("a", "", "b"))))
  expect_false(has_names_valid(rlang::set_names(1:3, c("a", "a", "a"))))
})

test_that("Examples - Invalid names and zero-length x's return TRUE by default", {
  expect_true(has_names_valid(rlang::set_names(1:3, c("_bad", "b", "c"))))
  expect_true(has_names_valid(integer(0)))
})

test_that("Examples - Use `how` to control how names are extracted", {
  expect_false(has_names_valid(rlang::global_env(), how = "attr"))
  expect_true(has_names_valid(c("a", "b", "c"), how = "x"))
})

test_that("Examples - Vectorized test", {
  expect_identical(
    are_names_valid(c("a", "b", "b", NA, ""), how = "x"),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})



# attributes-dimensions --------------------------------------------------------

# Tests:
# - Assert documented examples

x <- structure(
  1:6,
  dim = c(2, 3, 1),
  dimnames = list(c("a", "a"), c("x", "y", "z"), NULL)
)

test_that("Examples - Counting dimensions and dimension names", {
  expect_identical(n_dims(x), 3L)
  expect_identical(n_dims(x, empty = FALSE), 2L)
  expect_identical(n_dimnames(x), 2L)
  expect_identical(n_dimnames(x, invalid = FALSE), 1L)
})

test_that("Examples - Testing for dimensions and dimension names", {
  expect_true(has_dimnames(x))
  expect_true(has_rownames(mtcars))
  expect_true(has_dim(mtcars))
})

test_that("Examples - Data frames often have no actual dimension attribues", {
  expect_false(has_dim(mtcars, how = "attr"))
  expect_false(has_dimnames(mtcars, how = "attr"))
  expect_true(has_rownames(mtcars, how = "row.names"))
})



# attributes-getters-setters ---------------------------------------------------

# Tests:
# - Assert documented examples

x <- structure(
  1:6, dim = c(2, 3, 1), names = paste0("i", 1:6),
  dimnames = list(c("a", "a"), c("x", "y", "z"), "_bad"),
  class = c("classA", "", NA, "classB", "classA")
)

test_that("Examples - Getting repaired names and valid classes", {
  expect_identical(names3(x), c("i1", "i2", "i3", "i4", "i5", "i6"))
  expect_identical(
    suppressMessages(dimnames2(x)),
    list(c("a...1", "a...2"), c("x", "y", "z"), "_bad")
  )
  expect_identical(
    suppressMessages(dimnames2(x, repair = "universal")),
    list(c("a...1", "a...2"), c("x", "y", "z"), "._bad")
  )
  expect_identical(suppressMessages(rownames2(x)), c("a...1", "a...2"))
  expect_identical(class2(x), c("classA", "classB"))
})

test_that("Examples - Setting repaired names and valid classes", {
  y <- x
  class2(y) <- c("again", "duplicates", "again")
  expect_identical(class2(y), c("again", "duplicates"))

  y <- x
  suppressMessages(rownames2(y) <- NULL)
  expect_identical(rownames(y), c("...1", "...2"))
})
