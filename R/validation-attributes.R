
#' @include validation-helpers.R validation-menu.R
NULL



# Names ------------------------------------------------------------------------

#' Validation - Names attribute
#'
#' @description
#' Test if an object's names (or character vector of names) satisfies some
#' conditions.
#'
#' `test_names()` is the predicate test, while `assert_names()` validates
#' its input, aborting if it fails the test.
#'
#' Hint: use `sentinels = c("null")` to allow no (`NULL`) names, and `empty =
#' TRUE` to always pass the test if the underlying vector `x` is empty.
#'
#' @param x \[`any`] An object to get names from, or a character vector of names
#'   to test.
#' @param na_n,dup_n,empty_n,invalid_n `r ROXY$x_n("na_n,dup_n,empty_n,invalid_n")`
#' @param set `r ROXY$set("character")`
#' @param char_tests \[`list`] A list of additional arguments passed to
#'   [test_character()].
#' @param how \[`"x"` | `"names"` | `"attr"` | `"colnames"` | `"row.names"` |
#'   `integer(1)`] How to extract names from `x`: `"x"` for `x` directly,
#'   `"names"` for `names(x)`, `"attr"` for `attr(x, "names")`, `"colnames"` for
#'   `colnames(x)`, `"row.names"` for `attr(x, "row.names")`, or a positive
#'   integer for `dimnames(x)[[how]]`.
#' @param empty \[`TRUE` | `FALSE` | `NULL`] Whether to early pass or fail the
#'   test if the underlying vector `x` is empty. Set to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("names")`
#'
#' @name test_names
NULL

core_names <- function(
  x,
  na_n = NULL, empty_n = NULL, dup_n = NULL, invalid_n = NULL,
  set = NULL, char_tests = NULL, how = "names",
  empty = NULL, sentinels = NULL, custom = NULL,
  short_circuit = TRUE
) {
  x <- switch(how,
    names = names(x),
    x = x,
    attr = attr(x, "names", TRUE),
    colnames = colnames(x),
    row.names = attr(x, "row.names", TRUE),
    dimnames(x)[[as.integer(how)]]
  )

  run_tests(
    x, sentinels, na_n, empty_n, dup_n, invalid_n, set, char_tests, empty, custom,
    test_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_character(x) %@@% c(type = typeof(x)),
      empty_n = \(x, arg, pars) {
        n_empty <- sum(x == "", na.rm = TRUE)
        test_in_range(n_empty, arg, pars$l) %@@% c(n = n_empty)
      },
      invalid_n = \(x, arg, pars) {
        n_invalid <- sum(make.names(x) != x, na.rm = TRUE)
        test_in_range(n_invalid, arg, pars$l) %@@% c(n = n_invalid)
      },
      char_tests = \(x, arg, pars) {
        exec(test_character, x, !!!arg)
      },
      empty = \(x, arg, pars) {
        is_empty2(x)
      }
    )
  )
}
# cite sentinels NULL and empty
# TODO: order would be nice. set doesnt support it

#' @rdname test_names
#' @export
test_names <- fn_core_to_test(core_names)

#' @rdname test_names
#' @export
assert_names <- fn_core_to_assert(
  core_names,
  msgs_add = list(
    empty_n = \(attrs) "count of empty string names does not fall within the expected range.",
    invalid_n = \(attrs) "count of syntactically invalid R names does not fall within the expected range.",
    char_tests = \(attrs) "failed additional character tests specified in `char_tests`."
  )
)



# Dimensions -------------------------------------------------------------------

#' Validation - Matrix and array attributes
#'
#' @description
#' Test if an object has matrix/array-related attributesis that pass some
#' conditions. Add
#'
#' `test_matrix()` is a predicate test, while `assert_matrix()` validates their
#' input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param dims_n `r ROXY$x_n("dims_n")`
#' @param dims_shape \[`list()` | `integer()` | `NULL`] Expected size
#'   constraints for each dimension. Can be a vector of dimension sizes or a
#'   list of range specs (as for `dims_n`). Set to `NULL` to not test.
#' @param names_apply \[`list()` | `NULL`] A list of arguments passed to
#'   [test_names()] to test each dimension's names. For separate tests for each
#'   dimension, use a list of formulas, with the LHS being the dimension integer
#'   index, and the RHS being the list of arguments to `test_names()`.
#' @param how \[`"dim"` | `"attr"`] How to extract dimensions from `x`: `"dim"`
#'   for [dim()], `"attr"` for `attr(x, "dim")`.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_apply \[`list()` | `NULL`] A list of formulas. For each margin
#'   in the LHS (as in `MARGIN` in [apply()]), test the function in the RHS
#'   across that margin.
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns()`
#'
#' @name test_matrix
NULL

core_matrix <- function(
  x,
  dims_n = NULL, dims_shape = NULL, names_apply = NULL,
  how = "dim",
  sentinels = NULL, custom = NULL, custom_apply = NULL,
  short_circuit
) {
  dims <- switch(
    how,
    dim = dim(x),
    x = x,
    attr = attr(x, "dim", exact = TRUE)
  )

  run_tests(
    x, sentinels, dims_n, dims_shape, custom, custom_apply,
    tests_pars = list(dims = dims), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) !is_null(pars$dims),
      dims_n = \(x, arg, pars) {
        n_dims <- length(pars$dims)
        test_in_range(n_dims, arg, pars$l) %@@% c(n = n_dims)
      },
      dims_shape = \(x, arg, pars) {
        res <- logical(length(arg))
        for (i in seq_along(arg)) {
          res[i] <- test_in_range(pars$dims[i], arg[[i]], pars$l)
        }
        all(res)
      },
      names_apply = \(x, arg, pars) {
        dimnames <- dimnames(x)

        if (! is_formula(arg[[1]])) {
          res <- logical(length(dimnames))
          for (i in seq_along(dimnames)) {
            res[i] <- exec(test_names, x = dimnames[[i]], !!!arg)
          }
        } else {
          res <- logical(length(arg))
          for (i in seq_along(arg)) {
            margin <- eval(f_lhs(arg[[i]]))
            res[i] <- exec(test_names, x = dimnames[[margin]], !!!arg[[i]])
          }
        }
      },
      custom_apply = \(x, arg, pars) {
        res <- logical(length(arg))
        for (i in seq_along(arg)) {
          margin <- eval(f_lhs(arg[[i]]))
          res[i] <- test_custom(apply(x, margin, identity), arg[[i]])
        }
      }
    )
  )
}
# TODO: consider adding some of this functionality to n_dims and friends


#' @rdname test_matrix
#' @export
test_matrix <- fn_core_to_test(core_matrix)


#' @rdname test_matrix
#' @export
assert_matrix <- fn_core_to_assert(
  core_matrix,
  msgs_add = list(
    type = \(attrs) glue("no dimensions."),
    dims_n = \(attrs) "number of matrix dimensions does not fall within the expected range.",
    dims_shape = \(attrs) glue("dimension size for dimension {attrs$dim} failed expected shape check."),
    names_apply = \(attrs) glue("names for dimension failed expected check."),
    custom_apply = \(attrs) glue("custom applied assertion failed along margin '{attrs$margin}'.")
  )
)



# Classes ----------------------------------------------------------------------

#' Validation - Class attribute
#'
#' @description
#' Test if an object inherits from specific classes or has a valid class vector.
#'
#' Invalid classes fail the test: non-character vectors, empty character
#' vectors, or a vector with `NA` values.
#'
#' `test_class()` is the predicate test, while `assert_class()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param classes \[`list()` | `character()` | `NULL`] A named list specifying
#'   possible class inheritance criterias. The elements are the classes to test
#'   against, and the names are which test to do: `"any"` for
#'   [rlang::inherits_any()], `"all"` for [rlang::inherits_all()], `"only"` for
#'   [rlang::inherits_only()], and `"none"` for `!inherits_any()`. If any of the
#'   test passes, the overall test passes. If a single character, it is tested
#'   with `inherits_any()`. Set to `NULL` to not test.
#' @param tests_char \[`list`] A list of additional character tests passed to
#'   [test_character()].
#' @param how \[`"class"` | `"x"` | `"attr"`] How to extract class names for
#'   `tests_char`: `"x"` for `x` directly, `"class"` for [class()], and `"attr"`
#'   for `attr(x, "class")`.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("class")`
#'
#' @name test_class
NULL

core_class <- function(
  x,
  classes = NULL, tests_char = NULL, how = "class",
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  x <- switch(
    how,
    class = class(x),
    x = x,
    attr = attr(x, "class", exact = TRUE)
  )

  run_tests(
    x, sentinels, classes, tests_char, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) test_character(x, len = c(1, Inf), na_n = 0),
      classes = \(x, arg, pars) {
        if (is_character(arg)) {
          return(inherits_any(x, arg))
        }

        res <- logical(length(arg))
        hows <- names(arg)
        for (i in seq_along(arg)) {
          res[i] <- switch(hows[i],
            any = inherits_any(x, arg[[i]]),
            all = inherits_all(x, arg[[i]]),
            only = inherits_only(x, arg[[i]]),
            none = !inherits_any(x, arg[[i]])
          )
        }
        any(res)
      },
      tests_char = \(x, arg, pars) {
        exec(test_character, x, !!!arg)
      }
    )
  )
}

#' @rdname test_class
#' @export
test_class <- fn_core_to_test(core_class)

#' @rdname test_class
#' @export
assert_class <- fn_core_to_assert(
  core_class,
  msgs_add = list(
    type = \(attrs) "object does not have a valid class attribute.",
    classes = \(attrs) "object failed class inheritance constraints.",
    tests_char = \(attrs) "failed additional character tests on class names."
  )
)



# Objects ----------------------------------------------------------------------

#' Validation - Object-related attributes
#'
#' @description
#' Test if an input is an 'object' (see [predicates-objects]), and fits a 
#' specific Object-Oriented (OO) system (see [object_system()]).
#'
#' `test_object()` is the predicate test, while `assert_object()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param oo_system \[`character(1)` | `NULL`] Expected Object-Oriented system
#'   name, passed to [object_system()]. Set to `NULL` to not test.
#' @param s4_bit \[`TRUE` | `FALSE` | `NULL`] Whether the underlying S4 object
#'   bit must (`TRUE`) or musnt't be set. Set to `NULL` to not test.
#' @param tests_class \[`list`] A list of additional class tests passed directly
#'   to [test_class()].
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("object")`
#'
#' @name test_object
NULL

core_object <- function(
  x,
  oo_system = NULL, s4_bit = NULL, tests_class = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, oo_system, s4_bit, tests_class, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_object_like(x, bad = "f"),
      oo_system = \(x, arg, pars) is_system(x, arg),
      s4_bit = \(x, arg, pars) has_s4_bit(x) == arg,
      tests_class = \(x, arg, pars) exec(test_class, x = x, !!!arg)
    )
  )
}
# TODO: allow an any-like test in oo_system

#' @rdname test_object
#' @export
test_object <- fn_core_to_test(core_object)

#' @rdname test_object
#' @export
assert_object <- fn_core_to_assert(
  core_object,
  msgs_add = list(
    type = \(attrs) "is not an object.",
    oo_system = \(attrs) "failed object-oriented system constraint.",
    s4_bit = \(attrs) "S4 bit status does not match expected setting.",
    tests_class = \(attrs) "failed object class tests."
  )
)
