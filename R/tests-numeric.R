
#' @include tests-helpers.R tests-menu.R
NULL



# Core functions ---------------------------------------------------------------

#' Tests - Numeric vectors
#'
#' @description
#' Test if an object is a numeric vector:
#' - `test_integer(x, mode = "strict")` tests for integer vectors.
#' - `test_integer_like(x, mode = *)` tests for integer-like vectors via
#'   [is_integer_like()], with its multiple modes.
#' - `test_double(x, mode = "double")` tests for double vectors.
#' - `test_double(x, mode = "numeric")` tests for double or integer vectors.
#' - `test_complex()` tests for complex vectors, delegating tests on its real,
#'   imaginary, modulus, and argument parts, to `test_double()`.
#'
#' They all are predicate tests, while the `assert_*()` functions validate their
#' input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param mode,mode_tol \[`"strict"` | `"range"` | `"range_tol"` | `"trunc"` |
#'   `"trunc_tol"`, `double(1)`] For `*_integer()`: the `mode` and `tol`
#'   arguments to pass to [is_integer_like()].
#' @param len,na_n,dup_n,nan_n,inf_n `r ROXY$x_n("len,na_n,dup_n,nan_n,inf_n")`
#' @param range \[`integer()` | `NULL`] A vector with the upper and lower bound
#'   for `x` values (`Inf` is allowed). Or a vector with three or more values to
#'   test for `. %in% range`. Set to `NULL` to not test.
#' @param set `r ROXY$set("integer")`
#' @param sentinels `r ROXY$sentinels()`
#' @param sorted `r ROXY$sorted()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#' @param re_tests,im_tests,mod_tests,arg_tests \[`list()` | `NULL`] For
#'   `*_complex()`: a list with the same named arguments as `test_double()`, to
#'   test the real, imaginary, modulus and argument values of `x`.
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns()`
#'
#' @name test-numeric
NULL


core_integer <- function(
  x, mode = "strict",
  len = NULL, na_n = NULL, dup_n = NULL, nan_n = NULL, inf_n = NULL,
  range = NULL, set = NULL, sorted = NULL,
  custom = NULL, custom_map = NULL, sentinels = NULL,
  mode_tol = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, mode, len, na_n, dup_n, nan_n, inf_n, range, set, sorted,
    custom, custom_map,
    tests_pars = list(l = length(x), mode = mode, mode_tol = mode_tol), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        mode <- pars$mode
        tol <- pars$mode_tol
        if (mode == "strict") {
          is_integer(x)
        } else {
          if (is_null(tol)) { # To respect the default tol
            is_integer_like(x, mode = mode)
          } else {
            is_integer_like(x, mode = mode, tol = tol)
          }
        } %@@%
          c(mode = mode, type = typeof(x), tol = tol)
      }
    )
  )
}


core_double <- function(
  x, mode = "double",
  len = NULL, na_n = NULL, dup_n = NULL, nan_n = NULL, inf_n = NULL,
  range = NULL, set = NULL, sorted = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, mode, len, na_n, dup_n, nan_n, inf_n, range, set, sorted,
    custom, custom_map,
    tests_pars = list(l = length(x), mode = mode), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        mode <- pars$mode
        if (mode == "double") {
          is_double(x)
        } else {
          is_numeric(x)
        } %@@%
          c(mode = mode, type = typeof(x))
      }
    )
  )
}


core_complex <- function(
  x,
  re_tests = NULL, im_tests = NULL, mod_tests = NULL, arg_tests = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, re_tests, im_tests, mod_tests, arg_tests, custom, custom_map,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_complex(x) %@@% c(type = typeof(x)),
      re_tests = \(x, arg, pars) {
        exec(test_double, x = Re(x), !!!arg) %@@%
          c(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      },
      im_tests = \(x, arg, pars) {
        exec(test_double, x = Im(x), !!!arg) %@@%
          c(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      },
      mod_tests = \(x, arg, pars) {
        exec(test_double, x = Mod(x), !!!arg) %@@%
          c(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      },
      arg_tests = \(x, arg, pars) {
        exec(test_double, x = Arg(x), !!!arg) %@@%
          c(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      }
    )
  )
}



# Functions --------------------------------------------------------------------

# Test functions:

#' @rdname test-numeric
#' @export
test_integer <- fn_core_to_test(core_integer)

#' @rdname test-numeric
#' @export
test_double <- fn_core_to_test(core_double)

#' @rdname test-numeric
#' @export
test_complex <- fn_core_to_test(core_complex)


# Assert functions:

#' @rdname test-numeric
#' @export
assert_integer <- fn_core_to_assert(
  core_integer,
  msgs_add = list(
    type = \(attrs) {
      fn <- if (attrs$mode == "strict") "is_integer" else "is_integer_like"
      glue("had type `{attrs$type}` and did not pass `{fn}`.")
    }
  )
)

#' @rdname test-numeric
#' @export
assert_double <- fn_core_to_assert(
  core_double,
  msgs_add = list(
    type = \(attrs) {
      fn <- if (attrs$mode == "double") "is_double" else "is_numeric"
      glue("had type `{attrs$type}` and did not pass `{fn}`.")
    }
  )
)

#' @rdname test-numeric
#' @export
assert_complex <- fn_core_to_assert(
  core_complex,
  msgs_add = list(
    type = \(attrs) {
      glue("had type `{attrs$type}` and did not pass `is_complex`.")
    },
    re_tests = \(attrs) {
      glue("real component (`Re`) failed `test_double` for tests: {glue_collapse(attrs$tests, sep = ', ')}.")
    },
    im_tests = \(attrs) {
      glue("imaginary component (`Im`) failed `test_double` for tests: {glue_collapse(attrs$tests, sep = ', ')}.")
    },
    mod_tests = \(attrs) {
      glue("modulus component (`Mod`) failed `test_double` for tests: {glue_collapse(attrs$tests, sep = ', ')}.")
    },
    arg_tests = \(attrs) {
      glue("argument component (`Arg`) failed `test_double` for tests: {glue_collapse(attrs$tests, sep = ', ')}.")
    }
  )
)
# TODO: pluralize 'tests' (?)
