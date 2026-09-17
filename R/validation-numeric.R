
#' @include validation-helpers.R validation-menu.R
NULL



# Core functions ---------------------------------------------------------------

#' Validation - Numeric vectors
#'
#' @description
#' Test if an object is a numeric vector:
#' - `test_integer(x, mode = "strict")` tests for integer vectors.
#' - `test_integer(x, mode = *)` tests for integer-like vectors via
#'   [is_integer_like()], with "bounded" or "unbounded" mode.
#' - `test_double(x, mode = "double")` tests for double vectors.
#' - `test_double(x, mode = "numeric")` tests for double or integer vectors.
#' - `test_complex()` tests for complex vectors, delegating tests on its real,
#'   imaginary, modulus, and argument parts, to `test_double()`.
#'
#' They all are predicate tests, while the `assert_*()` functions validate their
#' input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param mode
#' - \[`"strict"` | `"bounded"` | `"unbounded"`] For `*_integer()`: the `mode`
#'   argument to pass to [is_integer_like()].
#' - \[`"double"` | `"numeric"`] For `*_double()`: `"double"` to accept only
#'   [double()] vectors, or `"numeric"` to accept both [double()] and
#'   [integer()] vectors.
#' @param len,n_na,n_dup,n_nan,n_inf `r ROXY$x_n("len,n_na,n_dup,n_nan,n_inf")`
#' @param range \[`numeric()` | `NULL`] A vector with the upper and lower bound
#'   for `x` values. Or a vector with three or more values to test for `all(x
#'   %in% range)`. Set to `NULL` to not test.
#' @param set `r ROXY$set("integer")`
#' @param mode_tol \[`double(1)`] The `tol` argument to pass to
#'   [is_integer_like()].
#' @param sentinels `r ROXY$sentinels()`
#' @param sorted `r ROXY$sorted()`
#' @param custom `r ROXY$custom()`
#' @param tests_re,tests_im,tests_mod,tests_arg \[`list()` | `NULL`] For
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
#' @examples
#' # Example 1: Integer-like vector with a floating-point tolerance error
#' x <- c(1.0, 2.0, 3.0 + 1e-100, 4.0, Inf)
#'
#' args <- list(
#'   mode = "unbounded",  # Allow double-precision whole numbers and Inf (will fail on 3.00001)
#'   mode_tol = sqrt(.Machine$double.eps),
#'   # Standard tolerance for decimal check
#'   len = c(1, 10),          # Length must be between 1 and 10 (will pass)
#'   n_na = 0,                # No NA values allowed (will pass)
#'   n_dup = 0,               # No duplicate values allowed (will pass)
#'   n_nan = 0,               # No NaN values allowed (will pass)
#'   n_inf = 0,               # At most 1 Inf value allowed (will pass)
#'   range = c(0, 10 ),       # Bounds between 0 and Inf (will fail)
#'   set = list(no = c(0)),   # Values must not contain 0 (will pass)
#'   sentinels = c("null"),   # Allow NULL x (not the case of x)
#'   sorted = "desc",         # Must be sorted in decreasing order (will pass)
#'   custom = NULL            # No custom predicate
#' )
#'
#' do.call(test_integer, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_integer, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#'
#' # Example 2: Testing an integer vector under "numeric" mode
#' x <- c(1L, 2L, 5L, 10L)
#'
#' args <- list(
#'   mode = "numeric",                 # Accepts both double and integer vectors (will pass)
#'   len = c(1, 10),                   # Length must be between 1 and 10 (will pass)
#'   n_na = 0,                         # No NA values allowed (will pass)
#'   n_dup = 0,                        # No duplicate values allowed (will pass)
#'   n_nan = 0,                        # No NaN values allowed (will pass)
#'   n_inf = 0,                        # No Inf values allowed (will pass)
#'   range = c(1, 100),                # Values between 1 and 100 (will pass)
#'   set = list(yes = c(1L, 2L, 3L)),  # Elements must belong to specified set (will fail)
#'   sentinels = c("null"),            # Allow NULL x (not the case of x)
#'   sorted = "asc",                   # Must be sorted in ascending order (will pass)
#'   custom = NULL                     # No custom predicate
#' )
#'
#' do.call(test_double, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_double, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name tests-numeric
NULL


core_integer <- function(
  x, mode = "strict",
  len = NULL, n_na = NULL, n_dup = NULL, n_nan = NULL, n_inf = NULL,
  range = NULL, set = NULL, sorted = NULL,
  custom = NULL, sentinels = NULL,
  mode_tol = 0,
  short_circuit
) {
  run_tests(
    x, sentinels, len, n_na, n_dup, n_nan, n_inf, range, set, sorted, custom,
    tests_pars = list(l = length(x), mode = mode, mode_tol = mode_tol), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        mode <- pars$mode
        tol <- pars$mode_tol
        if (mode == "strict") {
          is_integer(x)
        } else {
          is_integer_like(x, mode = mode, tol = tol)
        } %@@%
          list(mode = mode, type = typeof(x), tol = tol)
      }
    )
  )
}


core_double <- function(
  x, mode = "double",
  len = NULL, n_na = NULL, n_dup = NULL, n_nan = NULL, n_inf = NULL,
  range = NULL, set = NULL, sorted = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, n_na, n_dup, n_nan, n_inf, range, set, sorted, custom,
    tests_pars = list(l = length(x), mode = mode), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        mode <- pars$mode
        if (mode == "double") {
          is_double(x)
        } else {
          is_numeric(x)
        } %@@%
          list(mode = mode, type = typeof(x))
      }
    )
  )
}


core_complex <- function(
  x,
  tests_re = NULL, tests_im = NULL, tests_mod = NULL, tests_arg = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, tests_re, tests_im, tests_mod, tests_arg, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_complex(x) %@@% list(type = typeof(x)),
      tests_re = \(x, arg, pars) {
        exec(test_double, x = Re(x), !!!arg) %@@%
          list(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      },
      tests_im = \(x, arg, pars) {
        exec(test_double, x = Im(x), !!!arg) %@@%
          list(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      },
      tests_mod = \(x, arg, pars) {
        exec(test_double, x = Mod(x), !!!arg) %@@%
          list(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      },
      tests_arg = \(x, arg, pars) {
        exec(test_double, x = Arg(x), !!!arg) %@@%
          list(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      }
    )
  )
}
# TODO: move len and na tests to first-level tests



# Functions --------------------------------------------------------------------

# Test functions:

#' @rdname tests-numeric
#' @export
test_integer <- fn_core_to_test(core_integer)

#' @rdname tests-numeric
#' @export
test_double <- fn_core_to_test(core_double)

#' @rdname tests-numeric
#' @export
test_complex <- fn_core_to_test(core_complex)


# Assert functions:

#' @rdname tests-numeric
#' @export
assert_integer <- fn_core_to_assert(
  core_integer,
  msgs_add = list(
    type = \(attrs, test) {
      if (attrs$mode == "strict") {
        glue2(
          "must pass {.fn predicater::is_integer}().",
          fmt_postfix("Had type {.val [attrs$type]}.", test)
        )
      } else {
        glue2(
          "must pass {.fn predicater::is_integer_like}() in {.val [attrs$mode]} mode.",
          fmt_postfix("Had type {.val [attrs$type]}.", test)
        )
      }
    }
  )
)

#' @rdname tests-numeric
#' @export
assert_double <- fn_core_to_assert(
  core_double,
  msgs_add = list(
    type = \(attrs, test) {
      fn <- if (attrs$mode == "double") {
        "predicater::is_double"
      } else {
        "predicater::is_numeric"
      }
      glue2(
        "must pass {.fn [fn]}.",
        fmt_postfix("Had type {.val [attrs$type]}.", test)
      )
    }
  )
)

msg_tests_complex <- function(x) {
  \(attrs, test) {
    ts <- if (length(attrs$tests) == 1) "test" else "tests"
    glue2(
      "[x] must pass custom {.fn predicater::test_double} [ts].",
      fmt_postfix("Failed: [fmt_vec(attrs$tests)].", test),
      x = x
    )
  }
}

#' @rdname tests-numeric
#' @export
assert_complex <- fn_core_to_assert(
  core_complex,
  msgs_add = list(
    type = \(attrs, test) {
      glue2(
        "must pass {.fn predicater::is_complex}().",
        fmt_postfix("Had type {.val [attrs$type]}.", test)
      )
    },
    tests_re = msg_tests_complex("real component ({.fn Re})"),
    tests_im = msg_tests_complex("imaginary component ({.fn Im})"),
    tests_mod = msg_tests_complex("modulus component ({.fn Mod})"),
    tests_arg = msg_tests_complex("argument component ({.fn Arg})")
  )
)
