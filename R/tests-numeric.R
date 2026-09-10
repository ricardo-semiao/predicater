
#' @include tests-helpers.R
NULL


# Docs -------------------------------------------------------------------------

#' Tests - Numeric vectors
#'
#' @description
#' Test if an object is an numeric vector:
#' - `test_integer(x, mode = "strict")` tests for integer vectors.
#' - `test_integer_like(x, mode = *)`, tests for integer-like vectors via
#'   [is_integer_like()], with its multiple modes.
#' - `test_double(x)` tests for double vectors.
#' - `test_complex(x)` tests for complex vectors.
#'
#' They all are predicate tests, while the `assert_*()` functions validate their
#' input, aborting if it fails the test.
#'
#' @param x \[`any`] An  object to test.
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
#' @param re_tests,im_tests,mod_tests,arg_tests \[`list()` | `NULL`] For
#'   `*_complex()`: a list with the same named arguments as `test_double()`, to
#'   test the real, imaginary, modulus and argument values of `x`.
#'
#' @returns `r ROXY$test_returns()`
#'
#' @name tests-numeric
NULL



# Core functions ---------------------------------------------------------------

core_numeric <- function(
  x,
  len = NULL, na_n = NULL, dup_n = NULL, nan_n = NULL, inf_n = NULL,
  range = NULL, set = NULL, sorted = NULL, custom = NULL,
  env = caller_env(), tests = NULL
) {
  # Checks:
  # TODO:


  # Main:
  tests <- tests %||% initialize_tests(
    len, na_n, dup_n, nan_n, inf_n, range, set, sorted, custom
  )
  l <- length(x)

  tests$len <- test_in_range(l, len, l) %@@%
    c(n = l)
  tests$range <- test_in_range(x, range, l) %@@%
    c(range = range)
  tests$na_n <- test_in_range(n_na <- sum(are_na2(x, nan = "f")), na_n, l) %@@%
    c(n = n_na)
  tests$dup_n <- test_in_range(n_dups <- sum(duplicated(unclass(x))), dup_n, l) %@@%
    c(n = n_dups) # TODO: incomparables = NA_integer_?
  tests$nan_n <- test_in_range(n_nan <- sum(are_nan(x, na = "f")), nan_n, l) %@@%
    c(n = n_nan)
  tests$inf_n <- test_in_range(n_inf <- sum(are_inf(x, na = "f")), inf_n, l) %@@%
    c(n = n_inf)
  tests$sorted <- test_sorted(x, sorted) %@@%
    c(sorted = sorted)
  tests$set <- test_in_set(x, set)
  tests$custom <- test_custom(x, custom, env)

  tests
}


core_integer <- function(
  x, mode = "strict",
  len = NULL, na_n = NULL, dup_n = NULL, nan_n = NULL, inf_n = NULL,
  range = NULL, set = NULL, sorted = NULL,
  custom = NULL, sentinels = NULL,
  mode_tol = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type", sentinels, len, na_n, dup_n, nan_n, inf_n, range, set, sorted,
    custom
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE # Sentinels is not a 'failable' test

  tests$type <- if (mode == "strict") {
    is_integer(x)
  } else {
    if (is_null(mode_tol)) {
      is_integer_like(x, mode = mode)
    } else {
      is_integer_like(x, mode = mode, tol = mode_tol)
    }
  }
  tests$type <- tests$type %@@% c(mode = mode, type = typeof(x), tol = mode_tol)
  if (! tests$type) {
    return(tests)
  }

  tests_numeric <- core_numeric(
    x, len = len, na_n = na_n, dup_n = dup_n, nan_n = nan_n, inf_n = inf_n,
    range = range, set = set, sorted = sorted, custom = custom,
    env = env, tests = tests
  )

  c(tests, tests_numeric)
}


core_double <- function(
  x, mode = "double",
  len = NULL, na_n = NULL, dup_n = NULL, nan_n = NULL, inf_n = NULL,
  range = NULL, set = NULL, sorted = NULL,
  sentinels = NULL, custom = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    c("sentinels", "type"), len, na_n, dup_n, nan_n, inf_n, range, set, sorted,
    custom
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- if (mode == "double") {
    is_double(x)
  } else {
    is_numeric(x)
  }
  tests$type <- tests$type %@@% c(mode = mode, type = typeof(x))
  if (! tests$type) {
    return(tests)
  }

  tests_numeric <- core_numeric(
    x, len = len, na_n = na_n, dup_n = dup_n, nan_n = nan_n, inf_n = inf_n,
    range = range, set = set, sorted = sorted, custom = custom,
    env = env, tests = tests
  )

  c(tests, tests_numeric)
}


core_complex <- function(
  x,
  re_tests = NULL, im_tests = NULL, mod_tests = NULL, arg_tests = NULL,
  sentinels = NULL, custom = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    c("sentinels", "type"), re_tests, im_tests, mod_tests, arg_tests, custom
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_complex(x)
  tests$type <- tests$type %@@% c(type = typeof(x))
  if (! tests$type) {
    return(tests)
  }

  tests$custom <- test_custom(x, custom, env = env)

  tests$re_tests <- re_tests %&&% exec(test_double, x = Re(x), !!!re_tests) %@@%
    c(tests = names(re_tests)[vapply_lgl(re_tests, \(x) !is_null(x))])
  tests$im_tests <- im_tests %&&% exec(test_double, x = Im(x), !!!im_tests) %@@%
    c(tests = names(im_tests)[vapply_lgl(im_tests, \(x) !is_null(x))])
  tests$mod_tests <- mod_tests %&&% exec(test_double, x = Mod(x), !!!mod_tests) %@@%
    c(tests = names(mod_tests)[vapply_lgl(mod_tests, \(x) !is_null(x))])
  tests$arg_tests <- arg_tests %&&% exec(test_double, x = Arg(x), !!!arg_tests) %@@%
    c(tests = names(arg_tests)[vapply_lgl(arg_tests, \(x) !is_null(x))])

  tests
}



# Test and assert functions ----------------------------------------------------

#' @rdname tests-numeric
#' @export
test_integer <- fn_core_to_test(core_integer)

#' @rdname tests-numeric
#' @export
assert_integer <- fn_core_to_assert(core_integer, c(
  MSGS$sub_tests,
  type = \(attrs) {
    fn <- switch(attrs$mode, strict = "is_integer", "is_integer_like")
    glue("had type {{.val {attrs$type}}} and did not pass {{.fn {fn}}}.")
  }
))


#' @rdname tests-numeric
#' @export
test_double <- fn_core_to_test(core_double)

#' @rdname tests-numeric
#' @export
assert_double <- fn_core_to_assert(core_double, c(
  MSGS$sub_tests,
  type = \(attrs) {
    fn <- switch(attrs$mode, double = "is_integer", numeric = "is_numeric")
    glue("had type {{.val {attrs$type}}} and did not pass {{.fn {fn}}}.")
  }
))


#' @rdname tests-numeric
#' @export
test_complex <- fn_core_to_test(core_complex)

#' @rdname tests-numeric
#' @export
assert_complex <- fn_core_to_assert(core_complex, c(
  type = \(attrs) {
    glue("had type {{.val {attrs$type}}} and did not pass {{.fn is_complex}}.")
  },
  re_tests = \(attrs) {
    glue("its {{.fn Re}} value didn't passed {{.fn test_double}} with tests {{.val {attrs$tests}}}.")
  },
  im_tests = \(attrs) {
    glue("its {{.fn Im}} value didn't passed {{.fn test_double}} with tests {{.val {attrs$tests}}}.")
  },
  mod_tests = \(attrs) {
    glue("its {{.fn Mod}} value didn't passed {{.fn test_double}} with tests {{.val {attrs$tests}}}.")
  },
  arg_tests = \(attrs) {
    glue("its {{.fn Arg}} value didn't passed {{.fn test_double}} with tests {{.val {attrs$tests}}}.")
  },
  custom = MSGS$sub_tests$custom
))
# TODO: pluralize 'tests'



# Helpers ----------------------------------------------------------------------

# TODO: consider turning into is_* functions

test_sorted <- function(x, sorted) {
  if (is_null(sorted)) {
    return(NULL)
  }

  if (sorted == "asc") {
    !is.unsorted(x)
  } else if (sorted == "desc") {
    !is.unsorted(rev(x))
  }
}
# TODO: what to do with na.rm = TRUE?


test_in_set <- function(x, set, type_test = "integer") {
  type_tester <- switch(type_test,
    integer = is_integer,
    numeric = is_numeric
  )

  if (is_null(set)) {
    return(NULL)
  }

  if (type_tester(set)) {
    all(x %in% set)
  } else if (is_list(set)) {
    all(x %in% set$yes && ! x %in% set$no)
  }
}
# TODO: range and etc can be double, for to say len <= 1.1 (len < 1)
# TODO: this testing should be outside the function


test_in_range <- function(n, range, l, type_test = "integer") {
  type_tester <- switch(type_test,
    integer = \(x, n) is_integer_like(x, n, mode = "trunc"),
    numeric = is_numeric,
    # TODO: internal error
  )

  if (is_null(range)) {
    return(NULL)
  }

  if (type_tester(range, 1)) {
    all(n == range)
  } else if (type_tester(range, 2)) {
    all(n >= range[1] & n <= range[2])
  } else if (type_tester(range) && length(range) > 2) {
    all(n %in% range)
  } else if (is_function(range)) {
    range(n, l) # TODO: try catch also if not T/F
  } else {
    # TODO: err
  }
}
