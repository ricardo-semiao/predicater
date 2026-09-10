
# TODO: is_code with: is_code args, valid, sentinels, custom
# is_code <- function(x, sym = TRUE, call = TRUE, literal = TRUE) {
#   (sym && is_symbol(x)) ||
#     (call && is_language(x)) ||
#     (literal && is_syntactic_literal(x))
# }


#' Tests - Expression
#'
#' @description
#' Test if an object is an expression vector or expression object.
#'
#' `test_expression()` is the predicate test, while `assert_expression()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len,null_n,call_n,sym_n,literal_n,depth_n
#'   `r ROXY$x_n("len,null_n,call_n,sym_n,literal_n,depth_n")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#'
#' @returns `r ROXY$test_returns("expression")`
#'
#' @name tests-expression
NULL

core_expression <- function(
  x,
  len = NULL, null_n = NULL, call_n = NULL, sym_n = NULL, literal_n = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type", sentinels, len, null_n, call_n, sym_n, literal_n, custom, custom_map
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_expression2(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  l <- length(x)
  x_list <- as.list(x)

  tests$len <- test_in_range(l, len, l) %@@%
    c(n = l)
  tests$null_n <- test_in_range(n_null <- sum(vapply_lgl(x_list, is_null)), null_n, l) %@@%
    c(n = n_null)
  tests$call_n <- test_in_range(n_call <- sum(vapply_lgl(x_list, is_call)), call_n, l) %@@%
    c(n = n_call)
  tests$sym_n <- test_in_range(n_sym <- sum(vapply_lgl(x_list, is_symbol)), sym_n, l) %@@%
    c(n = n_sym)
  tests$literal_n <- test_in_range(n_lit <- sum(vapply_lgl(x_list, is_syntactic_literal)), literal_n, l) %@@%
    c(n = n_lit)
  tests$custom <- test_custom(x, custom, env)
  tests$custom_map <- test_custom_map(x_list, custom_map, env)

  tests
}



# Symbol -----------------------------------------------------------------------

#' Tests - Symbol
#'
#' @description
#' Test if an object is a symbol (name).
#'
#' `test_symbol()` is the predicate test, while `assert_symbol()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param char_n `r ROXY$x_n("char_n")`
#' @param valid \[`logical(1)` | `NULL`] Test if the symbol name is a valid
#'   syntactic R name (i.e. unchanged when processed by [make.names()]).
#'   Set to `NULL` to not test.
#' @param is_in,seen_in \[`environment` | `NULL`] Environment in which the
#'   symbol exists directly, or inherited from one of its parents, respectively
#'   (see [rlang::env_has()]). Set to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#'
#' @returns `r ROXY$test_returns("symbol")`
#'
#' @name tests-symbol
NULL


core_symbol <- function(
  x,
  char_n = NULL, valid = NULL, is_in = NULL, seen_in = NULL,
  sentinels = NULL, custom = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type", sentinels, char_n, valid, is_in, seen_in, custom
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_symbol(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  sym_str <- as_string(x)
  n_char <- nchar(sym_str)

  tests$char_n <- test_in_range(n_char, char_n, n_char) %@@%
    c(n = n_char)
  tests$valid <- if (! is_null(valid)) {
    (make.names(sym_str) == sym_str) == valid
  }
  tests$is_in <- test_env_has(is_in, sym_str, inherit = FALSE)
  tests$seen_in <- test_env_has(seen_in, sym_str, inherit = TRUE)
  tests$custom <- test_custom(x, custom, env)

  tests
}



# Call -------------------------------------------------------------------------

#' Tests - Call
#'
#' @description
#' Test if an object is a call (language object).
#'
#' `test_call()` is the predicate test, while `assert_call()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param name,ns \[`character(1)` | `NULL`] Expected function name and
#'   namespace of the call, via [rlang::call_name()] and [rlang::call_ns()]. Set
#'   to `NULL` to not test.
#' @param args_n `r ROXY$x_n("args_n")`
#' @param arg_names \[`character()` | `NULL`] Expected exact names of the call
#'   arguments. Set to `NULL` to not test.
#' @param simple \[`TRUE` | `FALSE` | `NULL`] Test if the call is simple via
#'   [is_call_simple()]. Set to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#'
#' @returns `r ROXY$test_returns("call")`
#'
#' @name tests-call
NULL


core_call <- function(
  x,
  name = NULL, ns = NULL, args_n = NULL, arg_names = NULL,
  simple = NULL, sentinels = NULL, custom = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type", sentinels, name, ns, args_n, arg_names,
    simple, custom
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_call(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  n_args <- length(x) - 1L

  tests$name <- if (! is_null(name)) identical(call_name(x), name)
  tests$ns <- if (! is_null(ns)) identical(call_ns(x), ns)
  tests$args_n <- test_in_range(n_args, args_n, n_args) %@@% c(n = n_args)
  tests$arg_names <- test_arg_names(x, arg_names)
  tests$simple <- if (!is_null(simple)) is_call_simple(x) == simple
  tests$custom <- test_custom(x, custom, env)

  tests
}
# TODO: valid via is_parseable



# Helpers ----------------------------------------------------------------------

test_arg_names <- function(x, arg_names) {
  if (is_null(arg_names)) {
    return(NULL)
  }

  actual_names <- names(call_args(x)) %||% character(length(x) - 1L)
  identical(actual_names, arg_names)
}
