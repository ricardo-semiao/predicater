
# Lists ------------------------------------------------------------------------

#' Tests - List
#'
#' @description
#' Test if an object is a list.
#'
#' `test_list()` is the predicate test, while `assert_list()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len,null_n,empty_n,dup_n `r ROXY$x_n("len,null_n,empty_n,dup_n")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#'
#' @returns `r ROXY$test_returns("list")`
#'
#' @name tests-list
NULL


core_list <- function(
  x,
  len = NULL, null_n = NULL, empty_n = NULL, dup_n = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type", sentinels, len, null_n, empty_n, dup_n, custom, custom_map
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_list(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  l <- length(x)

  tests$len <- test_in_range(l, len, l) %@@%
    c(n = l)
  tests$null_n <- test_in_range(n_null <- sum(vapply_lgl(x, is_null)), null_n, l) %@@%
    c(n = n_null)
  tests$empty_n <- test_in_range(n_empty <- sum(vapply_lgl(x, is_empty)), empty_n, l) %@@%
    c(n = n_empty)
  tests$dup_n <- test_in_range(n_dups <- sum(duplicated(x)), dup_n, l) %@@%
    c(n = n_dups)
  tests$custom <- test_custom(x, custom, env)
  tests$custom_map <- test_custom_map(x, custom_map, env)

  tests
}


core_pairlist <- function(
  x,
  len = NULL, null_n = NULL, empty_n = NULL, dup_n = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type", sentinels, len, null_n, empty_n, dup_n, custom, custom_map
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_pairlist(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  l <- length(x)

  tests$len <- test_in_range(l, len, l) %@@%
    c(n = l)
  tests$null_n <- test_in_range(n_null <- sum(vapply_lgl(x, is_null)), null_n, l) %@@%
    c(n = n_null)
  tests$empty_n <- test_in_range(n_empty <- sum(vapply_lgl(x, is_empty)), empty_n, l) %@@%
    c(n = n_empty)
  tests$dup_n <- test_in_range(n_dups <- sum(duplicated(as.list(x))), dup_n, l) %@@%
    c(n = n_dups)
  tests$custom <- test_custom(x, custom, env)
  tests$custom_map <- test_custom_map(x, custom_map, env)

  tests
}
# TODO: merge both and add a type 'list', 'pairlist', 'any'



# Environment ------------------------------------------------------------------

#' Tests - Environment
#'
#' @description
#' Test if an object is an environment.
#'
#' `test_environment()` is the predicate test, while `assert_environment()`
#' validates its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len `r ROXY$x_n("len")`
#' @param has,sees \[`character()` | `NULL`] Symbol names that must exist
#'   directly in `x`, or inherited from one of its parents, respectively (see
#'   [rlang::env_has()]). Set to `NULL` to not test.
#' @param parents \[`environment()` | `list()` | `NULL`] Environment or list of
#'   environments to test as parents of `x`. If a single environment is
#'   supplied, tests if `x` inherits from it. If a list is supplied, tests if
#'   [rlang::env_parents()] matches the list identically. Set to `NULL` to not
#'   test.
#' @param namespace \[`TRUE` | `FALSE` | `NULL`] Test if `x` is a namespace
#'   environment. Set to `NULL` to not test.
#' @param sees \[`character()` | `NULL`] Symbol names that must be visible from `x`
#'   (inheriting). Set to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#'
#' @returns `r ROXY$test_returns("environment")`
#'
#' @name tests-environment
NULL

core_environment <- function(
  x,
  len = NULL, has = NULL, sees = NULL, parents = NULL, namespace = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type", sentinels, len, namespace, parents,
    has, sees, custom, custom_map
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_environment(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  l <- length(x)

  tests$len <- test_in_range(l, len, l) %@@%
    c(n = l)
  tests$namespace <- if (!is_null(namespace)) is_namespace(x) == namespace
  tests$parents <- test_env_parents(x, parents)
  tests$has <- test_env_has(x, has, inherit = FALSE)
  tests$sees <- test_env_has(x, sees, inherit = TRUE)
  tests$custom <- test_custom(x, custom, env)
  tests$custom_map <- test_custom_map(x, custom_map, env)

  tests
}



# Helpers ----------------------------------------------------------------------

test_env_parents <- function(x, parents) {
  if (is_null(parents)) {
    return(NULL)
  }

  if (is_environment(parents)) {
    env_inherits(x, parents)
  } else {
    identical(env_parents(x), parents)
  }
}
