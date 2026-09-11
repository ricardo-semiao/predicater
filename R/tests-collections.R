
#' @include tests-helpers.R tests-menu.R
NULL



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
#' @param mode \[`character()` | `NULL`] Expected vector type(s) out of `"list"` or
#'   `"pairlist"`. Set to `NULL` to not test.
#' @param len,null_n,empty_n,dup_n `r ROXY$x_n("len,null_n,empty_n,dup_n")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#'
#' @returns `r ROXY$test_returns("list")`
#'
#' @name test_list
NULL

core_list <- function(
  x,
  mode = "list", len = NULL, null_n = NULL, empty_n = NULL, dup_n = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, mode, len, null_n, empty_n, dup_n, custom, custom_map,
    tests_pars = list(l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        mode <- pars$mode
        switch(mode, list = is_list(x), pairlist = is_pairlist(x)) %@@%
          c(type = typeof(x), mode = mode)
      },
      dup_n = \(x, arg, pars) {
        test_in_range(n_dups <- sum(duplicated(as.list(unclass(x)))), arg, pars$l) %@@%
          c(n = n_dups)
      }
    )
  )
}
# TODO: see if other ops are easily generic for lists (e.g. ordered, set)

#' @rdname test_list
#' @export
test_list <- fn_core_to_test(core_list)

#' @rdname test_list
#' @export
assert_list <- fn_core_to_assert(core_list, list(
  type = \(attrs) glue("is not of type {.val {attrs$mode}}.")
))



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
#' @param env_has,env_sees \[`character()` | `NULL`] Symbol names that must exist
#'   directly in `x`, or inherited from one of its parents, respectively (see
#'   [rlang::env_has()]). Set to `NULL` to not test.
#' @param parents \[`environment()` | `list()` | `NULL`] Environment or list of
#'   environments to test as parents of `x`. If a single environment is
#'   supplied, tests if `x` inherits from it. If a list is supplied, tests if
#'   [rlang::env_parents()] matches the list identically. Set to `NULL` to not
#'   test.
#' @param namespace \[`TRUE` | `FALSE` | `NULL`] Test if `x` is a namespace
#'   environment via [rlang::is_namespace()]. Set to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#'
#' @returns `r ROXY$test_returns("environment")`
#'
#' @name test_environment
NULL

core_environment <- function(
  x,
  len = NULL, has = NULL, sees = NULL, parents = NULL, namespace = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  l <- length(x)

  run_tests(
    x, sentinels, len, namespace, parents, has, sees, custom, custom_map,
    tests_pars = list(l = l), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_environment(x) %@@% c(type = typeof(x)),
      namespace = \(x, arg, pars) is_namespace(x) == arg,
      parents = \(x, arg, pars) test_env_parents(x, arg)
    )
  )
}

#' @rdname test_environment
#' @export
test_environment <- fn_core_to_test(core_environment)

#' @rdname test_environment
#' @export
assert_environment <- fn_core_to_assert(
  core_environment,
  msgs_add = list(
    type = \(attrs) "must be an environment.",
    namespace = \(attrs) "namespace status check failed.",
    parents = \(attrs) "does not match expected environment parents."
  )
)



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
