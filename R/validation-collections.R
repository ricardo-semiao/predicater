
#' @include validation-helpers.R validation-menu.R
NULL



# Lists ------------------------------------------------------------------------

#' Validation - List
#'
#' @description
#' Test if an object is a list.
#'
#' `test_list()` is the predicate test, while `assert_list()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param mode \[`character()` | `NULL`] Expected vector type(s) out of `"list"` or
#'   `"pairlist"`. Set to `NULL` to not test.
#' @param len,n_null,n_empty,n_dup `r ROXY$x_n("len,n_null,n_empty,n_dup")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("list")`
#'
#' @examples
#' x <- list(a = 1, b = NULL, c = integer(0), d = 1)
#'
#' args <- list(
#'   mode = "list",               # Must be a standard list (will pass)
#'   len = c(1, 10),              # Length must be between 1 and 10 (will pass)
#'   n_null = 0,                  # No NULL elements allowed (will fail)
#'   n_empty = c(0, 1),           # At most 1 empty element allowed (will pass)
#'   n_dup = 0,                   # No duplicate elements allowed (will fail)
#'   sentinels = c("null"),       # Allow NULL list (not the case of x)
#'   custom = \(x) is.list(x),    # Must be a list (will pass)
#'   custom_map = \(elt) !is.na(elt)
#'   # All list elements must be non-NA (will pass)
#' )
#'
#' do.call(test_list, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_list, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_list
NULL

core_list <- function(
  x,
  mode = "list", len = NULL, n_null = NULL, n_empty = NULL, n_dup = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, n_null, n_empty, n_dup, custom, custom_map,
    tests_pars = list(mode = mode, l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        mode <- pars$mode
        switch(mode, list = is_list(x), pairlist = is_pairlist(x)) %@@%
          list(type = typeof(x), mode = mode)
      },
      n_dup = \(x, arg, pars) {
        test_in_range(
          n_dups <- sum(duplicated(as.list(unclass(x)))), arg, pars$l
        ) %@@% list(arg = arg, n = n_dups)
      }
    )
  )
}
# TODO: see if other ops are easily generic for lists (e.g. ordered, set)
# TODO: add depth_n for lists/language
# TODO: add custom_recurse for lists/language

#' @rdname test_list
#' @export
test_list <- fn_core_to_test(core_list)

#' @rdname test_list
#' @export
assert_list <- fn_core_to_assert(
  core_list,
  list(
    type = \(attrs, test) {
      glue2(
        "must be of type {.val [attrs$mode]}.",
        fmt_postfix("Had type {.val [attrs$type]}.", test)
      )
    }
  )
)



# Environment ------------------------------------------------------------------

#' Validation - Environment
#'
#' @description
#' Test if an object is an environment.
#'
#' `test_environment()` is the predicate test, while `assert_environment()`
#' validates its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param len `r ROXY$x_n("len")`
#' @param env_has,env_sees \[`character()` | `NULL`] Symbol names that must exist
#'   directly in `x`, or inherited from one of its parents, respectively (see
#'   [rlang::env_has()]). Set to `NULL` to not test.
#' @param parents \[`environment()` | `list()` | `NULL`]
#'   Environment or list of environments to test as parents of `x`. If a single
#'   environment is supplied, tests if `x` inherits from it. If a list is
#'   supplied, tests if [rlang::env_parents()] matches the list identically. Set
#'   to `NULL` to not test.
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
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("environment")`
#'
#' @examples
#' x <- rlang::new_environment(list(a = 1, b = 2), parent = rlang::global_env())
#'
#' args <- list(
#'   len = c(1, 5),         # Length (number of bindings) must be between 1 and 5 (will pass)
#'   env_has = c("a", "b"), # Environment must directly bind "a" and "b" (will pass)
#'   env_sees = "__x__",    # Environment or its parents must see symbol "__x__" (will fail)
#'   parents = rlang::global_env(),
#'   # Must inherit from the global environment (will pass)
#'   namespace = TRUE,      # Must be a package namespace (will fail)
#'   sentinels = c("null"), # Allow NULL environment (not the case of x)
#'   custom = NULL,         # Not test
#'   custom_map = \(val) is.numeric(val)
#'   # All binding values must be numeric (will pass)
#' )
#'
#' do.call(test_environment, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_environment, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_environment
NULL

core_environment <- function(
  x,
  len = NULL, env_has = NULL, env_sees = NULL, parents = NULL, namespace = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  l <- length(x)

  run_tests(
    x, sentinels, len, namespace, parents, env_has, env_sees, custom, custom_map,
    tests_pars = list(l = l), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        is_environment(x) %@@% list(type = typeof(x))
      },
      namespace = \(x, arg, pars) {
        is <- is_namespace(x)
        is == arg %@@% list(arg = arg, is = is)
      },
      parents = \(x, arg, pars) {
        test_env_parents(x, arg)
      }
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
    type = \(attrs, test) {
      glue2(
        "must be of type {.val environment}.",
        fmt_postfix("Had type {.val [attrs$type]}.", test)
      )
    },
    namespace = \(attrs, test) {
      glue2(
        "{.code isNamespace(x)} must be {.val [attrs$arg]}.",
        fmt_postfix("Was {.val [attrs$namespace]}.", test)
      )
    },
    parents = \(attrs, test) {
      glue2(
        "must be child of specific parents.",
        fmt_postfix("Is not.", test)
      )
    }
  )
)



# Vector -----------------------------------------------------------------------

#' Validation - General vectors
#'
#' @description
#' Test if an input is a vector of a given mode (atomic, list, expression, or
#' pairlist) and optionally validate length, missing values, duplicate counts,
#' set inclusion, and ordering constraints.
#'
#' `test_vector()` is the predicate test, while `assert_vector()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param mode \[`"atomic"` | `"list"` | `"expression"` | `"pairlist"`]
#'   Allowed vector types. They are additive: `"atomic"` allows atomic vectors,
#'   `list` allows atomic and lists, `expression` allows atomic, lists, and
#'   expression objects, and `pairlist` allows all.
#' @param len,n_na,n_null,n_empty,n_dup `r ROXY$x_n("len,n_na,n_null,n_empty,n_dup")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("vector")`
#'
#' @examples
#' x <- list(1, 2, NA, 2, NULL)
#'
#' args <- list(
#'   mode = "list",          # Allow atomic vectors and lists (will pass)
#'   len = c(1, 10),         # Length must be between 1 and 10 (will pass)
#'   n_na = 0,               # No NA values allowed (will fail)
#'   n_null = c(0, 1),       # At most 1 NULL element allowed (will pass)
#'   n_empty = 0,            # No empty elements allowed (will pass)
#'   n_dup = 0,              # No duplicate elements allowed (will fail)
#'   sentinels = c("null"),  # Allow NULL x (not the case of x)
#'   custom = NULL,          # Not tested
#'   custom_map = \(elt) length(elt) <= 1
#'   # All list elements must have length <= 1 (will pass)
#' )
#'
#' do.call(test_vector, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_vector, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_vector
NULL

core_vector <- function(
  x, mode = "atomic",
  len = NULL, n_na = NULL, n_null = NULL, n_empty = NULL, n_dup = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, n_na, n_null, n_empty, n_dup,
    custom, custom_map,
    tests_pars = list(l = length(x), mode = mode), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        mode <- pars$mode
        switch(mode,
          atomic = is_atomic(x),
          list = is_atomic(x) || typeof(x) == "list",
          expression = is_atomic(x) || typeof(x) %in% c("list", "expression"),
          pairlist = is_atomic(x) || typeof(x) %in% c("list", "expression", "pairlist")
        ) %@@%
          list(type = typeof(x), mode = mode)
      },
      n_na = \(x, arg, pars) {
        if (pars$mode != "atomic") {
          return(TRUE %@@% list(arg = arg, n = 0))
        }
        test_in_range(n_na <- sum(are_na2(x, nan = FALSE)), arg, pars$l) %@@%
          list(arg = arg, n = n_na)
      }
    )
  )
}
# CHECK: we can create a atomic = T/F, list = T/F, ... scheme, for more control

#' @rdname test_vector
#' @export
test_vector <- fn_core_to_test(core_vector)

#' @rdname test_vector
#' @export
assert_vector <- fn_core_to_assert(
  core_vector,
  msgs_add = list(
    type = \(attrs, test) {
      glue2(
        "must be of 'type' {.val [attrs$mode]}.",
        fmt_postfix("Had type {.val [attrs$type]}.", test)
      )
    }
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
