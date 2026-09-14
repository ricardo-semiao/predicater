
#' @include validation-helpers.R validation-menu.R
NULL



# Other types ------------------------------------------------------------------

#' Validation - Other types
#'
#' @description
#' Test if an object is of various special base R types, including promises,
#' dots (`...`), weak references, bytecode, or external pointers.
#'
#' @param x \[`any`] An object to test.
#' @param len `r ROXY$x_n("len")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns()`
#'
#' @name tests-other_types
NULL

core_null <- function(
  x,
  short_circuit
) {
  run_tests(
    x,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, test_arg, params) is_null(x) %@@% c(type = typeof(x))
    )
  )
}

core_promise <- function(
  x, sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, test_arg, params) is_promise(x) %@@% c(type = typeof(x))
    )
  )
}

core_dots <- function(
  x, len = NULL, sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, custom,
    tests_pars = list(l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, test_arg, params) is_dots(x) %@@% c(type = typeof(x))
    )
  )
}
# CHECK: consider some test about the names (contains, all named, etc.). This
# can be left to an additional names test, as usual

core_weakref <- function(
  x, sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, test_arg, params) is_weakref(x) %@@% c(type = typeof(x))
    )
  )
}

core_bytecode <- function(
  x, sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, test_arg, params) is_bytecode(x) %@@% c(type = typeof(x))
    )
  )
}

core_externalptr <- function(
  x, sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, test_arg, params) is_externalptr(x) %@@% c(type = typeof(x))
    )
  )
}


# Test functions:

#' @rdname tests-other_types
#' @export
test_promise <- fn_core_to_test(core_promise)

#' @rdname tests-other_types
#' @export
test_dots <- fn_core_to_test(core_dots)

#' @rdname tests-other_types
#' @export
test_weakref <- fn_core_to_test(core_weakref)

#' @rdname tests-other_types
#' @export
test_bytecode <- fn_core_to_test(core_bytecode)

#' @rdname tests-other_types
#' @export
test_externalptr <- fn_core_to_test(core_externalptr)


# Assert functions:

#' @rdname tests-other_types
#' @export
assert_null <- fn_core_to_assert(core_null, list(
  type = \(x, test_arg, params) glue("`{x}` is not `NULL`")
))

#' @rdname tests-other_types
#' @export
assert_promise <- fn_core_to_assert(core_promise, list(
  type = \(x, test_arg, params) glue("`{x}` is not of type {.val promise}.")
))

#' @rdname tests-other_types
#' @export
assert_dots <- fn_core_to_assert(core_dots, list(
  type = \(x, test_arg, params) glue("`{x}` is not of type {.val ...}.")
))

#' @rdname tests-other_types
#' @export
assert_weakref <- fn_core_to_assert(core_weakref, list(
  type = \(x, test_arg, params) glue("`{x}` is not of type weak reference.")
))

#' @rdname tests-other_types
#' @export
assert_bytecode <- fn_core_to_assert(core_bytecode, list(
  type = \(x, test_arg, params) glue("`{x}` is not bytecode.")
))

#' @rdname tests-other_types
#' @export
assert_externalptr <- fn_core_to_assert(core_externalptr, list(
  type = \(x, test_arg, params) glue("`{x}` is not an external pointer.")
))



# Function ---------------------------------------------------------------------

#' Validation - Function
#'
#' @description
#' Test if an object is a function, with options to inspect function type, argument
#' names, environments, S3 generics, and methods.
#'
#' @param x \[`any`] An object to test.
#' @param mode \[`character()` | `NULL`] Expected function type(s) out of
#'   `"closure"`, `"primitive"`, `"builtin"`, or `"special"`. Set to `NULL` to
#'   not test.
#' @param args_names \[`character()` | `NULL`] Expected exact argument names of
#'   the function. Set to `NULL` to not test.
#' @param fn_env \[`environment` | `NULL`] Expected environment of the function.
#'   Checks if `fn_env(x)` is identical to `env` or inherits from it via
#'   [rlang::env_inherits()]. Set to `NULL` to not test.
#' @param dots \[`TRUE` | `FALSE` | `NULL`] Test if the function has `...` in
#'   its arguments. Set to `NULL` to not test.
#' @param generic,method \[`TRUE` | `FALSE` | `NULL` each] Test if the function
#'   is an S3 generic or method via
#'   [sloop::is_s3_generic()]/[sloop::is_s3_method()]. Set to `NULL` to not
#'   test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("function")`
#'
#' @name test_function
NULL

core_function <- function(
  x,
  mode = NULL, args_names = NULL, fn_env = NULL, dots = NULL,
  generic = NULL, method = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, args_names, env, dots, generic, method, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) test_fn_type(x, arg),
      args_names = \(x, arg, pars) identical(fn_fmls_names(x), arg),
      fn_env = \(x, arg, pars) test_fn_env(x, arg),
      dots = \(x, arg, pars) ("..." %in% fn_fmls_names(x)) == arg,
      generic = \(x, arg, pars) {
        if (check_installed2("sloop")) {
          sloop::is_s3_generic(x) == arg
        } else {
          cli_abort("Package {.pkg sloop} is required to run test {.arg generic}.")
        }
      },
      method <- \(x, arg, pars) {
        if (check_installed2("sloop")) {
          sloop::is_s3_method(x) == arg
        } else {
          cli_abort("Package {.pkg sloop} is required to run test {.arg method}.")
        }
      }
    )
  )
}

#' @rdname test_function
#' @export
test_function <- fn_core_to_test(core_function)

#' @rdname test_function
#' @export
assert_function <- fn_core_to_assert(core_function, list(
  type = \(x, test_arg, params) glue("`{x}` is not a function."),
  args_names = \(x, test_arg, params) {
    glue("`{x}` does not have the expected argument names.")
  },
  fn_env = \(x, test_arg, params) {
    glue("`{x}` does not have the expected environment.")
  },
  dots = \(x, test_arg, params) {
    glue("`{x}` does not have the expected `...` argument.")
  },
  generic = \(x, test_arg, params) {
    glue("`{x}` is not an S3 generic function.")
  },
  method = \(x, test_arg, params) {
    glue("`{x}` is not an S3 method function.")
  }
))



# Helpers ----------------------------------------------------------------------

test_fn_type <- function(x, type) {
  type_x <- typeof(x)
  switch(
    type,
    primitive = type_x %in% c("builtin", "special"),
    builtin = type_x == "builtin",
    special = type_x == "special",
    closure = type_x == "closure"
  ) %@@%
    c(type = type_x)
}

test_fn_env <- function(x, target_env) {
  if (is_primitive(x)) {
    identical(target_env, base_env())
  }

  fn_e <- fn_env(x)
  identical(fn_e, target_env) || env_inherits(fn_e, target_env)
}
