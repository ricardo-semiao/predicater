
#' @include validation-helpers.R validation-menu.R
NULL



# Other types ------------------------------------------------------------------

#' Validation - Other types
#'
#' @description
#' Test if an object is of various special base R types, including promises,
#' dots (`...`), weak references, bytecode, or external pointers.
#'
#' @param x `r ROXY$x()`
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
#' @examples
#' try(assert_null(integer(0))) #> Error
#'
#' @name tests-other_types
NULL

core_null <- function(
  x,
  short_circuit
) {
  sentinels <- NULL
  # CHECK: makeshift to allow run_tests to assume sentinels presence
  run_tests(
    x, sentinels,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, test_arg, params) is_null(x) %@@% list(type = typeof(x))
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
      type = \(x, test_arg, params) is_promise(x) %@@% list(type = typeof(x))
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
      type = \(x, test_arg, params) is_dots(x) %@@% list(type = typeof(x))
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
      type = \(x, test_arg, params) is_weakref(x) %@@% list(type = typeof(x))
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
      type = \(x, test_arg, params) is_bytecode(x) %@@% list(type = typeof(x))
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
      type = \(x, test_arg, params) is_externalptr(x) %@@% list(type = typeof(x))
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
  type = \(attrs, test) {
    glue2(
      "must be {.val NULL}.",
      fmt_postfix("Had type {.val [attrs$type]}.", test)
    )
  }
))

#' @rdname tests-other_types
#' @export
assert_promise <- fn_core_to_assert(core_promise, list(
  type = \(attrs, test) {
    glue2(
      "must be of type {.val promise}.",
      fmt_postfix("Had type {.val [attrs$type]}.", test)
    )
  }
))

#' @rdname tests-other_types
#' @export
assert_dots <- fn_core_to_assert(core_dots, list(
  type = \(attrs, test) {
    glue2(
      "must be of type {.val ...}.",
      fmt_postfix("Had type {.val [attrs$type]}.", test)
    )
  }
))

#' @rdname tests-other_types
#' @export
assert_weakref <- fn_core_to_assert(core_weakref, list(
  type = \(attrs, test) {
    glue2(
      "must be of type {.val weakref}.",
      fmt_postfix("Had type {.val [attrs$type]}.", test)
    )
  }
))

#' @rdname tests-other_types
#' @export
assert_bytecode <- fn_core_to_assert(core_bytecode, list(
  type = \(attrs, test) {
    glue2(
      "must be of type {.val bytecode}.",
      fmt_postfix("Had type {.val [attrs$type]}.", test)
    )
  }
))

#' @rdname tests-other_types
#' @export
assert_externalptr <- fn_core_to_assert(core_externalptr, list(
  type = \(attrs, test) {
    glue2(
      "must be of type {.val externalptr}.",
      fmt_postfix("Had type {.val [attrs$type]}.", test)
    )
  }
))



# Function ---------------------------------------------------------------------

#' Validation - Function
#'
#' @description
#' Test if an object is a function, with options to inspect function type, argument
#' names, environments, S3 generics, and methods.
#'
#' @param x `r ROXY$x()`
#' @param mode \[`"function"` | `"closure"` | `"builtin"` | `"special"` |
#'   `"primitive"` | `NULL`]
#'   Expected function type(s) out of `"function"` (any function), `"closure"`,
#'   `"builtin"`, `"special"`, or `"primitive"` (any of the former two). Set to
#'   `NULL` to not test.
#' @param args_names \[`character()` | `NULL`] Expected exact argument names of
#'   the function. Set to `NULL` to not test.
#' @param fn_env \[`environment` | `NULL`] Expected environment of the function.
#'   Checks if `fn_env(x)` is identical to `env` or inherits from it via
#'   [rlang::env_inherits()]. Set to `NULL` to not test.
#' @param dots \[`TRUE` | `FALSE` | `NULL`] Test if the function has `...` in
#'   its arguments. Set to `NULL` to not test.
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
#' @examples
#' x <- function(a, b = 1, ...) {
#'   a + b
#' }
#'
#' args <- list(
#'   mode = "closure",            # Must be a standard closure function (will pass)
#'   args_names = c("a", "b"),    # Exact argument names must match (will fail)
#'   fn_env = rlang::global_env(),
#'   # Function environment must equal or inherit from global env (will pass)
#'   dots = TRUE,                 # Function must accept `...` in arguments (will pass)
#'   sentinels = c("null"),       # Allow NULL function (not the case of x)
#'   custom = NULL                # No custom predicate
#' )
#'
#' do.call(test_function, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_function, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_function
NULL

core_function <- function(
  x,
  mode = "function", args_names = NULL, fn_env = NULL, dots = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, args_names, fn_env, dots, custom,
    tests_pars = list(mode = mode), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        test_fn_type(x, pars$mode) %@@% list(mode = pars$mode)
      },
      args_names = \(x, arg, pars) {
        args <- fn_fmls_names(x)
        identical(args, arg) %@@% list(arg = arg, args = args)
      },
      fn_env = \(x, arg, pars) test_fn_env(x, arg),
      dots = \(x, arg, pars) {
        has_dots <- "..." %in% fn_fmls_names(x)
        (has_dots == arg) %@@% list(arg = arg, dots = has_dots)
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
  type = \(attrs, test) {
    glue2(
      "must be of type {.val [attrs$mode]}.",
      fmt_postfix("Had type {.val [attrs$type]}.", test)
    )
  },
  args_names = \(attrs, test) {
    glue2(
      "must have argument names [fmt_vec(attrs$arg)].",
      fmt_postfix("Had [fmt_vec(attrs$args)].", test)
    )
  },
  fn_env = \(attrs, test) {
    glue2("must have the expected environment.")
  },
  dots = \(attrs, test) {
    glue2(
      "must [if (attrs$arg) 'accept' else 'not accept'] {.code ...}.",
      fmt_postfix("Did [if (attrs$dots) 'accept' else 'not accept'].", test)
    )
  }
))



# Helpers ----------------------------------------------------------------------

test_fn_type <- function(x, type) {
  type_x <- typeof(x)
  switch(type,
    "function" = type_x %in% c("closure", "builtin", "special"),
    primitive = type_x %in% c("builtin", "special"),
    builtin = type_x == "builtin",
    special = type_x == "special",
    closure = type_x == "closure"
  ) %@@%
    list(type = type_x)
}

test_fn_env <- function(x, target_env) {
  if (is_primitive(x)) {
    identical(target_env, base_env())
  }

  fn_e <- fn_env(x)
  identical(fn_e, target_env) || env_inherits(fn_e, target_env)
}
