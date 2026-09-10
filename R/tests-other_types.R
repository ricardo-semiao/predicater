
# Other types ------------------------------------------------------------------

#' Tests - Other types
#'
#' @description
#' Test if an object is of various special base R types, including `NULL`,
#' promises, dots (`...`), weak references, bytecode, or external pointers.
#'
#' @param x \[`any`] An object to test.
#' @param len `r ROXY$x_n("len")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#'
#' @returns `r ROXY$test_returns()`
#'
#' @name tests-other_types
NULL

core_null <- function(x, sentinels = NULL, custom = NULL, env = caller_env()) {
  # Main:
  tests <- initialize_tests("type", sentinels, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests) 
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_null(x) %@@% c(type = typeof(x))
  if (!tests$type) return(tests)

  tests$custom <- test_custom(x, custom, env)
  tests
}

core_promise <- function(x, sentinels = NULL, custom = NULL, env = caller_env()) {
  # Main:
  tests <- initialize_tests("type", sentinels, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests) 
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_promise(x) %@@% c(type = typeof(x))
  if (!tests$type) return(tests)

  tests$custom <- test_custom(x, custom, env)
  tests
}

core_dots <- function(x, len = NULL, sentinels = NULL, custom = NULL, env = caller_env()) {
  # Main:
  tests <- initialize_tests("type", sentinels, len, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests) 
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_dots(x) %@@% c(type = typeof(x))
  if (!tests$type) return(tests)

  l <- length(x)
  tests$len <- test_in_range(l, len, l) %@@% c(n = l)
  tests$custom <- test_custom(x, custom, env)
  tests
}

core_weakref <- function(x, sentinels = NULL, custom = NULL, env = caller_env()) {
  # Main:
  tests <- initialize_tests("type", sentinels, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests) 
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_weakref(x) %@@% c(type = typeof(x))
  if (!tests$type) return(tests)

  tests$custom <- test_custom(x, custom, env)
  tests
}

core_bytecode <- function(x, sentinels = NULL, custom = NULL, env = caller_env()) {
  # Main:
  tests <- initialize_tests("type", sentinels, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests) 
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_bytecode(x) %@@% c(type = typeof(x))
  if (!tests$type) return(tests)

  tests$custom <- test_custom(x, custom, env)
  tests
}

core_externalptr <- function(x, sentinels = NULL, custom = NULL, env = caller_env()) {
  # Main:
  tests <- initialize_tests("type", sentinels, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests) 
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_externalptr(x) %@@% c(type = typeof(x))
  if (!tests$type) return(tests)

  tests$custom <- test_custom(x, custom, env)
  tests
}



# Function ---------------------------------------------------------------------

#' Tests - Function
#'
#' @description
#' Test if an object is a function, with options to inspect function type, argument
#' names, environments, S3 generics, and methods.
#'
#' @param x \[`any`] An object to test.
#' @param type \[`character()` | `NULL`] Expected function type(s) out of `"closure"`,
#'   `"primitive"`, `"builtin"`, or `"special"`. Set to `NULL` to not test.
#' @param arg_names \[`character()` | `NULL`] Expected exact argument names of the function.
#'   Set to `NULL` to not test.
#' @param env \[`environment` | `NULL`] Expected environment of the function. Checks
#'   if `fn_env(x)` is identical to `env` or inherits from it via [env_inherits()].
#'   Set to `NULL` to not test.
#' @param dots \[`logical(1)` | `NULL`] Test if the function has `...` in its arguments.
#'   Set to `NULL` to not test.
#' @param generic \[`logical(1)` | `NULL`] Test if the function is an S3 generic via
#'   [sloop::is_s3_generic()]. Set to `NULL` to not test.
#' @param method \[`logical(1)` | `NULL`] Test if the function is an S3 method via
#'   [sloop::is_s3_method()]. Set to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#'
#' @returns `r ROXY$test_returns("function")`
#'
#' @name tests-function
NULL


core_function <- function(
  x,
  type = NULL, arg_names = NULL, env = NULL, dots = NULL,
  generic = NULL, method = NULL, sentinels = NULL, custom = NULL,
  caller_env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  tests <- initialize_tests(
    "type_is", sentinels, type, arg_names, env,
    dots, generic, method, custom
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type_is <- is_function(x) %@@% c(type = typeof(x))
  if (!tests$type_is) {
    return(tests)
  }

  args <- fn_fmls_names(x)

  tests$type <- test_fn_type(x, type)
  tests$arg_names <- if (!is_null(arg_names)) identical(args, arg_names)
  tests$env <- test_fn_env(x, env)
  tests$dots <- if (!is_null(dots)) ("..." %in% args) == dots
  tests$generic <- if (!is_null(generic)) sloop::is_s3_generic(x) == generic
  tests$method <- if (!is_null(method)) sloop::is_s3_method(x) == method
  tests$custom <- test_custom(x, custom, caller_env)

  tests
}
# TODO: make slopp suggest with check_installed2



# Helpers ----------------------------------------------------------------------

test_fn_type <- function(x, type) {
  if (is_null(type)) {
    return(NULL)
  }

  type_x <- typeof(x)

  switch(
    type,
    primitive = type_x %in% c("builtin", "special"),
    builtin = type_x == "builtin",
    special = type_x == "special",
    closure = type_x == "closure"
  )
}

test_fn_env <- function(x, target_env) {
  if (is_null(target_env)) {
    return(NULL)
  }

  if (is_primitive(x)) {
    identical(target_env, base_env())
  }

  fn_e <- fn_env(x)
  identical(fn_e, target_env) || env_inherits(fn_e, target_env)
}
