
#' @include validation-helpers.R validation-menu.R
NULL

# TODO: instead of x_names, add names to ... . Then, maybe, add . prefix to arg
# names
# TODO: review what to show for arg names when not simple symbols, and also for
# fun, ptype, ...



# Assert from others -----------------------------------------------------------

#' Validation - Rethrow errors with more information
#'
#' @description
#' Packages like this one, `checkmate`, and `chk` provide functions to check
#' objects and return a message or abort if the check fails. This function
#' allows the user to catch that message/error and generate a condition with
#' higher flexibility than the original package.
#'
#' @param fun \[`\(){}`]
#'   - For `assert_from_msg()`: a function that checks an object and returns
#'   `TRUE` if the check passes, or a message (`character(1)`) if it fails.
#'   - For `assert_from_error()`: a function that checks an object and raises an
#'   error if the check fails.
#' @param ... \[`any` each] Objects to check.
#' @param args_fun \[`list()`] Additional arguments to pass to `fun`.
#' @param x_names `r ROXY$x_names()`
#' @param env `r ROXY$env()`
#' @param args_cnd `r ROXY$args_cnd(FALSE)`
#'
#' @returns \[`list(...)`] `invisible(list(...))`, or aborts if the check fails.
#'
#' @examples
#' # Using checkmate::check_* functions:
#' # try({
#' #   f <- \(x) test_msg(checkmate::check_numeric, x, any.missing = FALSE)
#' #   f(c(1, NA))
#' # })
#' #> Error in `f()`:
#' #> ! Argument `x` contains missing values (element 2)
#'
#' # Multiple arguments with the same test:
#' # try({
#' #   g <- \(x, y) test_msgs(checkmate::check_logical, x, y, args = list(len = 1))
#' #   g(TRUE, c(FALSE, TRUE))
#' # })
#' #> Error in `g()`:
#' #> ! Argument `y` must have length 1, but has length 2
#'
#' @export
assert_from_msg <- function(
  fun, ..., args_fun = list(),
  x_names = NULL, env = caller_env(), args_cnd = list()
) {
  # Setup:
  x_exprs <- enexprs(...)
  force(env)


  # Checks:
  # - ... must be symbols or x_names must be supplied
  # - fun must be a function
  # - args and cnd_args must be lists
  # - env must be an environment
  # - x_names must be a character vector or NULL
  # - cnd_fun must be a function
  

  # Main:
  xs <- list2(...)
  x_names <- x_names %||% vapply(x_exprs, expr_name, character(1))

  for (i in seq_along(xs)) {
    msg <- do.call(fun, c(x = list(xs[[i]]), args_fun))

    if (! isTRUE(msg)) {
      msg <- gsub(
        "([^{])\\{([^{])", "\\1{{\\2",
        gsub("([^}])\\}([^}])", "\\1}}\\2", msg)
      ) # Escape braces for glue
      substr(msg, 1, 1) <- tolower(substr(msg, 1, 1))

      cnd_args <- c(
        message = list(c(
          glue("Error with argument {{.arg {x_names[[i]]}}}: {msg}"),
          "i" = "See this condition's {.code rs_assert_from_error} attribute for details."
        )),
        class = "rs_assert_from_error", call = env,
        rs_assert_from_error = list(x = xs[[i]], fun = fun, args = args_fun),
        args_cnd
      )
      do.call(cli_abort, cnd_args)
    }
  }

  invisible(xs)
}


#' @rdname assert_from_msg
#' @export
assert_from_error <- function(
  fun, ..., args_fun = list(),
  x_names = NULL, env = caller_env(), args_cnd = list()
) {
  # Setup:
  x_exprs <- enexprs(...)
  force(env)


  # Checks:
  # - ... must be symbols or x_names must be supplied
  # - fun must be a function
  # - args and cnd_args must be lists
  # - env must be an environment
  # - x_names must be a character vector or NULL
  # - cnd_fun must be a function
  

  # Main:
  xs <- list2(...)
  x_names <- x_names %||% vapply(x_exprs, expr_name, character(1))

  for (i in seq_along(xs)) {
    res <- tryCatch(do.call(fun, c(x = list(xs[[i]]), args_fun)), error = identity)
    if (!inherits(res, "error")) next # CHECK: use withCallingHandlers instead?

    msg <- gsub(
      "([^{])\\{([^{])", "\\1{{\\2",
      gsub("([^}])\\}([^}])", "\\1}}\\2", res$message)
    ) # Escape braces for glue
    substr(msg, 1, 1) <- tolower(substr(msg, 1, 1))
    cnd_args <- c(
      message = list(c(
        glue("Error with argument {{.arg {x_names[[i]]}}}: {msg}"),
        "i" = "See this condition's {.code rs_assert_from_error} attribute for details."
      )),
      class = "rs_assert_from_error", call = env,
      rs_assert_from_error = list(x = xs[[i]], fun = fun, args = args_fun),
      args_cnd
    )
    do.call(cli_abort, cnd_args)
  }

  invisible(xs)
}
# TODO: allow user to customize msg (probably via function)



# Assert from custom, ptype --------------------------------------------------

#' Validation - If object is of prototype
#'
#' This function wraps [is_ptype()] and aborts with a custom message if the
#' check fails. It is useful for testing function arguments.
#'
#' @param ptype \[`any`] The prototype to check against. See [is_ptype()] for
#'   details on what is a prototype.
#' @param ... \[`any` each] Objects to check.
#' @param args_ptype \[`list()`] Additional arguments to pass to [is_ptype()].
#' @param msg \[`character(1)` | `NULL`] The message to use if the check fails.
#'   If `NULL`, a default message is generated.
#' @param x_names `r ROXY$x_names()`
#' @param env `r ROXY$env()`
#' @param args_cnd `r ROXY$args_cnd(FALSE)`
#'
#' @returns \[`list(...)`] `invisible(list(...))`, or aborts if the check fails.
#'
#' @examples
#' try({
#'   f <- \(x) test_ptype(x, numeric())
#'   f("a")
#' })
#' #> Error in `f()`:
#' #> ! Argument `x` is not of prototype `numeric()`.
#'
#' @export
assert_ptype <- function(
  ptype, ..., args_ptype = list(),
  msg = NULL, x_names = NULL,
  env = caller_env(), args_cnd = list()
) {
  # Setup:
  x_exprs <- enexprs(...)
  ptype_quo <- enquo(ptype)
  force(env)


  # Checks:
  # - x must be a symbol or x_names must be supplied
  # - msg and x_names must be strings or NULL
  # - env must be an environment
  # - cnd_fun must be a function
  # - cnd_args must be a list
  

  # Main:
  x_names <- x_names %||% vapply(x_exprs, expr_name, character(1))
  xs <- list2(...)

  for (i in seq_along(xs)) {
    if (! do.call(is_ptype, c(list(.x = xs[[i]], .ptype = ptype), args_ptype))) {
      cnd_args <- c(
        message = list(c(
          msg %||% "Argument {.arg {x_names[i]}} is not of prototype \\
          {.code {deparse(quo_get_expr(ptype_quo))}}.",
          "i" = "See this condition's {.code rs_assert_ptype_error} attribute for details."
        )),
        class = "rs_assert_ptype_error", call = env,
        rs_assert_ptype_error = list(
          x = xs[[i]], ptype = ptype, ptype_quo = ptype_quo, ptype_args = args_ptype
        ),
        args_cnd
      )
      do.call(cli_abort, cnd_args)
    }
  }

  invisible(xs)
}


#' Validation - Throw error from custom predicate
#'
#' This function evaluates a custom predicate function on an object and aborts
#' with a custom message if the check fails. It is useful for testing function
#' arguments.
#'
#' @param fun \[`\(){}`] The predicate function to run. Must recieve the object
#'   to test as its first argument.
#' @param ... \[`any` each] Objects to check.
#' @param args_fun \[`list()`] Additional arguments to pass to `fun`.
#' @param msg \[`character(1)` | `NULL`] The message to use if the check fails.
#'   If `NULL`, a default message is generated.
#' @param x_names `r ROXY$x_names()`
#' @param env `r ROXY$env()`
#' @param args_cnd `r ROXY$args_cnd(FALSE)`
#'
#' @returns \[`list(...)`] `invisible(list(...))`, or aborts if the check fails.
#'
#' @examples
#' try({
#'   f <- \(x) test_when(x, all(.x > 0))
#'   f(c(-1, 0, 1))
#' })
#' #> Error in `f()`:
#' #> ! Argument `x` fails `all(.x > 0)`.
#'
#' @export
assert_predicate <- function(
  fun, ..., args_fun = list(),
  msg = NULL, x_names = NULL,
  env = caller_env(), args_cnd = list()
) {
  # Setup:
  x_exprs <- enexprs(...)
  xs <- list2(...)
  force(env)


  # Checks:
  # - x must be a symbol or x_names must be supplied
  # - msg and x_names must be strings or NULL
  # - env must be an environment
  # - cnd_fun must be a function
  # - cnd_args must be a list
  

  # Main:
  x_names <- x_names %||% vapply(x_exprs, expr_name, character(1))

  for (i in seq_along(xs)) {
    pred <- tryCatch(
      {
        res <- do.call(fun, c(xs[i], args_fun))
        if (! is_bool(res)) {
          cli_abort(
            c(
              "{.code fun(.)} must return {.val {TRUE}} or {.val {FALSE}}.",
              "i" = "Instead, with {.arg {x_names[i]}}, it returned {.val {res}}.",
              "i" = "See this condition's {.code rs_user_fun_error} attribute for details."
            ),
            class = "rs_user_fun_error", call = env,
            rs_user_fun_error = list(bad_result = res)
          )
        }
        res
      },
      rs_user_fun_error = cnd_signal,
      error = \(cnd) {
        cli_abort(
          "{.arg fun} run with error at argument {.arg {x_names[i]}}.",
          class = "rs_user_fun_error", parent = cnd, call = env
        )
      }
    )
    if (! pred) {
      cnd_args <- c(
        message = list(c(
          msg %||% "Argument {.arg {x_names[i]}} fails {.arg fun}.",
          "i" = "See this condition's {.code rs_assert_predicate_error} attribute for details."
        )),
        rs_assert_predicate_error = list(
          x = xs[[i]], fun = fun, fun_args = args_fun
        ),
        class = "rs_assert_predicate_error", call = env,
        args_cnd
      )
      do.call(cli_abort, cnd_args)
    }
  }

  invisible(xs)
}



# Assert from multiple types ---------------------------------------------------

#' Validation - Test for one of multiple types
#'
#' @description
#' This function checks if `x` passes at least one of a set of tests. `x` is
#' allowed to be any type [typeof()] in `types`, and needs to pass the
#' corresponding test for its type.
#'
#' For example, if `x` is of type `"integer"`, then it is tested with
#' [test_integer()]. Pass a list of arguments to the test function via `...`,
#' with the same argument name as the type. E.g. `integer = list(len = 2)`.
#'
#' @param x `r ROXY$x()`
#' @param types \[`character()`] A character vector of types to check against.
#' @param ... \[`list()` each] Lists of arguments to pass to the test function
#'   for each type. The list names
#'
#' @returns `r ROXY$test_returns("multi")`
#'
#' @examples
#' x <- list(a = 1:2, b = 3:4)
#'
#' args <- list(
#'   types = c("integer", "list"),
#'   list = list(
#'     custom_map = \(elt) is_integer(elt, 1)
#'   )
#'   # Either a integer vector of a list of integer scalars
#' )
#'
#' do.call(test_multiple, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' @name test_multiple
NULL

core_multiple <- function(x, types, ..., short_circuit) {
  type <- typeof(x)
  cores_args <- list2(...)

  if (! type %in% update_types(types)) {
    return(c(multi = FALSE) %@@% list(types = types))
  }

  exec(
    TABLE_TEST_TYPE[[type]],
    x, !!!cores_args[[type]], short_circuit = short_circuit
  )
}

#' @rdname test_multiple
#' @export
test_multiple <- fn_core_to_test(core_multiple)

# assert_multiple <- fn_core_to_assert(core_multiple, list(
#   multi = \(attrs, test) {
#     glue2("must be one of the 'types': [fmt_vec(attrs$types)]")
#   }
# ))
# try(do.call(assert_multiple, c(list(x), args))) #> Error
# Currently does not work because it does not have acess to all the msgs_fns. We
# would need to save all into TESTS_MSGS, and not filter it when creating this
# function



# Helpers ----------------------------------------------------------------------

update_types <- function(types) {
  if ("collection" %in% types) {
    types <- c(
      setdiff(types, "collection"),
      c("vector", "pairlist", "expression", "environment")
    )
  }
  if ("vector" %in% types) {
    types <- c(
      setdiff(types, "vector"),
      c("atomic", "list")
    )
  }
  if ("atomic" %in% types) {
    types <- c(
      setdiff(types, "atomic"),
      c("logical", "integer", "double", "complex", "character")
    )
  }
  if ("code" %in% types) {
    types <- c(
      setdiff(types, "code"),
      c("language", "symbol")
    )
  }
  types
}
