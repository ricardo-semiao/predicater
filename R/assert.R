
# TODO: change to assert_*
# TODO: make all accept multiple objects via ..., and args via args

#' Abort using a message-generating function
#'
#' @description
#' Packages like `checkmate` provide functions to check objects and return a
#' message if the check fails. This function allows the user to catch that
#' message and generate a condition with higher flexibility than the original
#' package.
#'
#' `test_msgs()` allows the user to apply the same test to multiple objects.
#'
#' @param fun \[`\(){}`] A function that checks an object and returns `TRUE` if
#'   the check passes, or a message if it fails.
#' @param x \[`any`] The object to check.
#' @param ... For `test_msgs()`, objects to check, for `test_msg()`, additional
#'   arguments to pass to `fun`.
#' @param args \[`list()`] For `test_msgs()`, additional arguments to pass to
#'   `fun`.
#' @param env \[`environment()`] The environment to use for the condition. Often
#'   useful to remove this helper form the trace stack.
#' @param x_name,x_names \[`character(1)`, `character()` | `NULL`] The name of
#'   `x` (or a vector of the names of `...` for `test_msgs()`) to print in
#'   messages. In `NULL`, the name is inferred from `x`'s symbol, if possible.
#' @param cnd_fun \[`function()`] The function to use to generate the condition.
#'   Defaults to `cli_abort()`.
#' @param cnd_args \[`list()`] Additional arguments to pass to `cnd_fun`.
#'
#' @returns \[`TRUE`] Invisibly `TRUE`, or aborts if the check fails.
#'
#' @examples
#' # Using checkmate::check_* functions:
#' try({
#'   f <- \(x) test_msg(checkmate::check_numeric, x, any.missing = FALSE)
#'   f(c(1, NA))
#' })
#' #> Error in `f()`:
#' #> ! Argument `x` contains missing values (element 2)
#'
#' # Multiple arguments with the same test:
#' try({
#'   g <- \(x, y) test_msgs(checkmate::check_logical, x, y, args = list(len = 1))
#'   g(TRUE, c(FALSE, TRUE))
#' })
#' #> Error in `g()`:
#' #> ! Argument `y` must have length 1, but has length 2
#'
#' @export
test_msg <- function(
  fun, x, ...,
  env = caller_env(), x_name = NULL, cnd_fun = cli_abort, cnd_args = list()
) {
  # Setup:
  x_sym <- enexpr(x)
  args <- list2(...)
  force(env)


  # Checks:
  # - x must be a symbol or x_name must be supplied
  # - fun must be a function
  # - env must be an environment
  # - x_name must be a string or NULL
  # - cnd_fun must be a function
  # - cnd_args must be a list
  if (is_null(x_name) && ! is_symbol(x_sym)) {
    cli_abort("{.arg x} must be a symbol or {.arg x_name} must be supplied.")
  }
  if (! is_function(fun)) cli_abort("{.arg fun} must be a function.")
  if (! is_environment(env)) cli_abort("{.arg env} must be an environment.")
  if (! is_null(x_name) && ! is_string(x_name)) {
    cli_abort("{.arg x_name} must be a string or NULL.")
  }
  if (! is_function(cnd_fun)) cli_abort("{.arg cnd_fun} must be a function.")
  if (! is_list(cnd_args)) cli_abort("{.arg cnd_args} must be a list.")


  # Main:
  x_name <- x_name %||% as_name(x_sym)

  msg <- do.call(fun, c(x = list(x), args))
  if (! isTRUE(msg)) {
    msg <- gsub(
      "([^{])\\{([^{])", "\\1{{\\2",
      gsub("([^}])\\}([^}])", "\\1}}\\2", msg)
    ) # Escape braces for glue
    substr(msg, 1, 1) <- tolower(substr(msg, 1, 1))
    cnd_args <- c(
      glue("Argument `{x_name}` {msg}"),
      class = "rs_msg_error",
      msg_args = list(x = x, fun = fun, args = args),
      call = env
    )
    do.call(cnd_fun, cnd_args)
  }

  invisible(TRUE)
}


#' @rdname test_msg
#' @export
test_msgs <- function(
  fun, ..., args = list(),
  env = caller_env(), x_names = NULL,
  cnd_fun = abort, cnd_args = list()
) {
  # Setup:
  x_syms <- enexprs(...)
  force(env)


  # Checks:
  # - ... must be symbols or x_names must be supplied
  # - fun must be a function
  # - args and cnd_args must be lists
  # - env must be an environment
  # - x_names must be a character vector or NULL
  # - cnd_fun must be a function
  if (is_null(x_names) && ! all(vapply(x_syms, is_symbol, logical(1)))) {
    cli_abort("{.arg ...} must be symbols or {.arg x_names} must be supplied.")
  }
  test_msg(checkmate::check_function, fun)
  test_msg(checkmate::check_list, args)
  test_msg(checkmate::check_environment, env)
  test_msg(checkmate::check_character, x_names, null.ok = TRUE)
  test_msg(checkmate::check_function, cnd_fun)
  test_msg(checkmate::check_list, cnd_args)


  # Main:
  x_names <- x_names %||% vapply(x_syms, as_name, character(1))

  Map(list2(...), x_names, f = \(x, name) {
    test_msg(
      fun, x, !!!args,
      env = env, x_name = name,
      cnd_fun = cnd_fun, cnd_args = cnd_args
    )
  })

  NULL
}


#' Test if object is of a given prototype
#'
#' This function wraps [is_ptype()] and aborts with a custom message if the
#' check fails. It is useful for testing function arguments.
#'
#' @param x \[`any`] The object to check.
#' @param ptype \[`any`] The prototype to check against. See [is_ptype()] for
#'   details on what is a prototype.
#' @param ... Additional arguments to pass to [is_ptype()].
#' @param msg \[`character(1)` | `NULL`] The message to use if the check fails.
#'   If `NULL`, a default message is generated.
#' @param env \[`environment()`] The environment to use for the condition. Often
#'   useful to remove this helper form the trace stack.
#' @param x_name \[`character(1)` | `NULL`] The name of `x` to print in
#'   messages. In `NULL`, the name is inferred from `x`'s symbol, if possible.
#' @param cnd_fun \[`function()`] The function to use to generate the condition.
#'   Defaults to `cli_abort()`.
#' @param cnd_args \[`list()`] Additional arguments to pass to `cnd_fun`.
#'
#' @returns \[`TRUE`] Invisibly `TRUE`, or aborts if the check fails.
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
test_ptype <- function(
  x, ptype, ...,
  msg = NULL, env = caller_env(), x_name = NULL,
  cnd_fun = cli_abort, cnd_args = list()
) {
  # Setup:
  x_sym <- enexpr(x)
  ptype_quo <- enquo(ptype)
  args <- list2(...)
  force(env)

  # Checks:
  # - x must be a symbol or x_name must be supplied
  # - msg and x_name must be strings or NULL
  # - env must be an environment
  # - cnd_fun must be a function
  # - cnd_args must be a list
  if (is_null(x_name) && ! is_symbol(x_sym)) {
    cli_abort("{.arg x} must be a symbol or {.arg x_name} must be supplied.")
  }
  test_msgs(checkmate::check_string, msg, x_name, args = list(null.ok = TRUE))
  test_msg(checkmate::check_environment, env)
  test_msg(checkmate::check_function, cnd_fun)
  test_msg(checkmate::check_list, cnd_args)

  # Main:
  x_name <- x_name %||% as_name(x_sym)

  if (! do.call(is_ptype, c(list(.x = x, .ptype = ptype), args))) {
    cnd_args <- c(
      msg %||% "Argument {.arg {x_name}} is not of prototype \\
      {.code {deparse(quo_get_expr(ptype_quo))}}.",
      class = "rs_ptype_error",
      ptype_args = list(x = x, ptype = ptype, ptype_quo = ptype_quo, args = args),
      call = env
    )
    do.call(cnd_fun, cnd_args)
  }

  invisible(TRUE)
}


#' Test if object passes custom expression
#'
#' This function evaluates a custom expression on an object and aborts with a
#' custom message if the check fails. It is useful for testing function
#' arguments.
#'
#' @param x \[`any`] The object to check.
#' @param expr \[`any`] R code (unquoted) to evaluate on `x` as `.x`. The
#'   expression should return `TRUE` if the check passes, or `FALSE` if it fails.
#' @param msg \[`character(1)` | `NULL`] The message to use if the check fails.
#'   If `NULL`, a default message is generated.
#' @param env \[`environment()`] The environment to use for the condition. Often
#'   useful to remove this helper form the trace stack.
#' @param x_name \[`character(1)` | `NULL`] The name of `x` to print in
#'   messages. In `NULL`, the name is inferred from `x`'s symbol, if possible.
#' @param cnd_fun \[`function()`] The function to use to generate the condition.
#'   Defaults to `cli_abort()`.
#' @param cnd_args \[`list()`] Additional arguments to pass to `cnd_fun`.
#'
#' @returns \[`TRUE`] Invisibly `TRUE`, or aborts if the check fails.
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
test_when <- function(
  x, expr, msg = NULL, env = caller_env(), x_name = NULL,
  cnd_fun = cli_abort, cnd_args = list()
) {
  # Setup:
  x_sym <- enexpr(x)
  expr_quo <- enquo(expr)
  force(env)

  # Checks:
  # - x must be a symbol or x_name must be supplied
  # - msg and x_name must be strings or NULL
  # - env must be an environment
  # - cnd_fun must be a function
  # - cnd_args must be a list
  if (is_null(x_name) && ! is_symbol(x_sym)) {
    cli_abort("{.arg x} must be a symbol or {.arg x_name} must be supplied.")
  }
  test_msg(checkmate::check_string, msg, x_name, null.ok = TRUE)
  test_msg(checkmate::check_environment, env)
  test_msg(checkmate::check_function, cnd_fun)
  test_msg(checkmate::check_list, cnd_args)

  # Main:
  expr_text <- deparse(quo_get_expr(expr_quo))
  x_name <- x_name %||% as_name(x_sym)

  if (! eval_tidy(expr_quo, list(.x = x))) {
    cnd_fun(
      msg %||% "Argument {.arg {x_name}} fails {.code {expr_text}}.",
      class = "rs_when_error",
      when_args = list(x = x, expr = expr_text, expr_quo = expr_quo),
      call = env
    )
  }

  invisible(TRUE)
}
# TODO: add try to catch user bad expr
# TODO: reconsider invisible(TRUE), if all these functions should live in the
# same rdname, if the difference in ... in test_msg and test_msgs is too
# confusing or at least create roxy helpers
# TODO: maybe use function \(x) {} instead of a captured expression. Its cleaner
# and works with combine_fns 
