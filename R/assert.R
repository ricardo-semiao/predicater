
# Assert from others -----------------------------------------------------------

#' Assert - Rethrow errors with more information
#'
#' @description
#' Packages like this one, `checkmate`, and `chk` provide functions to check
#' objects and return a message or abort if the check fails. This function
#' allows the user to catch that message/error and generate a condition with
#' higher flexibility than the original package.
#'
#' @param fun \[`\(){}`] For `assert_from_msg`: a function that checks an object
#'   and returns `TRUE` if the check passes, or a message (`character(1)`) if it
#'   fails; For `assert_from_error`: a function that checks an object and raises
#'   an error if the check fails.
#' @param ... \[`any` each] Objects to check.
#' @param args_fun \[`list()`] Additional arguments to pass to `fun`.
#' @param x_names \[`character()` | `NULL`] The names of `...` to print in
#'   messages. In `NULL`, the name is inferred from `x`'s expression.
#' @param env \[`environment()`] The environment to use for the condition. Often
#'   useful to remove this helper from the trace stack.
#' @param args_abort \[`list()`] Additional arguments to pass to
#'   [cli::cli_abort()], which rethrows the error.
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
assert_from_msg <- function(
  fun, ..., args_fun = list(),
  x_names = NULL, env = caller_env(), args_abort = list()
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
  # TODO:


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
        glue("Error with argument {{.arg {x_names[[i]]}}}: {msg}"),
        class = "rs_assert_from_error",
        rs_assert_from_error = list(x = xs[[i]], fun = fun, args = args_fun),
        call = env,
        args_abort
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
  x_names = NULL, nv = caller_env(), args_abort = list()
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
  # TODO:


  # Main:
  xs <- list2(...)
  x_names <- x_names %||% vapply(x_exprs, expr_name, character(1))

  for (i in seq_along(xs)) {
    res <- tryCatch(
      do.call(fun, c(x = list(xs[[i]]), args_fun)),
      error = \(cnd) {
        msg <- gsub(
          "([^{])\\{([^{])", "\\1{{\\2",
          gsub("([^}])\\}([^}])", "\\1}}\\2", res$message)
        ) # Escape braces for glue
        substr(msg, 1, 1) <- tolower(substr(msg, 1, 1))

        cnd_args <- c(
          glue("Error with argument {{.arg {x_names[[i]]}}}: {msg}"),
          class = "rs_assert_from_error",
          rs_assert_from_error = list(x = xs[[i]], fun = fun, args = args_fun),
          call = env,
          args_abort
        )
        do.call(cli_abort, cnd_args)
      }
    )
  }

  invisible(xs)
}
# TODO: allow user to customize msg (probably via function)



# Assert custom ----------------------------------------------------------------

#' Assert - Error if object is not of prototype
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
#' @param x_name \[`character(1)` | `NULL`] The name of `x` to print in
#'   messages. In `NULL`, the name is inferred from `x`'s symbol, if possible.
#' @param env \[`environment()`] The environment to use for the condition. Often
#'   useful to remove this helper form the trace stack.
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
  ptype, ..., args_ptype = list(),
  msg = NULL, x_names = NULL,
  env = caller_env(), args_abort = list()
) {
  # Setup:
  x_exprs <- enexprs(...)
  ptype_quo <- enquo(ptype)
  force(env)


  # Checks:
  # - x must be a symbol or x_name must be supplied
  # - msg and x_name must be strings or NULL
  # - env must be an environment
  # - cnd_fun must be a function
  # - cnd_args must be a list
  # TODO:


  # Main:
  x_names <- x_names %||% vapply(x_exprs, expr_name, character(1))
  xs <- list2(...)

  for (i in seq_along(xs)) {
    if (! do.call(is_ptype, c(list(.x = xs[[i]], .ptype = ptype), args_ptype))) {
      cnd_args <- c(
        msg %||% "Argument {.arg {x_name[i]}} is not of prototype \\
        {.code {deparse(quo_get_expr(ptype_quo))}}.",
        class = "rs_assert_ptype_error",
        rs_assert_ptype_error = list(
          x = xs[[i]], ptype = ptype, ptype_quo = ptype_quo, ptype_args = args_ptype
        ),
        call = env,
        args_abort
      )
      do.call(cli_abort, cnd_args)
    }
  }

  invisible(xs)
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
assert_predicate <- function(
  fun, ..., args_fun,
  msg = NULL, x_names = NULL,
  env = caller_env(), abort_args = list()
) {
  # Setup:
  x_exprs <- enexprs(...)
  force(env)


  # Checks:
  # - x must be a symbol or x_name must be supplied
  # - msg and x_name must be strings or NULL
  # - env must be an environment
  # - cnd_fun must be a function
  # - cnd_args must be a list
  # TODO:


  # Main:
  x_names <- x_names %||% vapply(x_exprs, expr_name, character(1))

  for (i in seq_along(xs)) {
    pred <- tryCatch(
      {
        res <- do.call(fun, c(list(.x = xs[[i]]), args_fun))
        if (! is_bool(res)) {
          cli_abort(
            c(
              "{.code custom(x)} must return {.val {TRUE}} or {.val {FALSE}}.",
              "i" = "Instead, it returned {.val {res}}."
            ),
            class = "rs_user_fun_error",
            call = env,
            rs_user_fun_error = list(bad_result = res)
          )
        }
        res
      },
      rs_user_fun_error = cnd_signal,
      error = \(cnd) {
        cli_abort(
          "{.arg fun} run with error at argument {.arg {x_name[i]}}.",
          .parent = cnd, call = env
        )
      }
    )
    if (! pred) {
      cnd_args <- c(
        msg %||% "Argument {.arg {x_name[i]}} fails {.arg fun}.",
        class = "rs_assert_predicate_error",
        rs_assert_predicate_error = list(
          x = xs[[i]], fun = fun, fun_args = args_fun
        ),
        call = env,
        abort_args
      )
      do.call(cli_abort, cnd_args)
    }
  }


  invisible(xs)
}
# TODO: add try to catch user bad expr
