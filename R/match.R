
# if_else and case_when --------------------------------------------------------

# From https://github.com/Yunuuuu/standalone/blob/main/R/standalone-tibble.R
# at 11/09/2026

#' @title Match - Vectorized if-else
#' @inherit funs::if_else
if_else2 <- function(condition, true, false, na = NULL) {
  # output size from `condition`
  size <- vctrs::vec_size(condition)

  # output type from `true`/`false`/`na`
  ptype <- vctrs::vec_ptype_common(true = true, false = false, na = na)

  args <- vctrs::vec_recycle_common(
    true = true, false = false, na = na, .size = size
  )
  args <- vctrs::vec_cast_common(!!!args, .to = ptype)

  out <- vctrs::vec_init(ptype, size)

  loc_true <- condition
  loc_false <- !condition

  out <- vctrs::vec_assign(out, loc_true, vctrs::vec_slice(args$true, loc_true))
  out <- vctrs::vec_assign(out, loc_false, vctrs::vec_slice(args$false, loc_false))

  if (!is.null(na)) {
    loc_na <- vctrs::vec_detect_missing(condition)
    out <- vctrs::vec_assign(out, loc_na, vctrs::vec_slice(args$na, loc_na))
  }

  out
}

#' @title Match - Vectorized nested if-else
#' @inherit dplyr::case_when
case_when2 <- function(.default, ..., .ptype = NULL) {
  if (is.null(.ptype)) {
    .ptype <- vctrs::vec_ptype(.default)
  } else {
    .default <- vctrs::vec_cast(.default, .ptype)
  }
  env <- rlang::caller_env()
  dots <- rlang::list2(...)
  unused <- vctrs::vec_rep(TRUE, times = vctrs::vec_size(.default))
  for (i in seq_along(dots)) {
    if (!any(unused)) {
      break
    }
    dot <- .subset2(dots, i)
    loc <- unused & rlang::eval_tidy(rlang::f_lhs(dot), env = env)
    value <- rlang::eval_tidy(rlang::f_rhs(dot), env = env)
    value <- vctrs::vec_cast(value, .ptype, x_arg = sprintf("`...` (%d)", i))
    if (length(value) > 1L) value <- vctrs::vec_slice(value, loc)
    .default <- vctrs::vec_assign(
      .default, loc, value, value_arg = sprintf("`...` (%d)", i)
    )
    unused[loc] <- FALSE
  }
  .default
}



# Match ------------------------------------------------------------------------

# TODO: empty RHS returns the value of last non-empty RHS
# TODO: _ptype and _id versions with a fixed tab
# TODO: try catch user code

#' Match - Pattern matching
#'
#' Pattern matching functions to return a value based on a case of `x`. Similar
#' to [switch()] but allows for any type for object as the case. Cases are two
#' sided formulas with the LHS being the case and the RHS being the return
#' value.
#' - For `match_hash` and `match_id`, cases are any R object to directly compare
#'   with `x`.
#' - For `match_ptype`, cases are prototypes to check if `x` [is_ptype()].
#' - For `match_when`, cases are any predicate expression, that matches when it
#'    evaluates to `TRUE`.
#'
#' The return value is evaluated dynamically and can refer to `x`, except for
#' `match_hash`, which in turn is faster and can be supplied a pre-built hash
#' table.
#'
#' @param x \[`any`] Object to match.
#' @param ... \[`<formula>` each] Cases to match. Each case is a formula with
#'   the left-hand side being the value to match against and the right-hand side
#'   being the value to return if the case matches. Cases are evaluated in
#'   order, and in the caller environment.
#' @param nomatch \[`any`] Value to return if no cases match. Use `stop()` or
#'   similar to err.
#' @param htab,type,size \[`hashtab()`, `character(1)`, `integer(1)`] For
#'   `match_hash`: an existing hash table, or the arguments passed to
#'   [utils::hashtab()] to create a new one.
#' @param fun,args_id \[`character(1)`, `list()`] For `match_id`: which
#'   'identical' function to use, `"identical"` for [identical()] or
#'   `"identical2"` for [identical2()]; and a list of arguments to pass to it.
#' @param args_ptype \[`list()`] For
#'   `match_ptype`. A list of arguments to pass to [is_ptype()].
#'
#' @returns \[`any`] The value of the first matching case, or `nomatch` if no
#'   cases match.
#'
#' @examples
#' htab <- utils::hashtab(size = 2)
#' utils::sethash(htab, 1.42, base::sum)
#' utils::sethash(htab, "oi", -Inf)
#' match_hash(1.42, htab = htab) #> base::sum
#'
#' match_id(1.42, "oi" ~ -Inf, 1.42 ~ .x * 2) #> 2.84
#'
#' match_ptype(1.42, character() ~ "chr", double() ~ "dbl") #> "dbl"
#'
#' match_when("oi", grepl("1", .x) ~ "1", grepl("o", .x) ~ "o") #> "o"
#'
#' @export
match_hash <- function(x, ..., nomatch = NULL, htab = NULL, type, size) {
  # Setup:
  cases <- list2(...)
  env <- caller_env()


  # Checks:
  # - ... must be formulas with non-empty LHS
  # - htab must be NULL or of class hashtab
  # - if ... is empty, htab must be provided
  # - if htab is providade and ... is not empty, warn
  # - type and size are left to hashtab
  # TODO:


  # Main:
  if (is_null(htab)) {
    htab <- utils::hashtab(type, size)
  }

  for (case in cases) {
    htab[[eval(f_lhs(case), env)]] <- eval(f_rhs(case), env)
  }

  utils::gethash(htab, x, nomatch = nomatch)
}
# TODO: use maybe_missing, better, type = missing_arg()


#' @rdname match_hash
#' @export
match_id <- function(
  x, ..., nomatch = NULL,
  fun = "identical", args_id = list()
) {
  # Setup:
  cases <- list2(...)
  env <- caller_env()
  env$.x <- x


  # Checks:
  # - ... must be formulas with non-empty LHS, at least one
  # - fun must be one of "identical" or "identical2"
  # - args_id must be a list
  # TODO:


  # Main:
  fun <- switch(fun,
    identical2 = identical2,
    identical = identical
  )

  args_id$x <- x
  for (case in cases) {
    args_id$y <- eval(f_lhs(case), env)
    if (do.call(fun, args_id)) {
      return(eval(f_rhs(case), env))
    }
  }

  nomatch
}


#' @rdname match_hash
#' @export
match_ptype <- function(x, ..., nomatch = NULL, args_ptype = list()) {
  # Setup:
  cases <- list2(...)
  env <- caller_env()
  env$x <- x
  args_ptype$.x <- x


  # Checks:
  # - ... must be formulas with non-empty LHS, at least one
  # - args_ptype must be a list
  # TODO:


  # Main:
  for (case in cases) {
    args_ptype$.ptype <- eval(f_lhs(case), env)
    if (do.call(is_ptype, args_ptype)) {
      return(eval(f_rhs(case), env))
    }
  }

  nomatch
}


#' @rdname match_hash
#' @export
match_when <- function(x, ..., nomatch = NULL) {
  # Setup:
  cases <- list2(...)
  env <- caller_env()
  env$.x <- x


  # Checks:
  # - ... must be formulas with non-empty LHS, at least one
  # TODO:


  # Main:
  for (case in cases) {
    if (eval(f_lhs(case), env)) {
      return(eval(f_rhs(case), env))
    }
  }

  nomatch
}
# CHECK: https://github.com/tidyverse/purrr/blob/main/R/deprec-when.R
# TODO: function instead of expr?



# Helpers ----------------------------------------------------------------------

match_check_cases <- function(cases, empty_ok = FALSE, env = caller_env()) {
  if (! empty_ok && length(cases) == 0) {
    cli_abort("{.arg ...} must be non-empty.", call = env)
  }

  cond <- all(vapply_lgl(cases, \(x) is_formula(x, scoped = TRUE, lhs = TRUE)))
  # Returns TRUE for empty cases

  if (! cond) {
    cli_abort("{.arg ...} must be formulas with non-empty LHS.", call = env)
  }
}
