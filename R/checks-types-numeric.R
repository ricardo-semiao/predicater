
# Infinite-related -------------------------------------------------------------

#' Type checks - Inf and NaN
#'
#' @description
#' Check if an object is `Inf`, `-Inf`, `NaN`, or none of them, flexibly dealing
#' with `NA` values.
#'
#' The `are_*()` functinos are vectorized, returning a vector of same length as
#' `x`, and errors for non-numeric objects. The `is_*()` functions return
#' `FALSE` for non-numeric objects, or if not all elements pass the
#' corresponding `are_*()` function, and `TRUE` otherwise.
#'
#' @param x \[`numeric()`, `any`] For `are_*()`, a numeric vector to tes; for
#'   `is_*()`, any object to test.
#' @param na \[`TRUE` | `FALSE` | `NA`] What to return for `NA` values.
#' @param signs \[`character(1)`] For `are_inf()` and `is_inf()` -- which signs
#'   of infinity to allow: `"+"` for positive infinity, `"-"` for negative
#'   infinity, or `"+-"` for both.
#'
#' @returns
#' - \[`logical(length(x))`] For `are_*`: the vectorized or result of the test.
#' - \[`TRUE` | `FALSE` | `NA`] For `is_*`: the scalar result of the test. If
#'   `na != NA`, then will always return `TRUE` or `FALSE`.
#'
#' @details
#' Currently, `NaN` values can never arise from operations with `NA` (`NA + NaN
#' #> NA`), so treating `NaN` as `NA` via `nan = NA` is not recommended.
#'
#' @examples
#' x <- c(1, Inf, -Inf, NaN, NA)
#'
#' # The default tests:
#' are_finite(x)
#' #> c(TRUE, FALSE, FALSE, FALSE, NA)
#'
#' are_inf(x)
#' #> c(FALSE, TRUE, TRUE, FALSE, NA)
#'
#' are_nan(x)
#' #> c(FALSE, FALSE, FALSE, TRUE, NA)
#'
#'
#' # For all, the NA value's result can be controlled:
#' are_finite(x, na = FALSE)
#' #> c(TRUE, FALSE, FALSE, FALSE, FALSE)
#'
#' are_nan(x, na = TRUE)
#' #> c(FALSE, FALSE, FALSE, TRUE, TRUE)
#'
#'
#' # We can consider only +Inf or -Inf:
#' are_inf(x, signs = "+")
#' #> c(FALSE, TRUE, FALSE, FALSE, NA)
#'
#'
#' # Errors for non-numeric objects:
#' try(are_finite(list(1, 2))) #> Error
#'
#'
#' # The is_* predicates test scalars:
#' is_finite(1) #> TRUE
#' is_finite(1:10) #> FALSE
#'
#' # To get a single TRUE/FALSE result, use all(are_*(...)):
#' all(are_finite(1:10)) #> TRUE
#'
#' @name predicates-infinite


#' @rdname predicates-infinite
#' @export
are_finite <- function(x, na = NA) {
  # Checks:
  # - x must be numeric
  # - na must be one of "f", "t", "na", or "abort"
  # TODO:


  # Main:
  if (is.na(na)) {
    if_else2(are_na2(x), NA, is.finite(x))
  } else if (na) {
    is.finite(x)
  } else {
    is.finite(x) & !are_na2(x)
  }
}


#' @rdname predicates-infinite
#' @export
is_finite <- function(x, na = NA) {
  if (is_numeric(x, n = 1)) {
    if (is.na(x)) na else is.finite(x)
  } else {
    FALSE
  }
}


#' @rdname predicates-infinite
#' @export
are_nan <- function(x, na = NA) {
  # Checks:
  # - x must be numeric
  # - na must be one of "f", "t", "na"
  # TODO:

  # Main:
  if (is.na(na)) {
    if_else2(are_na2(x), NA, is.nan(x))
  } else if (na) {
    is.nan(x)
  } else {
    is.nan(x) & !are_na2(x)
  }
}


#' @rdname predicates-infinite
#' @export
is_nan <- function(x, na = NA) {
  if (is_numeric(x, n = 1)) {
    if (is.na(x)) na else is.nan(x)
  } else {
    FALSE
  }
}


#' @rdname predicates-infinite
#' @export
are_inf <- function(x, na = NA, signs = "+-") {
  # Checks:
  # - x must be numeric
  # - na must be one of "f", "t", "na"
  # - signs must be one of "+-", "+", or "-"
  # TODO:


  # Main:
  signs_allowed <- switch(signs, both = c(-1, 1), "+" = 1, "-" = -1)
  if (is.na(na)) {
    if_else2(are_na2(x), NA, is.infinite(x) & sign(x) %in% signs_allowed)
  } else if (na) {
    is.infinite(x) & sign(x) %in% signs_allowed | are_na2(x)
  } else {
    is.infinite(x) & sign(x) %in% signs_allowed
  }
}


#' @rdname predicates-infinite
#' @export
is_inf <- function(x, na = NA, signs = "+-") {
  signs_allowed <- switch(signs, both = c(-1, 1), "+" = 1, "-" = -1)
  if (is_numeric(x, n = 1)) {
    if (is.na(x)) na else is.infinite(x) & sign(x) %in% signs_allowed
  } else {
    FALSE
  }
}



# Integer-like -----------------------------------------------------------------

#' Type checks - Integer-like values
#'
#' @description
#' Check if an object can be considerd integer in two different interpretations
#' (`mode`s):
#' - `"unbounded"`: checks if `x` can be represented as a double-precision
#'   integer, i.e. `x - round(x)` falls within some tolerance value (`tol`).
#'   Allows `Inf` and `NaN` values. If so, it can be [round()]-ded without loss
#'   of information.
#' - `"range"`: checks if `x` can be represented as an integer, i.e. passes
#'   'unbounded' and is within R's allowed integer range. Disallows `Inf` and
#'   `NaN`. If so, it can be coerced `as.integer(round(x))` without losing
#'   information.
#' - Note that `tol` can be set to zero, for a strict check of the decimal part.
#'
#' `are_integer_like()` is vectorized, returning a vector of same length as `x`,
#' and errors for non-numeric objects. `is_integer_like()` returns `FALSE` for
#' non-numeric objects, or if not all elements pass `are_integer_like()`, and
#' `TRUE` otherwise.
#'
#' @param x `r ROXY$x()`
#' @param mode \[`"bounded"` | `"unbounded"`] The mode of integer interpretation
#'   as described above.
#' @param tol \[`double(1)`] The tolerance for the `"trunc_tol"` and
#'   `"range_tol"` modes, ignored in other modes. A useful value is
#'   `sqrt(.Machine$double.eps)`.
#' @param na \[`TRUE` | `NA`] What to return for `NA` values. Integer vectors
#'   always return `TRUE` for `NA` values.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#'
#' @returns
#' - \[`logical(length(x))`] For `are_integer_like()`: the vectorized or result
#'   of the test.
#' - \[`TRUE` | `FALSE` | `NA`] For `is_integer_like()`: the scalar result of
#'   the test. If `na != NA`, then will always return `TRUE` or `FALSE`.
#'
#' @details
#' R stores integers with 32 bits, allowing to represent values between about
#' \eqn{\pm 2 \times 10^9}{+/- 2 * 10^9} (see `.Machine$integer.max` for the
#' exact value). Doubles and doubles use the 'binary64' format allowing to
#' represent values between about \eqn{\pm 1.8 \times 10^{308}}{+/- 1.8 *
#' 10^{308}} (see `.Machine[c("double.xmin", "double.xmax")]` for the exact
#' value). Thus, not all zero-decimal doubles can be coerced into integers,
#' which is the distinction between the `"trunc"` and `"range"` modes.
#'
#' Note that the mathematical `Inf` and `NaN` concepts exist for integers, but
#' they cannot be coerced into [integer()] in R, thats why they are allowed in
#' the `"trunc"` modes and not in `"range"` modes.
#'
#' Philosofically, for `NA`` values, consider: `na = TRUE` as "yes, `NA_real_`
#' can safely be coerced to `NA_integer_`"; and `na = NA` as "this NA value
#' might have a decimal part, so I don't know if I can consider it an integer".
#' Note that with integer vectors, NA values are surely integers, so they always
#' return `TRUE`.
#'
#' @examples
#' x <- c(1.0, NA, 1.0 + 1e-6, 1.0 + .Machine$double.eps, NaN, -Inf, 1e200)
#'
#' # Default test:
#' are_integer_like(x)
#' #> c(TRUE, NA, FALSE, FALSE, FALSE, FALSE, FALSE)
#'
#' # is_integer_like only returns TRUE if are_integer_like() is all TRUE:
#' is_integer_like(x) #> FALSE
#'
#' # Objects that dont pass is_integer_like() will generate NAs or loss of
#' # precision when coerced:
#' suppressWarnings(as.integer(round(x))) #> c(1L, NA, 1L, 1L, NA, NA, NA)
#'
#' # Changing NA interpretation:
#' are_integer_like(x, na = NA)
#' #> c(TRUE, NA, FALSE, FALSE, FALSE, FALSE, FALSE)
#'
#' # Adding tolerance:
#' are_integer_like(x, tol = sqrt(.Machine$double.eps))
#' #> c(TRUE, NA, TRUE, TRUE, FALSE, FALSE, FALSE)
#'
#' # Decreasing tolerance:
#' are_integer_like(x, tol = 1e-5)
#' #> c(TRUE, NA, FALSE, FALSE, FALSE, FALSE, FALSE)
#'
#' # unbounded mode allows Inf, NaN, and out-of-integer-range values:
#' are_integer_like(x, mode = "unbounded")
#' #> c(TRUE, NA, FALSE, FALSE, TRUE, TRUE, TRUE)
#'
#' # Adding tolerance, all pass, and finally is_integer_like() retursn TRUE:
#' are_integer_like(x, mode = "unbounded", tol = 1e-5)
#' #> c(TRUE, NA, TRUE, TRUE, TRUE, TRUE, TRUE)
#'
#' is_integer_like(x, mode = "unbounded", tol = 1e-5) #> TRUE
#'
#'
#' # are_integer_like() fails for non-numeric objects, while is_integer_like()
#' # returns FALSE:
#' try(are_integer_like(list(1L, 2L))) #> Error
#' is_integer_like(list(1L, 2L)) #> FALSE
#'
#' # To test for a single integer-like value, use the n argument:
#' is_integer_like(1L, n = 1) #> TRUE
#' is_integer_like(1:2, n = 1) #> FALSE
#'
#'
#' # Integer vectors always return TRUE for NA values:
#' are_integer_like(c(1L, NA_integer_), na = NA) #> c(TRUE, TRUE)
#'
#' @name is_integer_like
NULL


#' @rdname is_integer_like
#' @export
are_integer_like <- function(
  x, mode = "bounded", tol = 0, na = TRUE
) {
  # Checks:
  # - x must be numeric
  # - mode must be one of "type", "trunc", "trunc_tol", "range", or "range_tol"
  # - tol must be a non-NA, finite, non-negative double(1)
  # - na must be one of "t" or "na"
  # TODO:


  # Main:
  if (is_integer(x)) {
    return(rep(TRUE, length(x)))
  }

  if (mode == "unbounded") {
    case_when2(
      abs(x - round(x)) < tol,
      are_na2(x) ~ na,
      is_nan(x, na = FALSE) ~ TRUE,
      is_inf(x, na = FALSE) ~ TRUE
    )
  } else {
    case_when2(
      abs(x) <= .Machine$integer.max & abs(x - round(x)) < tol,
      are_na2(x) ~ na,
      is_nan(x, na = FALSE) ~ FALSE,
      is_inf(x, na = FALSE) ~ FALSE
    )
  }
}


#' @rdname is_integer_like
#' @export
is_integer_like <- function(x, n = NULL, mode = "bounded", tol = 0, na = TRUE) {
  # Checks: left to rlang and are_integer_like
  is_integer(x, n) || (is_double(x, n) && all(are_integer_like(x, mode, tol)))
}
