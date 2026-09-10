
# Infinite-related -------------------------------------------------------------

#' Types - Inf and NaN
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
#' @param x \[`any`] An object to test.
#' @param na \[`character(1)`] How to treat `NA` values: `"f"`
#'   to return `FALSE`, `"t"` return `TRUE`, or `"na"` to return `NA`.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#' @param signs \[`character(1)`] For `are_inf()` and `is_inf()` -- which signs
#'   of infinity to allow: `"+"` for positive infinity, `"-"` for negative
#'   infinity, or `"+-"` for both.
#'
#' @returns \[`logical(length(x))`, `logical(1)`] The vectorized or scalar
#'   result of the test, respectively.
#'
#' @details
#' Currently, `NaN` values can never arise from operations with `NA` (`NA + NaN
#' #> NA`), so treating `NaN` as `NA` via `nan = "na"` is not recommended.
#'
#' @examples
#' x <- c(1, Inf, -Inf, NaN, NA)
#'
#' # The default tests:
#' are_finite(x)
#' #> [1] TRUE  FALSE  FALSE  FALSE  NA
#'
#' are_inf(x)
#' #> [1] FALSE  TRUE  TRUE  FALSE  NA
#'
#' are_nan(x, na = "na")
#' #> [1] FALSE  FALSE  FALSE  TRUE  NA
#'
#' # For all, the NA value's result can be controlled:
#' are_finite(x, na = "f")
#' #> [1]  TRUE FALSE FALSE FALSE FALSE
#'
#' are_nan(x, na = "t")
#' #> [1] FALSE FALSE FALSE  TRUE  TRUE
#'
#'
#' # We can consider only +Inf or -Inf:
#' are_inf(x, signs = "+")
#' #> [1] FALSE  TRUE FALSE FALSE  NA
#'
#'
#' # The is_* predicates return TRUE only if all elements pass the test:
#' is_finite(1:10) #> TRUE
#' is_inf(c(-Inf, Inf, Inf, -Inf)) #> TRUE
#'
#' # To test for a single value, use the n argument:
#' is_nan(NaN, n = 1) #> FALSE
#' is_nan(c(NaN, NaN), n = 1) #> FALSE
#'
#' @name predicates-infinite


#' @rdname predicates-infinite
#' @export
are_finite <- function(x, na = "na") {
  # Checks:
  # - x must be numeric
  # - na must be one of "f", "t", "na", or "abort"
  # TODO:


  # Main:
  switch(na,
    f = is.finite(x),
    t = is.finite(x) | are_na2(x),
    na = vec_if_else(are_na2(x), NA, is.finite(x))
  )
}
# NOTE: could be defined as 'not all other options'


#' @rdname predicates-infinite
#' @export
is_finite <- function(x, n = NULL, na = "na") {
  # Checks: left to rlang and are_finite
  is_numeric(x, n = n) && all(are_finite(x, na))
}


#' @rdname predicates-infinite
#' @export
are_nan <- function(x, na = "na") {
  # Checks:
  # - x must be numeric
  # - na must be one of "f", "t", "na"
  # TODO:

  # Main:
  switch(na,
    f = is.nan(x),
    t = is.nan(x) | are_na2(x),
    na = vec_if_else(are_na2(x), NA, is.nan(x))
  )
}


#' @rdname predicates-infinite
#' @export
is_nan <- function(x, n = NULL, na = "na") {
  # Checks: left to rlang and are_nan
  is_numeric(x, n = n) && all(are_nan(x, na))
}


#' @rdname predicates-infinite
#' @export
are_inf <- function(x, na = "na", signs = "+-") {
  # Checks:
  # - x must be numeric
  # - na must be one of "f", "t", "na"
  # - signs must be one of "+-", "+", or "-"
  # TODO:


  # Main:
  signs_allowed <- switch(signs, both = c(-1, 1), "+" = 1, "-" = -1)
  switch(na,
    f = is.infinite(x) & sign(x) %in% signs_allowed,
    t = is.infinite(x) | are_na2(x) & sign(x) %in% signs_allowed,
    na = vec_if_else(are_na2(x), NA, is.infinite(x) & sign(x) %in% signs_allowed)
  )
}


#' @rdname predicates-infinite
#' @export
is_inf <- function(x, n = NULL, na = "na", signs = "+-") {
  is_numeric(x, n = n) && all(are_inf(x, na, signs = "+-"))
}



# Integer-like -----------------------------------------------------------------

#' Types - Integer-like values
#'
#' @description
#' Check if an object can be considerd integer in 4 different interpretations
#' (`mode`s):
#' - `"trunc"`: checks if `x` can be represented as a double-precision integer,
#'   i.e. has negligible decimal part. Allows `Inf` and `NaN` values. If so, it
#'   can be truncated without losing information.
#' - `"range"`: checks if `x` can be represented as an integer, i.e. has
#'   negligible decimal part and is within the integer range. Disallows `Inf`
#'   and `NaN`. If so, it can be coerced [as.integer()] without losing
#'   information.
#' - Both have a `"*_tol"` variant that allows for a tolerance in the decimal
#'   part check, via `abs(x - round(x)) < tol`. In these modes, lossless
#'   coercion is only guaranteed for `round(x)`, not `x`. See the 'Details'
#'   section for more information.
#'
#' `are_integer_like()` is vectorized, returning a vector of same length as `x`,
#' and errors for non-numeric objects. `is_integer_like()` returns `FALSE` for
#' non-numeric objects, or if not all elements pass `are_integer_like()`, and
#' `TRUE` otherwise.
#'
#' @param x \[`any`] Any R object.
#' @param mode \[`character(1)`] The mode of integer interpretation, one of
#'   `"trunc"`, `"trunc_tol"`, `"range"`, or `"range_tol"`.
#' @param tol \[`double(1)`] The tolerance for the `"trunc_tol"` and
#'   `"range_tol"` modes, ignored in other modes. Defaults to the square root of
#'   the machine epsilon.
#' @param na \[`character(1)`] How to treat `NA` values: `"t"` to return `TRUE`,
#'   `"na"` to return `NA`. Integer vectors always return `TRUE` for `NA`
#'   values.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#'
#' @returns \[`logical(length(x))`, `logical(1)`] The vectorized or scalar
#'   result of the test, respectively.
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
#' Philosofically, for `NA`` values, consider: `na = "t"` as "yes, `NA_real_`
#' can safely be coerced to `NA_integer_`"; and `na = "na"` as "this NA value
#' might have a decimal part, so I don't know if I can consider it an integer".
#' Note that with integer vectors, NA values are surely integers, so they always
#' return `TRUE`.
#'
#' @examples
#' x <- c(1.0, NA, 1.0 + 1e-6, 1.0 + .Machine$double.eps, NaN, -Inf, 1e200)
#'
#' # Default test:
#' are_integer_like(x)
#' # [1]  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
#'
#' # is_integer_like only returns TRUE if are_integer_like() is all TRUE:
#' is_integer_like(x) #> FALSE
#'
#' # Objects that dont pass is_integer_like() will generate NAs or loss of
#' # precision when coerced:
#' suppressWarnings(as.integer(x)) #> [1]  1 NA NA NA NA 1 1
#'
#' # Changing NA interpretation:
#' are_integer_like(x, na = "na")
#' # [1]  TRUE    NA FALSE FALSE FALSE FALSE FALSE
#'
#' # Adding tolerance:
#' are_integer_like(x, mode = "range_tol")
#' # [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE
#'
#' # Decreasing tolerance:
#' are_integer_like(x, mode = "range_tol", tol = 1e-5)
#' # [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
#'
#' # trunc mode allows Inf, NaN, and out-of-integer-range values:
#' are_integer_like(x, mode = "trunc")
#' # [1]  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
#'
#' # Adding tolerance, all pass, and finally is_integer_like() retursn TRUE:
#' are_integer_like(x, mode = "trunc_tol", tol = 1e-5)
#' # [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#' is_integer_like(x, mode = "trunc_tol", tol = 1e-5) #> TRUE
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
#' are_integer_like(c(1L, NA_integer_), na = "na")
# [1] TRUE TRUE
#'
#' @name predicates-integer-like
NULL


#' @rdname predicates-integer-like
#' @export
are_integer_like <- function(
  x, mode = "range", tol = sqrt(.Machine$double.eps), na = "t"
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
  na_value <- switch(na, t = TRUE, na = NA)

  if (mode == "trunc") {
    dplyr::case_when(
      are_na2(x) ~ na_value,
      is_nan(x, na = "f") ~ TRUE,
      is_inf(x, na = "f") ~ TRUE,
      TRUE ~ x == round(x)
    )
  } else if (mode == "trunc_tol") {
    dplyr::case_when(
      are_na2(x) ~ na_value,
      is_nan(x, na = "f") ~ TRUE,
      is_inf(x, na = "f") ~ TRUE,
      TRUE ~ abs(x - round(x)) < tol
    )
  } else if (mode == "range") {
    dplyr::case_when(
      are_na2(x) ~ na_value,
      is_nan(x, na = "f") ~ FALSE,
      is_inf(x, na = "f") ~ FALSE,
      TRUE ~ abs(x) <= .Machine$integer.max & x == round(x)
    )
  } else if (mode == "range_tol") {
    dplyr::case_when(
      are_na2(x) ~ na_value,
      is_nan(x, na = "f") ~ FALSE,
      is_inf(x, na = "f") ~ FALSE,
      TRUE ~ abs(x) <= .Machine$integer.max & abs(x - round(x)) < tol
    )
  }
}
# TODO: rethink modes names


#' @rdname predicates-integer-like
#' @export
is_integer_like <- function(
  x, n = NULL, mode = "range", tol = sqrt(.Machine$double.eps), na = "t"
) {
  # Checks: left to rlang and are_integer_like
  is_integer(x, n) || (is_double(x, n) && all(are_integer_like(x, mode, tol)))
}
# Note: faster and more readable than a TryCatch
