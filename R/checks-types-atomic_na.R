
# Base checks ------------------------------------------------------------------

#' Type checks - Atomic vectors
#'
#' @description
#' Check if an object is of a specific atomic type ([typeof()]). The test is
#' invariant to attributes such as class.
#'
#' - [rlang::is_logical()], [rlang::is_integer()], [rlang::is_double()],
#'   [rlang::is_complex()], [rlang::is_character()], and [rlang::is_raw()] check
#'   if an object is of the respective atomic type.
#' - [rlang::is_atomic()] checks if an object is any of the above.
#' - `is_numeric()` checks if an object is either an integer or
#'   double vector.
#'
#' @usage
#' is_logical(x, n = NULL)
#'
#' is_integer(x, n = NULL)
#'
#' is_double(x, n = NULL, finite = NULL)
#'
#' is_complex(x, n = NULL, finite = NULL)
#'
#' is_character(x, n = NULL)
#'
#' is_raw(x, n = NULL)
#'
#' is_atomic(x, n = NULL)
#'
#' is_numeric(x, n = NULL)
#'
#'
#' @param x `r ROXY$x()`
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#' @param finite \[`TRUE` | `FALSE` | `NULL`] Whether all values of the vector are
#'   not `NA`, `NaN`, `Inf`, or `-Inf`. Set to `NULL` to not test. [is_finite()]
#'   is designed to allow more flexibility to this test.
#'
#' @returns `r ROXY$test_res()`
#'
#' @aliases is_logical is_integer is_double is_complex is_character is_raw is_atomic
#' @rawNamespace export(is_logical, is_integer, is_double, is_complex, is_character, is_raw, is_atomic)
#'
#' @name predicates-atomic
NULL


#' @rdname predicates-atomic
#' @usage NULL
#' @export
is_numeric <- function(x, n = NULL) {
  # Checks: left to rlang
  is_integer(x, n) || is_double(x, n)
}
# CHECK: Long vectors, and others? Allow generic implementations?



# TRUE and FALSE ---------------------------------------------------------------

# CHECK: reconsider adding a @name, maybe use is_bool

#' Type checks - TRUE and FALSE values
#'
#' Check if an object is literally `TRUE` or `FALSE`, controlling for `NA`
#' values. [rlang::is_bool()] checks for either `TRUE` or `FALSE`.
#'
#' @param x \[`logical()`, `any`] For `are_*()`, a logical vector; for `is_*()`,
#'   an object to test.
#' @param na \[`FALSE` | `NA`] What to return for `NA` values.
#'
#' @returns
#' - \[`logical(length(x))`] For `are_*`: the vectorized or result of the test.
#' - \[`TRUE` | `FALSE` | `NA`] For `is_*`: the scalar result of the test. If
#'   `na != NA`, the result is always `TRUE` or `FALSE`.
#'
#' @name predicates-true-false
NULL


#' @rdname predicates-true-false
#' @export
are_true <- function(x, na = FALSE) {
  # Checks:
  # - x must be a logical vector
  # - na must be one of "f", "na"


  # Main:
  if (is.na(na)) {
    if_else2(is.na(x), NA, x)
  } else {
    !is.na(x) & x
  }
}


#' @rdname predicates-true-false
#' @export
is_true2 <- function(x, na = FALSE) {
  # Checks:
  # - na must be one of "f", "na"


  # Main:
  if (is_logical(x, 1)) {
    if (is.na(x)) na else x
  } else {
    FALSE
  }
}


#' @rdname predicates-true-false
#' @export
are_false <- function(x, na = FALSE) {
  # Checks:
  # - x must be a logical vector
  # - na must be one of "f", "na"


  # Main:
  if (is.na(na)) {
    if_else2(is.na(x), NA, x)
  } else {
    !(is.na(x) | x)
  }
}


#' @rdname predicates-true-false
#' @export
is_false2 <- function(x, na = FALSE) {
  # Checks:
  # - na must be one of "f", "na"


  # Main:
  if (is_logical(x, 1)) {
    if (is.na(x)) na else x
  } else {
    FALSE
  }
}


#' @rdname predicates-true-false
#' @export
is_bool2 <- function(x, na = FALSE) {
  if (is_logical(x, 1)) {
    if (is.na(x)) na else TRUE
  } else {
    FALSE
  }
}



# NAs --------------------------------------------------------------------------

#' Type checks - NA values
#'
#' @description
#' Checks if an object is `NA`, `NA_integer_`, `NA_real_`, `NA_complex_`, or
#' `NA_character_`. Differently from [is.na()] and
#' [rlang::is_na()]/[rlang::are_na()], it allows to control the behaviour for
#' `NaN` objects.
#'
#' - `are_na2()` is vectorized, returning a vector of same length as `x`, and
#' errors for non-atomic objects.
#' - `is_na2()` returns `FALSE` for non-atomic
#' objects, or if not all elements pass `are_na2()`, and `TRUE` otherwise.
#'
#' @param x `r ROXY$x()`
#' @param nan \[`TRUE` | `FALSE`] What to return for `NaN` values.
#' @param types \[`character()` | `NULL`] A character vector of allowed `NA`
#'   types (see [`NA`]). Set to `NULL` to allow all types.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#'
#' @returns
#' - \[`logical(length(x))`] For `are_*`: the vectorized or result of the test.
#' - \[`TRUE` | `FALSE`] For `is_*`: the scalar result of the test.
#'
#' @examples
#' x <- c(1, Inf, -Inf, NaN, NA)
#'
#' are_na2(x)
#' #> [1] FALSE FALSE FALSE FALSE  TRUE
#'
#' are_na2(x, nan = TRUE)
#' #> [1] FALSE FALSE FALSE  TRUE  TRUE
#'
#' are_na2(x, nan = NA)
#' #> [1] FALSE FALSE FALSE    NA  TRUE
#'
#' is_na2(x) #> FALSE # In all nan modes
#' is_na2(c(NA, NA)) #> TRUE
#'
#' # To test for a single NA value, use the n argument:
#' is_na2(NA, n = 1) #> TRUE
#'
#' @name is_na2
NULL

#' @rdname is_na2
#' @export
are_na2 <- function(x, nan = FALSE) {
  # Checks:
  # - x must be atomic
  # - nan must be one of "f", "t", "na"
  # TODO:


  # Main:
  if (is.na(nan)) {
    if_else2(is.nan(x), NA, is.na(x))
  } else if (nan) {
    is.na(x)
  } else {
    is.na(x) & !is.nan(x)
  }
}


#' @rdname is_na2
#' @export
is_na2 <- function(x, nan = FALSE, types = NULL) {
  if (is_atomic(x, 1)) {
    res <- if (is.nan(x)) nan else is.na(x)
    if (is_null(types)) {
      res
    } else {
      res && (typeof(x) %in% types)
    }
  } else {
    FALSE
  }
}
