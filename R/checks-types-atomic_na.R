
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
#' @param x \[`any`] An object to test.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#' @param finite \[`TRUE` | `FALSE` | `NULL`] Whether all values of the vector are
#'   not `NA`, `NaN`, `Inf`, or `-Inf`. Set to `NULL` to not test. [is_finite()]
#'   is designed to allow more flexibility to this test.
#'
#' @returns \[`TRUE` | `FALSE`] `TRUE` if `x` passes the test, `FALSE` otherwise.
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
# TODO: Long vectors, and others? Allow generic implementations?



# TRUE and FALSE ---------------------------------------------------------------

# CHECK: reconsider adding a @name, maybe use is_bool

#' Type checks - TRUE and FALSE values
#'
#' Check if an object is literally `TRUE` or `FALSE`, controlling for `NA`
#' values. [rlang::is_bool()] checks for either `TRUE` or `FALSE`.
#'
#' @param x \[`logical()`, `any`] For `are_*()`, a logical vector; for `is_*()`,
#'   an object to test.
#' @param na \[`character(1)`] How to treat `NA` values: `"f"` to return
#'  `FALSE`, or `"na"` to return `NA`.
#'
#' @returns
#' - \[`logical(length(x))`] For `are_*`: the vectorized or result of the test.
#' - \[`TRUE` | `FALSE`] For `is_*`: the scalar result of the test.
#'
#' @name predicates-true-false
NULL


#' @rdname predicates-true-false
#' @export
are_true <- function(x, na = "f") {
  # Checks:
  # - x must be a logical vector
  # - na must be one of "f", "na"


  # Main:
  switch(na,
    f = !is.na(x) & x,
    na = if_else2(is.na(x), NA, x)
  )
}


#' @rdname predicates-true-false
#' @export
is_true2 <- function(x, na = "f") {
  # Checks:
  # - na must be one of "f", "na"


  # Main:
  if (na == "na" && is.na(x)) {
    return(NA)
  }

  is_logical(x, 1) && !is.na(x) && x
}


#' @rdname predicates-true-false
#' @export
are_false <- function(x, na = "f") {
  # Checks:
  # - x must be a logical vector
  # - na must be one of "f", "na"


  # Main:
  switch(na,
    f = !(is.na(x) | x),
    na = if_else2(is.na(x), NA, x)
  )
}


#' @rdname predicates-true-false
#' @export
is_false2 <- function(x, na = "f") {
  # Checks:
  # - na must be one of "f", "na"


  # Main:
  if (na == "na" && is.na(x)) {
    return(NA)
  }
  is_logical(x, 1) && !is.na(x) && !x
}

#' @rdname predicates-true-false
#' @export
is_bool <- is_bool



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
#' @param x \[`any`] Any R object.
#' @param nan \[`character(1)`] How to treat `NaN` values: `"f"` to return
#'   `FALSE`, `"t"` to return `TRUE`, or `"na"` to return `NA`.
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
#' are_na2(x, nan = "t")
#' #> [1] FALSE FALSE FALSE  TRUE  TRUE
#'
#' are_na2(x, nan = "na")
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
are_na2 <- function(x, nan = "f", types = NULL) {
  # Checks:
  # - x must be atomic
  # - nan must be one of "f", "t", "na"
  # - type must be one of NULL, "logical", "integer", "double", "complex", "character"
  # TODO:


  # Main:
  if (!is_null(types)) {
    return(typeof(x) %in% types)
  }

  switch(sub("w", "", nan),
    f = is.na(x) & !is.nan(x),
    t = is.na(x),
    na = if_else2(is.nan(x), NA, is.na(x))
  )
}


#' @rdname is_na2
#' @export
is_na2 <- function(x, n = NULL, nan = "f", types = NULL) {
  # Checks: left to rlang and are_na2
  is_atomic(x, n = n) && all(are_na2(x, nan = nan, types = types))
}
