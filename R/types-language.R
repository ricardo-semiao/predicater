
# Base type tests --------------------------------------------------------------

#' Predicates - Language objects
#'
#' @description
#' In R, there are three building blocks that compose the language itself:
#' - Syntactic literals: numbers, strings, and other literal values, e.g.
#'   `1.0`, `NULL`, etc. Thay can have many types ([typeof()]).
#' - Symbols: variable names, e.g. `x`, `sum`, etc. They are of type
#'   `"symbol"` (SYMSXP).
#' - Function calls: `sum(1, 2)`, `1 + 2`, `if (TRUE) 1 else 2`, etc. They are
#'   of type `"language"` (LANGSXP).
#'
#' For testing these building blocks, there are the following predicates:
#' - Syntatic literals: [rlang::is_syntactic_literal()].
#' - Symbols: [rlang::is_symbol()]. A symbol is parseable from R code
#'   if it passes `is_symbol_valid()`.
#' - Function calls: `is_language()`. A function call is parseable from R code
#'   if it passes `is_language_valid()`. Also consider [rlang::is_call()] for
#'   testing the name, namespace, and number of arguments of a call.
#' - Expressions vector: R has an additional type, `"expression"`, which is a
#'   list of elements from any of the three building blocks above. It can be
#'   tested with `is_expression2()`.
#'
#' @usage
#' is_syntactic_literal(x)
#'
#' is_symbol(x, name = NULL)
#'
#' is_symbol_valid(x)
#'
#' is_language(x)
#'
#' is_language_valid(x)
#'
#' is_expression2(x, n = NULL)
#'
#'
#' @param x \[`any`] An object to test.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#' @param name \[`character(1)` | `NULL`] An optional name or vector of names
#'   that the symbol should match. Set to `NULL` to not test.
#'
#' @returns \[`logical(1)`] `TRUE` if `x` passes the test, `FALSE` otherwise.
#'
#' @aliases is_syntactic_literal is_symbol
#' @rawNamespace export(is_syntactic_literal, is_symbol)
#'
#' @name predicates-language
NULL


#' @rdname predicates-language
#' @usage NULL
#' @export
is_symbol_valid <- function(x) {
  is_symbol(x) && is_parseable(x)
}


#' @rdname predicates-language
#' @usage NULL
#' @export
is_language <- function(x) {
  typeof(x) == "language"
}


#' @rdname predicates-language
#' @usage NULL
#' @export
is_language_valid <- function(x) {
  is_language(x) && is_parseable(x)
}


#' @rdname predicates-language
#' @usage NULL
#' @export
is_expression2 <- function(x, n = NULL) {
  # Checks:
  # - n must pass is_integer_like(n, 1) or be NULL
  # TODO:

  typeof(x) == "expression" && (is.null(n) || length(x) == n)
}



# Helpers ----------------------------------------------------------------------

#' @noRd
is_parseable <- function(x) {
  tryCatch(
    {
      parse(text = deparse(x))
      TRUE
    },
    error = function(e) FALSE
  )
}
