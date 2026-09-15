
# Base type tests --------------------------------------------------------------

#' Type checks - Language objects
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
#' - Symbols: `is_symbol2` (based on [rlang::is_symbol()]).
#' - Function calls: `is_language()`. Also consider [rlang::is_call()] for
#'   testing the name, namespace, and number of arguments of a call.
#' - Expressions vector: R has an additional type, `"expression"`, which is a
#'   list of elements from any of the three building blocks above. It can be
#'   tested with `is_expression2()`.
#' - Any of the above: `is_code()`.
#'
#' @param x `r ROXY$x()`
#' @param name \[`character(1)` | `NULL`] An optional name or vector of names
#'   that the symbol or call should match. Set to `NULL` to not test.
#' @param valid \[`TRUE` | `FALSE`] Whether to test if the code is 'valid', i.e.
#'   can be [parse()]'d, or is a syntatic symbol.
#' @param empty \[`TRUE` | `FALSE`] Whether to allow the empty symbol.
#' @param n \[`integer(1)` | `NULL`] Number of elements in the expression vector
#'   or arguments in the call, set to `NULL` to not test.
#' @param ns \[`character(1)` | `NULL`] Namespace of the call, set to `NULL` to
#'   not test.
#' @param sym,lang,literal \[`TRUE` | `FALSE`] Whether to allow symbols,
#'   language objects, or syntactic literals.
#'
#' @returns `r ROXY$test_res()`
#'
#' @examples
#' is_syntactic_literal(1) #> TRUE
#' is_syntactic_literal("a") #> TRUE
#' is_syntactic_literal(NULL) #> TRUE
#'
#' is_symbol2(quote(x)) #> TRUE
#' is_symbol2(quote(x), name = "y") #> FALSE
#' is_symbol2(rlang::expr(), empty = FALSE) #> FALSE
#'
#' is_language(quote(x + 1)) #> TRUE
#' is_language(quote(f(x))) #> TRUE
#' is_language(quote(if (TRUE) 1 else 2)) #> TRUE
#' # See ?rlang::is_call() for is_call() examples
#'
#' is_expression2(expression(1, x, x + 1)) #> TRUE
#' is_expression2(rlang::exprs(1, x, x + 1)) #> FALSE (exprs generates a list)
#'
#' x <- 1
#' is_code(x) #> TRUE
#' # Identical to is_syntactic_literal(x) || is_symbol2(x) || is_language(x)
#'
#' is_code(x, literal = FALSE) #> FALSE
#' # Identical to is_symbol2(x) || is_language(x)
#'
#' @name predicates-language
NULL

#' @rdname predicates-language
#' @export
is_syntactic_literal <- is_syntactic_literal

#' @rdname predicates-language
#' @export
is_symbol2 <- function(x, name = NULL, valid = FALSE, empty = TRUE) {
  is_symbol(x, name) &&
    (!valid || x == sym(make.names(x))) &&
    (!empty || identical(x, expr()))
}

#' @rdname predicates-language
#' @export
is_language <- function(x, valid = FALSE) {
  (typeof(x) == "language") && (!valid || is_parseable(x))
}

#' @rdname predicates-language
#' @export
is_call <- is_call

#' @rdname predicates-language
#' @export
is_expression2 <- function(x, n = NULL, valid = FALSE) {
  # Checks:
  # - n must pass is_integer_like(n, 1) or be NULL
  # TODO:

  (typeof(x) == "expression") &&
    (is.null(n) || length(x) == n) &&
    (!valid || all(vapply_lgl(x, is_parseable)))
}

#' @rdname predicates-language
#' @export
is_code <- function(
  x, sym = TRUE, lang = TRUE, literal = TRUE,
  valid = FALSE, empty = TRUE
) {
  (sym && is_symbol2(x, valid = valid, empty = empty)) ||
    (lang && is_language(x, valid)) ||
    (literal && is_syntactic_literal(x))
}



# Helpers ----------------------------------------------------------------------

#' @noRd
is_parseable <- function(x) {
  tryCatch(
    {
      parse(text = deparse(x))
      TRUE
    },
    error = \(cnd) FALSE
  )
}
# CHECK: consider exporting. Would do nice alongside is_code. Maybe just is_code
# with a 'valid' arg
