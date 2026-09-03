
# Other types ------------------------------------------------------------------

#' Predicates - Other types
#'
#' @description
#' Test if object is of a specific type ([typeof()]). The test is invariant to
#' attributes such as class. The general `is_type()` checks for any
#' user-supplied type.
#'
#' See the Details section for the full list of types.
#'
#' Functions from rlang: [rlang::is_null()], [rlang::is_weakref()].
#'
#' @usage
#' is_type(x, type, n = NULL)
#'
#' is_null(x)
#'
#' is_promise(x)
#'
#' is_dots(x, n = NULL)
#'
#' is_weakref(x)
#'
#' is_bytecode(x)
#'
#' is_externalptr(x)
#'
#' is_char(x)
#' is_any(x)
#'
#' @param x \[`any`] An object to test.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test. 
#'
#' @returns \[`logical(1)`] `TRUE` if `x` passes the test, `FALSE` otherwise.
#'
#' @details
#' The full list of R object types ([typeof()]) can be seen in ["R Internals"
#' section 1.1.3](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#The-_0060data_0027).
#'
#' The existing types and their corresponding C SEXP types are:
#' - Atomic vectors: _"logical"/LGLSXP_, _"integer"/INTSXP_,
#'      _"double"/"numeric"/REALSXP_, _"complex"/CPLXSXP_, _"raw"_/RAWSXP.
#' - Lists: _"list"/VECSXP_, _"pairlist"/LISTSXP_.
#' - Objects: _"s4"/S4SXP, "object"/OBJSXP_.
#' - Language-related: _"symbol"/"name"/SYMSXP_, _"language"/LANGSXP_,
#'   _"expression"/EXPRSXP_.
#' - Functions: _"closure"/CLOSXP_, _"builtin"/SPECIALSXP_,
#'   _"special"/BUILTINSXP_.
#' - Promises: _"promise"/PROMSXP_, _"..."/DOTSXP_.
#' - Environments: _"environment"/ENVSXP_.
#' - Null: _"NULL"/NILSXP_.
#' - Bytecode: _"bytecode"/BCODESXP_.
#' - External pointer: _"externalptr"/EXTPTRSXP_.
#' - Weak reference: _"weakref"/WEAKREFSXP_.
#' - Internal-only: _"char"/CHARSXP_, _"any"/ANYSXP_.
#'
#' @aliases is_null is_weakref
#' @rawNamespace export(is_null, is_weakref)
#'
#' @name predicates-other-types


#' @rdname predicates-other-types
#' @export
is_type <- function(x, type, n = NULL) {
  # Checks:
  # - type must be one of the typeof() values
  # - n can only be supplied for the types in TYPES_LENGTH
  # TODO:


  # Main:
  typeof(x) == type && (is.null(n) || length(x) == n)
}


#' @rdname predicates-other-types
#' @usage NULL
#' @export
is_promise <- function(x) {
  typeof(x) == "promise"
}


#' @rdname predicates-other-types
#' @usage NULL
#' @export
is_dots <- function(x, n = NULL) {
  typeof(x) == "dots" && (is.null(n) || length(x) == n)
}


#' @rdname predicates-other-types
#' @usage NULL
#' @export
is_bytecode <- function(x) {
  typeof(x) == "bytecode"
}


#' @rdname predicates-other-types
#' @usage NULL
#' @export
is_externalptr <- function(x) {
  typeof(x) == "externalptr"
}


#' @rdname predicates-other-types
#' @usage NULL
#' @export
is_char <- function(x) {
  typeof(x) == "char"
}


#' @rdname predicates-other-types
#' @usage NULL
#' @export
is_any <- function(x) {
  typeof(x) == "any"
}



# Functions --------------------------------------------------------------------

#' Predicates - Functions
#'
#' @description
#' There are three types ([typeof()]) of functions in R: `"closure"` (standard
#' functions), or `"builtin"`/`"special"` (primitive, special functions). See
#' [rlang::is_function()] for more details on their difference.
#'
#' - [rlang::is_function()] tests for any function type.
#' - [rlang::is_closure()] tests for closures.
#' - [rlang::is_primitive()] tests for `"builtin"` or `"special"`.
#' - [rlang::is_primitive_eager()] tests for `"builtin"`.
#' - [rlang::is_primitive_lazy()] tests for `"special"`.
#'
#' @usage
#' is_function(x)

#' is_closure(x)
#'
#' is_primitive(x)
#'
#' is_primitive_eager(x)
#'
#' is_primitive_lazy(x)
#'
#' @param x \[`any`] An object to test.
#'
#' @returns \[`logical(1)`] `TRUE` if `x` passes the test, `FALSE` otherwise.
#'
#' @aliases is_function, is_closure is_primitive is_primitive_eager is_primitive_lazy
#' @rawNamespace export(is_function, is_closure, is_primitive, is_primitive_eager, is_primitive_lazy)
#'
#' @name predicates-functions
NULL



# Collections ------------------------------------------------------------------

#' Predicates - Collection types
#'
#' @description
#' In R, there are several types ([typeof()]) that can "store elements":
#' - The atomic vector types (see [predicates-atomic]).
#' - `"list"`: the generic vector type (can store elements of any type). Tested
#'   by [rlang::is_list()].
#' - `"pairlist"`: a linked list, used in R's internals. Tested by
#'   `is_pairlist2()`.
#' - `"environment"`: a hash table, used to store variables and their values.
#'   Tested by `is_environment2()`.
#' - `"expression"`: a vector of unevaluated R code objects. Tested by
#'   [is_expression2()].
#'
#' See the 'Details' section for what behaviour you can expect from these types.
#'
#' Additionally:
#' - [rlang::is_vector()] tests for atomic or generic (list) vectors.
#' - `is_collection()` tests for any of the above collection types, with options
#'   to exclude any of them.
#'
#' @usage
#' is_list(x, n = NULL)
#'
#' is_pairlist2(x, n = NULL)
#'
#' is_environment2(x, n = NULL)
#'
#' is_expression2(x, n = NULL)
#'
#' is_vector(x, n = NULL)
#'
#' is_collection(x, n = NULL, expr = TRUE, pairlist = TRUE, env = TRUE)
#'
#' @param x \[`any`] An object to test.
#' @param n \[`integer(1)` | `NULL`] Length of `x`, set to `NULL` to not test.
#' @param expr,pairlist,env \[`flag`] Whether to include expression, pairlist,
#'   or environment as collections.
#'
#' @returns \[`logical(1)`] `TRUE` if `x` passes the test, `FALSE` otherwise.
#'
#' @details
#' All collection types:
#' - Have varying [length()].
#' - Can have [names()].
#' - Can have their elements accessed by names (if present) via `[`, `[[`, and `$`.
#' - All but environment can have their elements accessed by integer indexes
#'   too, and their names set via [`names(x) <- value`].
#'
#' Note that the result of acessing out-of-bounds indexes depends on the type and
#' operator, and is very quirky and inconsistent across R. See [Advanced R, 2nd
#' edition, chapter  4](https://adv-r.hadley.nz/subsetting.html) for more
#' details.
#'
#' Objects that are not considered collections:
#' - Objects of type `"s4"` or `"object"` are a 'collection of slots', but their
#'   data is stored in their attributes, which any R object can do, and their
#'   length is always 1.
#' - The `"dots"` object is a collection of promises, have varying length,
#'   names, etc. But it is low-level, and removed from this definition for
#'   simplicity.
#' - 'NULL' (and 'any') have length 0, and are often thought as empty collections,
#'   but in face of `integer(0)` and `list()`, such thought is not the best.
#' - 'language' and 'promise' can have names, but don't have varying length.
#' - All the other types have lenght 1, and cannot have names.
#'
#' See [rlang::is_namespace()] for another environment-related test.
#'
#' @aliases is_list is_vector
#' @rawNamespace export(is_list, is_vector)
#'
#' @name predicates-collections
NULL


#' @rdname predicates-collections
#' @usage NULL
#' @export
is_pairlist2  <- function(x, n = NULL) {
  typeof(x) == "pairlist" && (is.null(n) || length(x) == n)
}


#' @rdname predicates-collections
#' @usage NULL
#' @export
is_environment2 <- function(x, n = NULL) {
  typeof(x) == "environment" && (is.null(n) || length(x) == n)
}


#' @rdname predicates-collections
#' @usage NULL
#' @export
is_collection <- function(x, n = NULL, expr = TRUE, pairlist = TRUE, env = TRUE) {
  # Checks:
  # - n must pass is_integer_like(n, 1) or be NULL
  # - expr, pairlist, and env must be flags
  # TODO:

  # Main:
  (is_null(n) || lengh(x) == n) && (
    is_vector(x) ||
      (expr && is.expression(x)) ||
      (pairlist && is.pairlist(x)) ||
      (env && is.environment(x))
  )
}
# Check if include_* is TRUE or FALSE (??)
# Test for list, pairlist, expression, or environment, and primitive function (??)
# TODO: add dots (... is a collection of promises, can have names)
