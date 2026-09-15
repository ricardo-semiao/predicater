
# Length -----------------------------------------------------------------------

#' Metadata checks - Check if a collection is empty
#'
#' @description
#' Is a collection object (see [is_collection()]) empty, i.e. has zero length?
#' Similar to [rlang::is_empty()], but yields an error for non-collections,
#' including `NULL` by default.
#'
#' Note that objects of type 'S4' and 'object' are not considered collections,
#' even if they are 'filled' with slots/attributes.
#'
#' @param x `r ROXY$x()`
#' @param null \[`TRUE` | `FALSE`] Should `NULL` be considered a collection? If
#' false will error for `NULL`, else, will return `TRUE`.
#'
#' @returns `r ROXY$test_res()`
#'
#' @examples
#' is_empty2(list()) #> TRUE
#' is_empty2(list(1)) #> FALSE
#' is_empty2(list(NULL)) #> FALSE
#'
#' is_empty2(character(0)) #> TRUE
#' is_empty2(letters) #> FALSE
#'
#' # Only works for collections (see ?is_collection()):
#' try(is_empty2(sum))
#' #> Error in is_empty2(sum) : `x` is not a collection.
#'
#' # But can be made to work with NULL:
#' is_empty2(NULL, null = TRUE) #> TRUE
#'
#' @export
is_empty2 <- function(x, null = FALSE) {
  if (! is_collection(x, null = null)) {
    cli_abort("{.arg x} is not a collection.")
  }
  length(x) == 0L
}
# NOTE: for S4/object, it we at most could create a generic function



# C header ---------------------------------------------------------------------

# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Rest-of-header-1

# Other metadata:
# - altrep, scalar
# - mark, debug, trace, spare
# - named, gcgen, gccls

# gp bits:
# Character:
# gp bits 6,3,2,1 (encoding, see Encoding)
# gp bit 5 — Object resides in the global string pool (CHARSXP hash cache).
#
# Sym and env:
# is_locked_binding(sym, env): gp bit 14 — Locked variable binding.
# is_active_binding(sym, env): gp bit 15 — Active binding (closure-evaluated on read/write).
# is_env_cached(env): gp bit 15 — Environment participates in method dispatch hash caching.
#
# Promise and dots:
# is_promise_seen(prom): gp bit 0 — PRSEEN flag (evaluating loop detection).
# is_ddval_symbol(sym): gp bit 0 — Dot-dot variable marker (..1, ..2 from ...).
# is_arg_missing(arg): gp bits 0–1 — Argument list missingness/default-evaluation status.
