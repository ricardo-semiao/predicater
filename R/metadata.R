
# Length -----------------------------------------------------------------------

#' Predicates - Metadata
is_empty2 <- function(x, non_collection, ...) {
  if (! is_collection(x, ...) && ! is_null(x)) {
    switch(non_collection,
      f = return(FALSE),
      stop = cli_abort("{.arg x} is not a collection."),
    )
  }

  length(x) == 0L
}
# TODO: Cite that it ignores types s4/object
# NOTE: for S4/object, it we at most could create a generic function



# C header ---------------------------------------------------------------------

# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Rest-of-header-1

# TODO: other metadata:
# - altrep, scalar
# - mark, debug, trace, spare
# - named, gcgen, gccls

# TODO: gp bits:
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
