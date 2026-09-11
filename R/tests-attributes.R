
#' @include tests-helpers.R tests-menu.R
NULL



# Names ------------------------------------------------------------------------

#' Tests - Names
#'
#' @param x \[`any`] An object to test.
#' @param zero_len \[`TRUE` | `FALSE`] The result for objects with zero length.
#' @param non_collection \[`TRUE` | `FALSE`] The result for objects that are not
#'   collections (as defined by [is_collection()]).
#' @param na \[`TRUE` | `FALSE`] Are `NA` names allowed?
#' @param empty \[`TRUE` | `FALSE`] Are empty names allowed?
#' @param dups \[`TRUE` | `FALSE`] Are duplicate names allowed?
#' @param invalid \[`TRUE` | `FALSE`] Are non syntatic names allowed? As defined
#'   by [make.names()].
#' @param how \[`"names"` | `"attr"` | `"names2"`] How to get the names of `x`:
#'   `"names"` for [names()], `"attr"` for [attr()], and `"names2"` for
#'   [rlang::names2()].
#'
#' @name tests-names
NULL


#' @noRd
# core_names <- function(
#   x,
#   zero_len = TRUE, non_collection = FALSE,
#   na = FALSE, empty = FALSE, dups = FALSE, invalid = TRUE,
#   how = "names"
# ) {
#   tests <- initialize_tests(
#     zero_len, non_collection, na, empty, dups, invalid
#   )

#   nms <- switch(how,
#     names = names(x),
#     attr = attr(x, "names", TRUE),
#     names2 = names2(x)
#   )

#   # Main:
#   if (! is_collection(x)) {
#     tests$non_collection <- non_collection
#     return(tests)
#   }
#   tests$non_collection <- TRUE

#   if (length(x) == 0) {
#     tests$zero_len <- zero_len
#     return(tests)
#   }

#   if (is_null(nms)) {
#     tests$names <- FALSE
#     return(tests)
#   }

#   tests$na <- na || any(is.na(nms))
#   tests$empty <- empty || any(nms == "")
#   tests$dups <- dups || anyDuplicated(nms)
#   tests$invalid <- invalid || any(make.names(nms) != nms)

#   tests
# }
# TODO: we could swithc to TRUE = yes there are NAs, FALSE = no there are no
# NAs, and NULL = we don't care. Here it is not too useful, but in other
# functions yes


# #' @rdname tests-names
# #' @export
# test_names <- fn_core_to_test(core_names)


# #' @rdname tests-names
# #' @export
# assert_names <- fn_core_to_assert(core_names, list(
#   non_collection = \(attrs) "fails {.fn is_collection}",
#   zero_len = \(attrs) "has zero length",
#   names = \(attrs) "has no names",
#   na = \(attrs) "has {.val NA} names",
#   empty = \(attrs) "has empty names",
#   dups = \(attrs) "has duplicate names",
#   invalid = \(attrs) "has invalid names"
# ))
