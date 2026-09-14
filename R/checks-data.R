
#' Data checks - Check if object is ordered
#'
#' This function is a wrapper around [is.unsorted()] to check if a vector is
#' sorted in ascending or descending order.
#'
#' @param x \[`atomic`] An atomic vector to test.
#' @param order \[`"asc"` | `"desc"`] The order to check for: `"asc"` for
#'   ascending, `"desc"` for descending.
#' @param na.rm \[`TRUE` | `FALSE`] If `TRUE`, `NA` values are removed before
#'   checking for order. If `FALSE`, `NA` values propagate and imply in an `NA`
#'   result.
#' @param strictly \[`TRUE` | `FALSE`] If `TRUE`, the function checks for strict
#'   order, meaning that no two elements can be equal.
#'
#' @returns \[`TRUE` | `FALSE` | `NA`] The scalar result of the test.
#'
#' @export
is_sorted <- function(x, order = "asc", na.rm = FALSE, strictly = FALSE) {
  switch(order,
    asc = !is.unsorted(x, na.rm = na.rm, strictly = strictly),
    desc = !is.unsorted(rev(x), na.rm = na.rm, strictly = strictly)
  )
}
# TODO: checks


#' Data checks - Check if object is in (and/or not in) a set
#'
#' This function checks if an object is in a set of values, and optionally if it
#' is not in another set of values. It can check if all, any, or only the values
#' are in the set.
#'
#' @param x \[`atomic` | `list()`] An atomic vector or list to test.
#' @param yes,no \[`atomic` | `list()` | `NULL`] A set of values that `x` should
#'   be in, and not be in, respectively. If `NULL`, this check is ignored.
#' @param mode \[`"all"` | `"any"` | `"only"`] The mode of the check: `"all"`
#'   for all values in `x` must be in `yes`, `"any"` for at least one value in
#'   `x` must be in `yes`, and `"only"` for all values in `x` must be in `yes`
#'   and all values in `yes` must be in `x`.
#'
#' @returns \[`TRUE` | `FALSE`] The scalar result of the test.
#'
#' @export
is_matching_set <- function(x, yes = NULL, no = NULL, mode = "all") {
  if (! is_empty(no) && any(x %in% no)) {
    return(FALSE)
  }

  if (is_empty(yes)) {
    return(TRUE)
  }

  x_in_yes <- x %in% yes
  switch(mode,
    all = all(x_in_yes),
    any = any(x_in_yes),
    only = all(x_in_yes) && all(yes %in% x)
  )
}
# TODO: checks, deal with NA, cite in ROXY$set


#' Data checks - Check for duplicate values
#'
#' @description
#' - `any_duplicated`: is identical to [vctrs::vec_duplicate_any()], and detects
#'   the presence of duplicated values, similar to [anyDuplicated()].
#' - `are_duplicated()` is identical to [vctrs::vec_duplicate_detect()], and
#'   returns a logical vector describing if each element of the vector is
#'   duplicated elsewhere. Unlike duplicated(), it reports all duplicated
#'   values, not just the second and subsequent repetitions.
#'
#' @inheritParams vctrs::vec_duplicate_any
#'
#' @returns
#' - \[`TRUE` | `FALSE`] For `any_duplicated()`: the scalar result of the test.
#' - \[`logical(length(x))`] For `are_duplicated()`: a logical vector of the same
#'   size as `x`, describing if each element is duplicated elsewhere.
#'
#' @inheritSection vctrs::vec_duplicate_any Missing values
#'
#' @export
any_duplicated <- vctrs::vec_duplicate_any

#' @rdname any_duplicated
#' @export
are_duplicated <- vctrs::vec_duplicate_detect
# TODO: add control over considering NAs, NaNs, and Infs as duplicates or not
