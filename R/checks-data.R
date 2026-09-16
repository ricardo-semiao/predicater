
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
#' @returns `r ROXY$test_res(na = TRUE)`. If `na.rm = TRUE`, the result is
#'   always `TRUE` or `FALSE`.
#'
#' @examples
#' # Control the order of the check with `order` argument
#' is_sorted(1:5) #> TRUE
#' is_sorted(1:5, order = "desc") #> FALSE
#'
#' # Control the handling of NA values with `na.rm` argument:
#' is_sorted(c(1, 2, NA, 4), na.rm = TRUE) #> TRUE
#'
#' # Control the strictness of the order with `strictly` argument:
#' is_sorted(c(1, 2, 2, 4), strictly = TRUE) #> FALSE
#'
#' @export
is_sorted <- function(x, order = "asc", na.rm = FALSE, strictly = FALSE) {
  # Checks:
    switch(order,
    asc = !is.unsorted(x, na.rm = na.rm, strictly = strictly),
    desc = !is.unsorted(rev(x), na.rm = na.rm, strictly = strictly)
  )
}


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
#' @returns `r ROXY$test_res()`
#'
#' @examples
#' x <- c(5, 7, 5, 9, 9)
#'
#' # Default mode checks if all values in `x` are in `yes` and not in `no`:
#' is_matching_set(x, yes = 1:10) #> TRUE
#' is_matching_set(x, yes = 1:8) #> FALSE
#' is_matching_set(x, yes = 1:10, no = 5) #> FALSE
#'
#' # Mode `any` passes even if there are values in `x` that are not in `yes`:
#' is_matching_set(x, yes = 1:6, mode = "any") #> TRUE
#'
#' # For mode `only`, all values in `x` must be in `yes` and vice versa:
#' is_matching_set(x, yes = 1:10, mode = "only") #> FALSE
#' is_matching_set(x, yes = c(5, 7, 9), mode = "only") #> TRUE
#'
#' # `yes` can be NULL to test only `no` (independent of mode):
#' is_matching_set(x, no = 11) #> TRUE
#' is_matching_set(x) #> TRUE
#'
#' @export
is_matching_set <- function(x, yes = NULL, no = NULL, mode = "all") {
  # Checks:
  # TODO
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
# TODO: deal with NA, cite in ROXY$set
# TODO: create are_ version


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
#' @param x \[`atomic()` | `list()`] An object to test.
#'
#' @returns
#' - \[`TRUE` | `FALSE`] For `any_duplicated()`: the scalar result of the test.
#' - \[`logical(length(x))`] For `are_duplicated()`: a logical vector of the same
#'   size as `x`, describing if each element is duplicated elsewhere.
#'
#' @inheritSection vctrs::vec_duplicate_any Missing values
#'
#' @examples
#' any_duplicated(1:10)       #> FALSE
#' any_duplicated(c(1, 1:10)) #> TRUE
#'
#' x <- c(10, 10, 20, 30, 30, 40)
#' are_duplicated(x) #> c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
#' duplicated(x) #> c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
#' # Note that `duplicated()` ignores the first instance of a duplicated value
#'
#' @export
any_duplicated <- vctrs::vec_duplicate_any

#' @rdname any_duplicated
#' @export
are_duplicated <- vctrs::vec_duplicate_detect
# TODO: add control over considering NAs, NaNs, and Infs as duplicates or not
