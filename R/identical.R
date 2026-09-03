
#' Test objects for exact equality with more flexibility
#'
#' Similar to [identical()] but with more flexibility on how to handle data and
#' attributes.
#' - `identical2()` test two objects for exact equality.
#' - `identical_reduce()` test multiple objects for exact equality.
#' - `identical_vec()` is a vectorized `identical2()`, comparing each element of
#'   `x` and `y`.
#' - `identical_flag()` is a rercursive `identical2()`, returning the comparison
#'   results in a list with same structure as `x`. Useful to flag where
#'   `x` and `y` differ.
#'
#' @param x,y \[`any`] Any R object.
#' @param single_NA \[`logical(1)`] Whether to treat `NA` values as a identical
#'   to each other.
#' @param single_zero \[`logical(1)`] Whether to treat `+0` and `-0` as
#'   identical to each other.
#' @param ord_data,ord_attrs \[`logical(1)` each] Whether to keep (`TRUE`) or
#'   ignore (`FALSE`) the order of the data and attributes in `x` and `y`.
#' @param tol_type \[`character(1)`] Type of tolerance to use when comparing
#'   numeric values. One of:
#'   - `"none"`: no tolerance (default).
#'   - `"abs"`: absolute tolerance.
#'   - `"rel"`: relative tolerance.
#' @param tol \[`numeric(1)`] Tolerance value.
#' @param ignore_data,ignore_attrs \[`character()` each] Names of data and
#'   attributes to ignore when comparing `x` and `y`.
#' @param ... For variants, arguments passed to `identical2()`.
#' @param fun \[`character(1)`] For variants, function to use for comparison.
#'   One of `"identical"` or `"identical2"`.
#' @param l \[`list()`] For `identical_reduce()`, a list of objects to compare.
#' @param accumulate \[`logical(1)`] For `identical_reduce()`, whether to return
#'   the accumulated tests' results or just the final result.
#'
#' @returns
#' - \[`logical(1)`] for `identical2()` and `identical_reduce()`.
#' - \[`logical(length(x))`] for `identical_vec()`.
#' - `identical_flag()` returns a object with the same structure as `x` and
#'   logical elements.
#'
#' @examples
#' x <- structure(c(1, 2, 3), a = "a", b = "b", c = "c")
#' y <- structure(c(3, 2, 1), b = "b", a = "a", c = "d")
#'
#' identical2(x, y) #> FALSE
#'
#' # Pre-sort data and ignore "c" attribute:
#' identical2(x, y, ord_data = FALSE, ignore_attrs = "c") #> TRUE
#'
#' # Consider attribute order:
#' identical2(x, y, ord_data = FALSE, ignore_attrs = "c", ord_attrs = TRUE) #> FALSE
#'
#' # No sorting but accept up to 2.1 numerical absolute tolerance:
#' identical2(x, y, tol_type = "abs", tol = 2.1, ignore_attrs = "c") #> TRUE
#'
#' # Vectorized comparison:
#' identical_vec(list(x, y, x, x), list(x, y, y, y))
#' #> TRUE  TRUE  FALSE  FALSE
#'
#' # Understading where the differences are:
#' identical_flag(x, y)
#' #> $.data
#' #> [1] FALSE  TRUE FALSE  # First (3 & 1) and third (1 & 3) elements are different
#' #>
#' #> $.attrs
#' #> $.attrs$.data
#' #> $.attrs$.data$a
#' #> $.attrs$.data$a$.data
#' #> [1] TRUE
#' #>
#' #> $.attrs$.data$b
#' #> $.attrs$.data$b$.data
#' #> [1] TRUE
#' #>
#' #> $.attrs$.data$c
#' #> $.attrs$.data$c$.data
#' #> [1] FALSE  # Attribute "c" ("c" & "d") is different
#'
#' @export
identical2 <- function(
  x, y,
  single_NA = TRUE, single_zero = TRUE,
  ord_data = TRUE, ord_attrs = FALSE,
  tol_type = "none", tol = sqrt(.Machine$double.eps),
  ignore_data = character(), ignore_attrs = character()
) {
  # Checks:
  # - single_NA, single_zero, ord_data, ord_attrs must be flags
  # - tol_type must be one of "none", "abs", "rel"
  # - tol must be numeric(1) in ]0, Inf[
  # - ignore_data, ignore_attrs must be non-NA character vectors
  # TODO:
  #test_msgs(checkmate::check_flag, single_NA, single_zero, ord_data, ord_attrs)
  #test_msg(checkmate::check_choice, tol_type, choices = c("none", "abs", "rel"))
  #if (! is_finite(tol) || tol <= 0) {
  #  cli_abort("{.arg tol} must be a finite number greater than 0.")
  #}
  #test_msgs(
  #  checkmate::check_character, ignore_data, ignore_attrs,
  #  args = list(any.missing = FALSE)
  #)


  # Main:
  attrs_x <- attributes(x)
  attrs_y <- attributes(y)

  if (! ord_data) {
    res <- ord_recurse(x, y)
    x <- res$x
    y <- res$y
  }

  if (length(ignore_data) > 0) {
    x <- x[! names(x) %in% ignore_data]
    y <- y[! names(y) %in% ignore_data]
  }

  if (length(ignore_attrs) > 0) {
    attributes(x) <- attrs_x[! names(attrs_x) %in% ignore_attrs]
    attributes(y) <- attrs_y[! names(attrs_y) %in% ignore_attrs]
  }

  if (tol_type != "none") {
    res <- tol_recurse(x, y, tol, tol_type = tol_type)
    x[] <- res$x
    y[] <- res$y
  }

  identical(
    x, y,
    num.eq = single_zero, single.NA = single_NA,
    attrib.as.set = !ord_attrs
  )
}
# TODO: single_nan, single_inf
# TODO: use attrs_rmv?
# TODO: order data by names, values, or both (currently only by values)
# TODO: allow ignore_data to accept vector of names or vector of indices. later
# could even accept mixed, regex, etc.
# NOTE: Not important but possible: make encoding matter, make altrep matter, ...
# TODO: cite using reduce_predicate and Map(identical) usages


#' @rdname identical2
#' @export
identical_flag <- function(x, y, ..., fun = "identical2") {
  # Checks:
  # - x and y must be lists
  # - fun must be one of "identical2" or "identical"
  # TODO:
  #test_msgs(checkmate::check_list, x, y)
  #test_msg(checkmate::check_choice, fun, choices = c("identical2", "identical"))


  # Main:
  res <- list(.data = NA, .attrs = NA)
  attrs_x <- attributes(x)
  attrs_y <- attributes(y)

  if (length(x) != length(y) || typeof(x) != typeof(y)) {
    "do nothing" # * Could early exit with FALSE
    res$.data <- FALSE
  } else if (is_list(x)) {
    if (is_named3(x)) {
      names_x <- names(x)
      res$.data <- set_names(vector("list", length(x)), names_x)
    } else {
      names_x <- seq_along(x)
      res$.data <- vector("list", length(x))
    }
    for (i in names_x) {
      res$.data[[i]] <- identical_flag(x[[i]], y[[i]], ..., fun = fun)
    }
  } else if (is_atomic(x)) {
    res$.data <- identical_vec(x, y, ..., fun = fun)
  }

  attrs_x_none <- is_null(attrs_x) || identical(names(attrs_x), "names")
  attrs_y_none <- is_null(attrs_y) || identical(names(attrs_y), "names")
  if (attrs_x_none) {
    res$.attrs <- if (attrs_y_none) NULL else FALSE
  } else {
    res$.attrs <- identical_flag(attrs_x, attrs_y, ..., fun = fun)
  }

  res
}
# TODO: allow attrib as set



# identical2 Helpers ----------------------------------------------------------

ord_recurse <- function(x, y) {
  if (length(x) != length(y) || typeof(x) != typeof(y)) {
    "do nothing" # * Could early exit with FALSE
  } else if (is_list(x)) {
    for (i in seq_along(x)) {
      res <- ord_recurse(x[[i]], y[[i]])
      x <- res$x
      y <- res$y
    }
  } else if (is_atomic(x)) {
    x <- sort(x)
    y <- sort(y)
  }

  list(x = x, y = y)
}

tol_recurse <- function(x, y, tol, tol_type) {
  if (length(x) != length(y) || typeof(x) != typeof(y)) {
    "do nothing" # * Could early exit with FALSE
  } else if (is_list(x)) {
    for (i in seq_along(x)) {
      res <- tol_recurse(x[[i]], y[[i]], tol)
      x <- res$x
      y <- res$y
    }
  } else if (is_numeric(x)) {
    tol2 <- switch(tol_type,
      abs = tol,
      rel = tol * max(abs(x), abs(y))
    )

    for (i in seq_along(x)) {
      if (abs(x[[i]] - y[[i]]) < tol2) x[[i]] <- y[[i]]
    }
  }

  list(x = x, y = y)
}
