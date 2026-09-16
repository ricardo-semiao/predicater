
#' Compare - Exact equality with more flexibility
#'
#' @description
#' Similar to [identical()] but with more flexibility on how to handle data and
#' attributes.
#' - `identical2()` test two objects for exact equality.
#' - `identical_flag()` runs [identical()] or `identical2()` rercursively,
#'   returning the comparison results in a list with same structure as `x`.
#'   Useful to flag where `x` and `y` differ.
#'
#' To see if multiple objects are identical, wrap them in a list and use
#' [reduce_predicate()]. To vectorize over two lists, checking that each pair of
#' elements between them are identical, use `Map(identical2, x, y)` (see
#' [Map()]).
#'
#' @param x,y \[`any`] Any R object.
#' @param single_NA,single_zero \[`TRUE` | `FALSE` each] Whether to treat `NA`
#'   values as a identical to each other, and the same for `+0` vs. `-0`.
#' @param ord_data,ord_attrs \[`TRUE` | `FALSE` each] Whether to keep the order
#'   of the data and attributes in `x` and `y`.
#' @param tol_type \[`"none"` | `"abs"` | `"rel"`]
#'   Type of tolerance to use when comparing numeric values. One of:
#'   - `"none"`: no tolerance (default).
#'   - `"abs"`: absolute tolerance.
#'   - `"rel"`: relative tolerance.
#' @param tol \[`numeric(1)`] Tolerance value.
#' @param ignore_data \[`character()` | `integer()`] indexes or names of
#'   elements to ignore when comparing `x` and `y`. Names are compared against
#'   [rlang::names2()].
#' @param ignore_attrs \[`list()`] Arguments to pass to [attrs_rmv()] to remove
#'   attributes from `x` and `y`
#' @param ... For `identical_flag()`: arguments passed to `identical2()` or
#'   [identical()].
#' @param fun \[`"identical"` | `"identical2"`] For `identical_flag()`: function
#'   to use for comparison.
#' @param .is_attrs \[`TRUE` | `FALSE`] For internal use only.
#'
#' @returns
#' - \[`TRUE` | `FALSE`] For `identical2()`: the scalar result of the test.
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
#' identical2(
#'   x, y, ord_data = FALSE,
#'   ignore_attrs = list(exact = c("c"))
#' ) #> TRUE
#'
#' # Consider attribute order:
#' identical2(
#'   x, y, ord_data = FALSE,
#'   ignore_attrs = list(exact = c("c")), ord_attrs = TRUE
#' ) #> FALSE
#'
#' # No sorting but accept up to 2.1 numerical absolute tolerance:
#' identical2(
#'   x, y, tol_type = "abs", tol = 2.1,
#'   ignore_attrs = list(exact = c("c"))
#' ) #> TRUE
#'
#' # Understading where the differences are:
#' identical_flag(x, y)
#' #> $.data
#' #> c(FALSE, TRUE, FALSE) # First (3 & 1) and third (1 & 3) elements are different
#' #>
#' #> $.attrs
#' #> $.attrs$a
#' #> $.attrs$a$.data
#' #> TRUE
#' #>
#' #> $.attrs$b
#' #> $.attrs$b$.data
#' #> TRUE
#' #>
#' #> $.attrs$c
#' #> $.attrs$c$.data
#' #> FALSE # Attribute "c" ("c" & "d") is different
#'
#' @export
identical2 <- function(
  x, y,
  single_NA = TRUE, single_zero = TRUE,
  ord_data = TRUE, ord_attrs = FALSE,
  tol_type = "none", tol = sqrt(.Machine$double.eps),
  ignore_data = character(), ignore_attrs = list()
) {
  # Checks:
  # - single_NA, single_zero, ord_data, ord_attrs must be flags
  # - tol_type must be one of "none", "abs", "rel"
  # - tol must be numeric(1) in ]0, Inf[
  # - ignore_data, ignore_attrs must be non-NA character vectors
  # TODO:


  # Main:
  if (length(ignore_data) > 0) {
    if (is_character(ignore_data)) {
      x <- x[! names2(x) %in% ignore_data]
      y <- y[! names2(y) %in% ignore_data]
    } else {
      x <- x[- ignore_data]
      y <- y[- ignore_data]
    }
  }

  if (! ord_data) {
    res <- ord_recurse(x, y)
    x[] <- res$x
    y[] <- res$y
  }

  if (length(ignore_attrs) > 0) {
    x <- do.call(attrs_rmv, c(list(x), ignore_attrs))
    y <- do.call(attrs_rmv, c(list(y), ignore_attrs))
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
# TODO: single_nan, single_inf, make single.NA work as we would want
# TODO: order data by names, values, or both (currently only by values)
# TODO: allow ignore_data to accept vector of names or vector of indices. later
# could even accept mixed, regex, etc.
# CHECK: Less important but possible: make C header metadata matter


#' @rdname identical2
#' @export
identical_flag <- function(x, y, ..., fun = "identical2", .is_attrs = FALSE) {
  # Checks:
  # - x and y must be lists
  # - fun must be one of "identical2" or "identical"
  # TODO:


  # Main:
  f <- switch(fun,
    identical = identical,
    identical2 = identical2
  )
  res <- list(.data = NA, .attrs = NA)
  attrs_x <- attributes(x)
  attrs_y <- attributes(y)

  if (length(x) != length(y) || typeof(x) != typeof(y)) {
    "do nothing" # * Could early exit with FALSE
    res$.data <- FALSE

  } else if (is_atomic(x)) {
    res$.data <- mapply(f, x, y, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    names(res$.data) <- names(x)

  } else if (is_list(x)) {
    if (has_names_valid(x)) {
      names_x <- names(x)
      res_loop <- set_names(vector("list", length(x)), names_x)
    } else {
      names_x <- seq_along(x)
      res_loop <- vector("list", length(x))
    }

    for (i in names_x) {
      res_loop[[i]] <- identical_flag(x[[i]], y[[i]], ..., fun = fun)
    }

    if (.is_attrs) {
      res <- res_loop
    } else {
      res$.data <- res_loop
    }
  }

  attrs_x_none <- is_null(attrs_x) || identical(names(attrs_x), "names")
  attrs_y_none <- is_null(attrs_y) || identical(names(attrs_y), "names")
  if (attrs_x_none) {
    res$.attrs <- if (attrs_y_none) NULL else FALSE
  } else {
    res$.attrs <- identical_flag(attrs_x, attrs_y, ..., fun = fun, .is_attrs = TRUE)
  }

  res
}
# TODO: allow attrib as set



# identical2 Helpers ----------------------------------------------------------

ord_recurse <- function(x, y) {
  if (length(x) != length(y) || typeof(x) != typeof(y)) {
    "do nothing" # NOTE: Could early exit with FALSE or something
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
