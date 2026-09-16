
# Prototypes -------------------------------------------------------------------

#' Compare - Check if object is of prototype
#'
#' @description
#' Check if `.x` has the same attributes as `.ptype`, with more flexibility than
#' [vctrs::vec_is()]. A prototype is a template object that defines metadata:
#' [typeof()], [length()], names of the attributes, and their values, everything
#' besides data. With `is_ptype()`, the user can specify which metadata to check
#' and how.
#'
#' `is_ptype_list()` recursively checks if a list object has the correct
#' elements given a list prototype, useful when dealing with complex objects
#' like `list(a = integer(), b = list(c = character(), d = double()))`.
#'
#' @param .x `r ROXY$x()`
#' @param .ptype \[`any`] A prototype object that defines metadata to check
#'  against `.x`.
#' @param .length,.attrs \[`character(1)` | `\(vx, vp) {}`]
#'  How to check the non-attribute metadata -- `length()`, and attributes names
#'  -- of `.x` against `ptype`. Can be either: a function that takes the
#'  metadata's value of `.x` (`vx`) and `.ptype` (`vp`) as arguments and returns
#'  a boolean; or a string that specifies a predefined function (see
#'  [Details](#details)).
#' @param class,dim,names,row.names,dimnames,...
#'   \[`character(1)` | `\(vx, vp) {}`]
#'   How to check the attributes -- each argument is an attribute name -- of
#'   `.x` against `.ptype`. With a function or string (same as above).
#' @param .named \[`TRUE` | `FALSE`] For `is_ptype_list()`, whether to use the
#'   names of `.x` and `.ptype` for matching them (`TRUE`), or the order
#'   (`FALSE`).
#' @param .depth \[`integer(1)`] For `is_ptype_list()`, how many levels of
#'   recursion to check. `1` means check the first level elements only.
#'
#' @returns `r ROXY$test_res()`
#'
#' @details
#' Predefined check functions:
#' - `"no"`: don't check, always return `TRUE`.
#' - `"id"`: check if `identical(vp, vx)`.
#' - `"=="`: check if `all(vp == vx)`.
#' - `"0|id"` and `"0|=="` ('zero or equal'): same as above, but return `TRUE`
#'   if `vp` has zero
#'   lenght or is `0`.
#' - `"id_ord"` and `"==_ord"`: same as above, but ignore the order of values.
#' - `"id_sub"` and `"==_sub"`: same as above, but allow `vx` to be a subset of
#'  `vp`.
#'
#' Attributes of attributes of `.x` or `.ptype` are ignored.
#'
#' @examples
#' # By default, length is checked when the prototype's is not 0:
#' is_ptype(1:10, integer())  #> TRUE
#' is_ptype(1:10, integer(9)) #> FALSE
#'
#' # Same is true for class, dim, names, row.names, and dimnames attributes:
#' is_ptype(matrix(1:9, 3, 3), integer()) #> TRUE
#' is_ptype(matrix(1:9, 3, 3), integer(), dim = "==") #> FALSE
#'
#' # For less common attributes, checks need to be specified in `...`:
#' is_ptype(
#'   factor(c("a", "b")), factor(levels = c("a", "b", "c"))
#' )
#' #> TRUE
#'
#' is_ptype(
#'   factor(c("a", "b")), factor(levels = c("a", "b", "c")),
#'   levels = "=="
#' )
#' #> FALSE
#'
#' # Complex objects can be checked with `is_ptype_list()`:
#' schema <- list(a = integer(1), b = data.frame(), c = double())
#' is_ptype_list(list(a = 1L, b = mtcars, c = rnorm(sample(1:10))), schema)
#' #> TRUE
#'
#' @export
is_ptype <- function(
  .x, .ptype,
  .length = "0|==", .attrs = "no",
  class = "0|==", dim = "0|==",
  names = "0|==", row.names = "0|==", dimnames = "0|id",
  ...
) {
  # Checks:
  # - .length, .attrs, class, dim, names, row.names, dimnames, ... must
  #   conform to docs
  ptype_check_ops(
    .length, .attrs,
    class, dim, names, row.names, dimnames, ...
  )


  # Main:
  if (typeof(.x) != typeof(.ptype)) {
    return(FALSE)
  }

  checks_dots <- c(...)
  if (any(c(".x", ".ptype", ".length", ".attrs") %in% names(checks_dots))) {
    cli_warn("Arguments in {.arg ...} must not be named {.val .x}, {.val .ptype}, \\
    {.val .typeof}, {.val .length}, or {.val .attrs}, which will be ignored.")
  }

  checks_all <- c(
    ".length", ".attrs",
    "class", "dim", "names", "row.names", "dimnames", names(checks_dots)
  )
  checks_all_ops <- c(
    .length = .length, .attrs = .attrs,
    class = class, dim = dim, names = names, row.names = row.names,
    dimnames = dimnames, checks_dots
  )

  x_attrs <- get_attrs(.x)
  p_attrs <- get_attrs(.ptype)
  attrs_in_x_or_irrelevant <- c(names(x_attrs), setdiff(checks_all, names(p_attrs)))

  is <- TRUE
  for (attr in checks_all) {
    check_op <- CHECK_OPS[[checks_all_ops[[attr]]]]

    is <- is &&
      (attr %in% attrs_in_x_or_irrelevant) &&
      check_op(x_attrs[[attr]], p_attrs[[attr]])

    if (!is) break
  }

  is
}
# TODO: consider all headers of the c data (altrep, ...)
# TODO: check if we need to require that specific signarure for op function in
# the type hing


#' @rdname is_ptype
#' @export
is_ptype_list <- function(.x, .ptype, .named = TRUE, .depth = 1, ...) {
  # Setup:
  if (.depth == 0) {
    return(is_ptype(.x, .ptype, ...))
  }


  # Checks:
  # - .named must be a flag
  # - .depth must be >= 0 integerish(1)
  # - .x and .ptype must conform
  # TODO:

  if (! is_list(.x) || ! is_list(.ptype)) {
    cli_abort("is_ptype_list() only works for lists.")
  }
  if (.named && (! identical(names(.x), names(.ptype)))) {
    cli_abort("{.arg .x} and {.arg ..ptype} must have the same names.")
  }
  if (!.named && (length(.x) != length(.ptype))) {
    cli_abort("{.arg .x} and {.arg .ptype} must have the same length.")
  }
  # if (any(c(".x", ".ptype", ".named", ".depth") %in% names(list2(...)))) {
  #   cli_inform("{.arg ...} must not contain {.val .x}, {.val .ptype}, \\
  #   {.val .named}, or {.val .depth}, which will be ignored.")
  # }


  # Main:
  if (!.named) {
    names(.x) <- seq_along(.x)
    names(.ptype) <- seq_along(.ptype)
    .x <- set_names(.x, seq_along(.x))
  }

  is <- TRUE
  for (nm in names(.ptype)) {
    xi <- .x[[nm]]
    pi <- .ptype[[nm]]

    if (is_list(pi)) {
      is <- is &&
        is_list(xi) &&
        is_ptype_list(xi, pi, .named, .depth - 1, ...)
    } else {
      is <- is && is_ptype(xi, pi, ...)
    }

    if (!is) break
  }

  is
}
# TODO: reconsider name, is_ptype_rec?
# TODO: add .depth = NULL that infers depth from .ptype



# Helpers ----------------------------------------------------------------------

#' @noRd
get_attrs <- function(x) {
  c(
    .length = list(length(x)),
    .attrs = list(names(attributes(x)) %||% character()),
    attributes(x)[]
  )
}


#' @noRd
ptype_check_ops <- function(...) {
  ops_quos <- enquos(...)
  for (q in ops_quos) {
    op <- eval_tidy(q)
    cond <- (is_function(op) && all(names(formals(args(op))) == c("vx", "vp"))) ||
      (is_character(op, 1) && !is.na(op) && op %in% names(CHECK_OPS))
    if (!cond) {
      cli_abort("{.arg {as_label(q)}} must be a function with signature \\
      ({.code vx, vp}) or one of {.val {names(CHECK_OPS)}}.")
    }
  }
}


CHECK_OPS <- list(
  "no" = \(vx, vp) TRUE,
  "id" = \(vx, vp) identical(vx, vp),
  "==" = \(vx, vp) isTRUE(all.equal(vx, vp)),
  "0|==" = \(vx, vp) (length(vp) == 0 || vp == 0) || isTRUE(all.equal(vx, vp)),
  "0|id" = \(vx, vp) (length(vp) == 0 || vp == 0) || identical(vx, vp),
  "id_ord" = \(vx, vp) identical(sort(vx), sort(vp)),
  "==_ord" = \(vx, vp) isTRUE(all.equal(sort(vx), sort(vp))),
  "id_sub" = \(vx, vp) {
    lx <- length(vx)
    lp <- length(vp)
    lx <= lp && identical(vx[seq_len(lp)], vp)
  },
  "==_sub" = \(vx, vp) {
    lx <- length(vx)
    lp <- length(vp)
    lx <= lp && isTRUE(all.equal(vx[seq_len(lp)], vp))
  }
)
# TODO: change all.equal to something == based
