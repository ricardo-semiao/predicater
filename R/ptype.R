
# Prototypes -------------------------------------------------------------------

#' Check if object is of prototype
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
#' @param .x \[`any`] Any R object.
#' @param .ptype \[`any`] A prototype object that defines metadata to check
#'  against `.x`.
#' @param .typeof,.length,.attrs \[`character(1)` | `\(vx, vp) {}`]
#'  How to check the non-attribute metadata -- `typeof()`, `length()`, and
#'  attributes names -- of `.x` against `ptype`. Can be either: a function that
#'  takes the metadata's value of `.x` (`vx`) and `.ptype` (`vp`) as arguments
#'  and returns a boolean; or a string that specifies a predefined function (see
#'  [Details](#details)).
#' @param class,dim,names,row.names,dimnames,...
#'   \[`character(1)` | `\(vx, vp) {}`] How to check the attributes -- each
#'   argument is an attribute name -- of `.x` against `.ptype`. With a function
#'   or string (same as above).
#' @param .named \[`TRUE` | `FALSE`] For `is_ptype_list()`, whether to use the
#'   names of `.x` and `.ptype` for matching them (`TRUE`), or the order
#'   (`FALSE`).
#' @param .depth \[`integer(1)`] For `is_ptype_list()`, how many levels of
#'   recursion to check. `1` means check the first level elements only.
#'
#' @details
#' Predefined check functions:
#' - `"no"`: don't check, always return `TRUE`.
#' - `"id"`: check if `identical()`.
#' - `"=="`: check if `all(equal())`.
#' - `"id_0n"` and `"==_0n"`: same as above, but return `TRUE` if `vp` is `NULL`
#'  or `0`.
#' - `"id_ord"` and `"==_ord"`: same as above, but ignore the order of values.
#' - `"id_sub"` and `"==_sub"`: same as above, but allow `vx` to be a subset of
#'  `vp`.
#'
#' @examples
#' # By default, length is checked when the prototype's is not 0:
#' is_ptype(1:10, integer()) #> TRUE
#' is_ptype(1:10, integer(9)) #> FALSE
#'
#' # You can change the default check for common attributes:
#' is_ptype(matrix(1:9, 3, 3), integer()) #> FALSE
#' is_ptype(matrix(1:9, 3, 3), integer(), dim = "no") #> TRUE
#'
#' # Less common attributes' checks need to be specified in `...`:
#' is_ptype(
#'   factor(c("a", "b")), factor(levels = c("a", "b", "c")),
#'   levels = "=="
#' )
#' #> FALSE
#'
#' # Complex objects can be checked with `is_ptype_list()`:
#' is_ptype_list(
#'   list(a = 1L, b = mtcars, c = rnorm(sample(1:10))),
#'   list(a = integer(1), b = data.frame(), c = double())
#' )
#' #> TRUE
#'
#' @export
is_ptype <- function(
  .x, .ptype,
  .typeof = "==", .length = "==_0n", .attrs = "no",
  class = "==_0n", dim = "==_0n",
  names = "==_0n", row.names = "==_0n", dimnames = "id_0n",
  ...
) {
  # Checks:
  # - .typeof, .length, .attrs, class, dim, names, row.names, dimnames, ... must
  #   conform to docs
  ptype_check_ops(
    .typeof, .length, .attrs,
    class, dim, names, row.names, dimnames, ...
  )


  # Main:
  checks_extra <- c(.typeof = .typeof, .length = .length, .attrs = .attrs)

  checks_available <- names(attributes(.ptype))
  checks_in_x <- names(attributes(.x))
  checks <- c(...)

  checks_unused <- which(! names(checks) %in% checks_available)
  if (length(checks_unused) > 0) {
    cli_inform("{.arg ...} contains attributes not present in {.arg .ptype}, \\
    and will be ignored: ({.val {names(checks_unused)}}).")
  }

  if (any(c(".x", ".ptype", ".typeof", ".length", ".attrs") %in% names(checks))) {
    cli_inform("{.arg ...} must not contain {.val .x}, {.val .ptype}, \\
    {.val .typeof}, {.val .length}, or {.val .attrs}, which will be ignored.")
  }

  x_attrs <- get_attrs(.x)
  p_attrs <- get_attrs(.ptype)

  checks_available <- c(names(checks_extra), checks_available)
  checks <- c(checks_extra, checks)
  for (c in c("class", "dim", "names", "row.names", "dimnames")) {
    if (! c %in% names(checks)) checks[[c]] <- get(c)
  }
  checks_in_x <- c(checks_in_x, ".typeof", ".length", ".attrs")

  is <- TRUE
  for (attr in intersect(checks_available, names(checks))) {
    check_op <- CHECK_OPS[[checks[[attr]]]]

    is <- is &&
      (attr %in% checks_in_x) &&
      check_op(x_attrs[[attr]], p_attrs[[attr]])

    if (!is) break
  }

  is
}
# TODO: "==|0" and "==|null"
# CHECK: why do we allow control over typeof check? just remove it?
# TODO: consider all headers of the c data (altrep, ...)


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
    .x <- set_names(.x, seq_along(.x)) # TODO: switch to names()<- ?
    .ptype <- set_names(.ptype, seq_along(.ptype))
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
    .typeof = typeof(x), .length = length(x),
    .attrs = names(attributes(x)), attributes(x)[]
  )
}


#' @noRd
ptype_check_ops <- function(...) {
  ops_quos <- enquos(...)
  for (q in ops_quos) {
    op <- eval_tidy(q)
    cond <- (is_function(op) && all(names(formals(args(op))) == c("vx", "vp"))) ||
      (is_string(op) && op %in% names(CHECK_OPS)) # TODO: reconsider is_string
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
  "==_0n" = \(vx, vp) (is_null(vp) || length(vp) == 0 || vp == 0) || isTRUE(all.equal(vx, vp)),
  "id_0n" = \(vx, vp) (is_null(vp) || length(vp) == 0 || vp == 0) || identical(vx, vp),
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
