
# Base tests -------------------------------------------------------------------

#' Type checks - Objects
#'
#' @description
#' Functions to test for 'object'-related properties:
#' - `has_class()`: checks if `x` has a "class" attribute.
#' - `has_object_bit()`: checks if `x` has the "object" bit set, identical to
#'   [is.object].
#' - `has_s4_bit()`: checks if `x` has the "S4" bit set, identical to [isS4].
#' - `is_s4()`: checks if `x` is of type "S4".
#' - `is_object()`: checks if `x` is of type "object".
#' - `is_object_like()`: checks if object has the "object" bit, while testing
#'   for inconsistencies with the other properties above.
#'
#' @usage
#' has_class(x, empty = "t", bad = "f")
#'
#' has_object_bit(x)
#'
#' has_s4_bit(x)
#'
#' is_s4(x)
#'
#' is_object(x)
#'
#' is_object_like(x, bad = "warn")
#'
#' @param x \[`any`] Object to check.
#' @param empty \[`"t"` | `"f"`] How to handle empty class attribute in
#'   `has_class()`: `"t"` to return `TRUE`, `"f"` to return `FALSE`.
#' @param bad \[`character(1)`] How to handle inconsistencies in
#'   `is_object_like()`: `"warn"` to issue a warning, `"stop"` to throw an
#'   error, or `"f"` to return `FALSE`.
#'
#' @returns \[`TRUE` | `FALSE`] `TRUE` if `x` passes the test, `FALSE` otherwise.
#'
#' @name predicates-objects
NULL


#' @rdname predicates-objects
#' @usage NULL
#' @export
has_object_bit <- is.object


#' @rdname predicates-objects
#' @usage NULL
#' @export
has_s4_bit <- isS4


#' @rdname predicates-objects
#' @usage NULL
#' @export
has_class <- function(x, empty = "t", bad = "f") {
  # Checks:
  # - bad must be one of t, f
  class <- attr(x, "class", TRUE)

  if (is_null(class)) {
    FALSE
  } else if (!has_object_bit(x) || !is_character(class) || anyNA(class)) {
    switch(bad, t = TRUE, f = FALSE)
  } else if (any(class == "")) {
    switch(empty, t = TRUE, f = FALSE)
  } else {
    TRUE
  }
}


#' @rdname predicates-objects
#' @usage NULL
#' @export
is_object_like <- function(x, bad = "warn") {
  # Checks:
  # - bad must be one of "warn", "stop", "f"
  # TODO:


  # Main:
  has_class <- has_class(x)
  has_object_bit <- has_object_bit(x)
  has_s4_bit <- has_s4_bit(x)

  is_bad <- has_class != has_object_bit ||
    (has_s4_bit && !has_object_bit) ||
    (typeof(x) %in% c("object", "S4") && !has_object_bit) ||
    (typeof(x) == "S4" && !has_s4_bit)
  # TODO: adhere to full list of relations below

  if (is_bad) {
    switch(bad,
      warn = do.call(cli_warn, CNDS$is_object_like()),
      f = return(FALSE)
    )
  }

  has_object_bit
}
# CHECK: consider allowing user chosing the final test (has_bit or other)
# List of relations betwee: class, object bit, s4 bit, s4 typeof, object typeof
# - class -> object bit
# - object bit -> class
# - s4 bit -> class & -> object bit
# - s4 typeof -> class & -> object bit, -> s4 bit
# - object typeof -> class & -> object bit

#' @rdname predicates-objects
#' @usage NULL
#' @export
is_s4 <- function(x) {
  typeof(x) == "S4"
}
# Note the difference with s4 bit, and cite object_system


#' @rdname predicates-objects
#' @usage NULL
#' @export
is_object <- function(x) {
  typeof(x) == "object"
}



# Object system ----------------------------------------------------------------

#' Type checks - Determine object-oriented system of object
#'
#' @description
#' R has many object-oriented systems, including S3, S4, RC, R6, R.oo, S7,
#' proto, and ggproto. This function determines to which of these an object
#' belongs, extending the test done by [sloop::otype()].
#'
#' If `x` fails [is.object()], the function returns `"base"`. `x` matches `"S3"`
#' if it is an object but does not match any other system, so systems not
#' implemented here will fall into that category.
#'
#' `is_system()` tests if an object belongs to a specific system.
#'
#' @param x \[`any`] Object to check.
#' @param system \[`character(1)`] Object-oriented system to check for.
#'
#' @returns
#' - \[`character(1)`] For `object_system()`: the object-oriented system of `x`.
#' - \[`TRUE` | `FALSE`] For `is_system()`: `TRUE` if `x` belongs to `system`,
#'   `FALSE` otherwise.
#'
#' @details
#' The implemented check is as below:
#' 1. If not [is.object()], return `"base"`.
#' 2. Else, if [isS4()], return `"RC"` if `is(x, "refClass")`, else return
#'    `"S4"`.
#' 3. Else, if `inherits_any(x, "S7_object")` and `typeof(x) == "object"` (as an
#'    additional safeguard), return `"S7"`.
#' 4. Else, if `inherits_any(x, "R6")` and [rlang::is_environment()], return
#'    `"R6"`.
#' 5. Else, if `inherits_any(x, "ggproto")` and [rlang::is_environment()],
#' 5. return
#'    `"ggproto"`.
#' 6. Else, if `inherits_any(x, "proto")` and `all(c(".super", ".that") %in%
#'    names(x))` and [rlang::is_environment()], return `"proto"`.
#' 7. Else, if `inherits_any(x, "Object")` and `all(c(".env", "...modifiers",
#'    "...finalize") %in% names(attributes(x)))` and `is_logical(x, 1)`, return
#'    `"R.oo"`.
#'
#' @examples
#' object_system(1:10) #> "base"
#'
#' object_system(factor(letters)) #> "S3"
#'
#' track <- setClass("track", slots = c(x = "numeric", y = "numeric"))
#' xs4 <- track(x = 1:10, y = 1:10 + rnorm(10))
#' object_system(xs4) #> "S4"
#'
#' @export
object_system <- function(x) {
  # Main:
  if (! is_attrs_allowed(x, warn = "t")) {
    return("base")
  }
  attrs <- attributes(x)

  has_roo_attrs <- (
    (".env" %in% names(attrs)) &&
      all(c("...modifiers", "...finalize") %in% names(attrs$.env))
  )
  has_proto_names <- all(c(".super", ".that") %in% names(x))

  if (! is_object_like(x)) {
    "base"
  } else if (has_s4_bit(x)) {
    if (methods::is(x, "refClass")) { # CHECK: try to remove this dep on methods
      "RC"
    } else {
      "S4"
    }
  } else if (inherits_any(x, "S7_object") && is_object(x)) {
    "S7"
  } else if (inherits_any(x, "R6") && is_environment(x)) {
    "R6"
  } else if (inherits_any(x, "ggproto") && is_environment(x)) {
    "ggproto"
  } else if (inherits_any(x, "proto") && is_environment(x) && has_proto_names) {
    "proto"
  } else if (inherits_any(x, "Object") && is_logical(x, 1) && has_roo_attrs) {
    "R.oo"
  } else {
    "S3"
  }
}
# NOTE: this is challenging because there are many mixed elements: the class
# attribute, the object bit, the s4 bit, the object and s4 typeofs, the
# packages' table of defined classes, ...
# TODO: include mutatr, what else?


#' @rdname object_system
#' @export
is_system <- function(x, system) {
  object_system(x) == system
}



# Helpers ----------------------------------------------------------------------

CNDS$is_object_like <- function(env = caller_env()) {
  list(
    c(
      "!" = "Object bit and class attribute are inconsistent.",
      "i" = "See this condition's `rs_object` attribute for the object in question." 
    ),
    class = "rs_object_error",
    call = env,
    rs_object_error = list(
      x = env$x,
      has_object_bit = env$has_s4_bit,
      has_s4_bit = env$has_s4_bit,
      has_class_attr = env$has_class,
      is_object_type = env$is_object_type
    )
  )
}
