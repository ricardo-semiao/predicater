
# General attributes functions -------------------------------------------------

#' Attributes - Test if object allows attributes
#'
#' @description
#' In R, some objects do not allow attributes to be set on them. This function
#' checks if `attr(x, which) <- value` runs without errors. See [attr()]
#' for details.
#'
#' Up to R version 4.6.0, only three object types ([typeof()]) disallow
#' attributes:
#' - `"primitive"` starting in R 4.6.0.
#' - `"symbol"` starting in R 3.5.0.
#' - `"NULL"` starting in R 3.4.0.
#'
#' Besides these, objects of type `"char"` and `"any"`, which exist only in the
#' internals of R, always return `FALSE`.
#'
#' @param x \[`any`] Object to check.
#' @param cnd_match \[`character(1)`] String to match in the condition message.
#'   If no match, the condition is re-thrown. Defaults to allow any message.
#' @param warn \[`character(1)`] How to handle erros: `"f"` to return `FALSE`;
#'   `"t" for `TRUE`, `"tw"` to return true but re-throw the warning; and ``"e"`
#'   to throw the warning as an error.
#'
#' @returns \[`logical(1)`] Whether attributes can be set on `x`, or re-throws
#'   the catched condition.
#'
#' @examples
#' attrs_allow(1:10) #> TRUE
#' attrs_allow(NULL) #> FALSE
#'
#' @export
is_attrs_allowed <- function(x, cnd_match = NULL, warn = "tw") {
  # Checks:
  # - cnd_match should be no-NA string or NULL
  # - warn should be "f", "t", "tw", or "e"
  # TODO:
  #test_msg(checkmate::check_string, cnd_match, null.ok = TRUE)
  #test_msg(checkmate::check_choice, warn, c("f", "t", "tw", "e"))


  # Main:
  cnd_match <- cnd_match %||% "."

  if (typeof(x) %in% c("char", "any")) { # Running attr() on these might break R
    return(FALSE)
  }

  tryCatch(
    {
      attr(x, "__example_attr__") <- "example_value"
      TRUE
    },
    error = \(cnd) {
      if (grepl(cnd_match, cnd$message)) FALSE else stop(cnd)
    },
    warning = \(cnd) {
      if (warn == "f" && grepl(cnd_match, cnd$message)) {
        FALSE
      } else if (warn == "e") {
        stop(cnd)
      } else {
        if (warn == "tw") warning(cnd$message)
        TRUE
      }
    }
  )
}


#' Get attribute of object
#'
#' Identical to [attr()] but with `exact = TRUE` by default.
#'
#' @param x \[`any`] Object to get attribute from.
#' @param which \[`character(1)`] Name of attribute to get.
#' @param exact \[`logical(1)`] Whether to match attribute name exactly, or allow
#'   partial matching.
#'
#' @returns \[`any`] The value of the attribute matched, or NULL if no match
#'   was found.
#'
#' @examples
#' x <- structure(1, long_name = 2)
#'
#' # Exact matches by default:
#' attr(x, "long") #> 2
#' attr2(x, "long") #> NULL
#'
#' @export
attr2 <- function(x, which, exact = TRUE) {
  # Checks: left to attr()
  attr(x, which, exact = exact)
}


#' Remove attributes from object
#'
#' Remove all attributes of `x`, while defining exceptions by name in `others`
#' or some common shorthands in `keep`.
#'
#' @param x \[`any`] Object to remove attributes from.
#' @param abbr \[`character(1)`] String of characters specifying attributes to
#'   keep: `n` for 'names', `d` for 'dim', `c` for 'class', `0` to keep all
#'   'dimnames', or `1`-`9` to keep only some dimension names.
#' @param exact \[`character()`] Other attributes to keep, specified by name.
#' @param match \[`character()`] Other attributes to keep, specified by regex
#'   pattern. They are combined with the or regex operator `|`.
#'
#' @returns \[`=x`] Same object as `x` but with attributes removed.
#'
#' @details
#' With `"0"` or `"1"` in `keep`, "row.names" are kept. Adding "dimnames" to
#' `others` does not impede the removal of certain dimension if `keep`
#' specified so.
#'
#' @examples
#' x <- structure(
#'   1:3, a = 1, b = 2, c = 3,
#'   names = c("X", "Y", "Z"), dim = c(1, 3), class = "myclass",
#'   levels = c("aa", "bb", "cc")
#' )
#'
#' attrs_rmv(x) # removes all attributes
#' attrs_rmv(x, abbr = "ncd") # keeps names, dim, and class
#' attrs_rmv(x, abbr = "c", exact = c("levels", "b")) # keeps class, levels, b
#' attrs_rmv(x, match = c("^[a-z]$")) # keeps a, b, c
#'
#' @export
attrs_rmv <- function(
  x, abbr = "", exact = character(), match = character()
) {
  # Checks:
  # - abbr should be a no-NA string with only "[ndc0-9]"
  # - exact and match should be a no-NA character()
  # - keep should be a no-NA flag
  # TODO:


  # Main:
  if (! attrs_allow(x, warn = "t")) {
    return(x)
  }

  attrs <- attributes(x)
  abbr <- strsplit(abbr, "")[[1]]
  keep_dims <- as.numeric(grep("^[0-9]+$", abbr, value = TRUE))

  keep_attrs <- unique(c(
    if ("n" %in% abbr) "names",
    if ("d" %in% abbr) "dim",
    if ("c" %in% abbr) "class",
    exact,
    if (length(match) > 0) grep(paste0(match, collapse = "|"), names(attrs), value = TRUE)
  ))

  if (length(keep_dims) > 0) {
    keep_attrs <- c(
      keep_attrs,
      "dimnames",
      if (any(c(0, 1) %in% keep_dims)) "row.names"
    )

    if (! 0 %in% keep_dims) {
      for (i in seq_along(attr2(x, "dimnames"))) {
        if (! i %in% keep_dims) {
          attr(x, "dimnames")[[i]] <- NULL
        }
      }
    }
  }

  for (name in setdiff(names(attrs), keep_attrs)) {
    attr(x, name) <- NULL # WARN: Might fail for special attributes
  }

  x
}
# NOTE: setting via attributes(x) fails for ilegal combinations (e.g. dimnames
# without dim), but consider using it for better performance
# TODO: consider adding value-based filtering, use fns_combine
# TODO: implement keep == TRUE or FALSE



# Names ------------------------------------------------------------------------

#' Attributes - Names
#'
#' A more flexible version of [rlang::is_named()] that allows the user to
#' specify how to handle edge cases in names.
#'
#' @param x \[`any`] Object to check for names.
#' @param how \[`character(1)`] How to extract names: "names" for `names(x)`, or
#'   "attr" for `attr(x, "names")`.
#' @param na,empty,dups,invalid \[`character(1)`] How to handle NA, empty,
#'   duplicate, or invalid names: `"f"` to return `FALSE`, or `"t"` return
#'   `TRUE`. `na` can also be `"na"` to return `NA`. A `"w"` suffix can be added
#'   (e.g. `"tw"`) to also issue a warning.
#' @param non_collection,zero_len \[`character(1)`] How to handle non-vector or
#'   zero-length inputs: `"f"` to return `FALSE`, or `"t"` return `TRUE`. A `"w"`
#'   suffix can be added (e.g. `"tw"`) to also issue a warning.
#'
#' @returns \[`logical(1)`] Whether `x` has names according to the specified
#'   checks.
#'
#' @details
#' The default argument values guaratee that `x` can be safely iterated by its
#' names.
#'
#' A `non_collection` is an object that is not an atomic vector, list, pairlist,
#' expression, or environment. I.e. returns `FALSE` for [is_collection()].
#'
#' The `how` methods differ only for environments, with "names" returning names
#' and "attr" returning NULL.
#'
#' `invalid = "f"` checks if the names don't change after being passed to
#' `make.names()`.
#'
#' Setting `na = "na"` could make sense if any of `empty`, `dups`, or `invalid`
#' is set to `"f"`, in which the true value of the NA name is relevant.
#'
#' @examples
#' has_names(mtcars) #> TRUE
#'
#' has_names(setNames(1:3, c("a", "", "b")), empty = "t") #> TRUE
#' has_names(setNames(1:3, c("a", "b", NA)), na = "t") #> TRUE
#' has_names(setNames(1:3, c("a", "a", "a"))) #> FALSE
#' has_names(setNames(1:3, c("_bad", "b", "c")), invalid = "f") #> FALSE
#'
#' @export
has_names <- function(
  x,
  na = "f", empty = "f", dups = "f", invalid = "t",
  zero_len = "f", non_collection = "f",
  how = "names"
) {
  # Main:
  if (! attrs_allow(x, warn = "t")) {
    return(FALSE)
  }
  if (!is_collection(x)) {
    return(resolve_category(non_collection, "{.arg x} is not a collection."))
  }
  if (length(x) == 0) {
    return(resolve_category(zero_len, "{.arg x} has zero length."))
  }

  nms <- switch(how,
    names = names(x),
    attr = attr(x, "names", TRUE)
  )

  if (is_null(nms)) {
    FALSE
  } else if (any(is.na(nms))) {
    resolve_category(na, "{.arg x} has {.val NA} in its names.")
  } else if (any(nms == "")) {
    resolve_category(empty, "{.arg x} has {.val ''} in its names.")
  } else if (anyDuplicated(nms)) {
    resolve_category(dups, "{.arg x} has duplicate names.")
  } else if (any(make.names(nms) != nms)) {
    resolve_category(invalid, "{.arg x} has invalid names.")
  } else {
    TRUE
  }
}
# NOTE: language and promise accept names<-; ... and environment have names();
# S4 accepts attr(x, "names")<- but object doesnt
# CHECK: test which objects accept names<-/attr(x, "names")<-
# If has warnings, must not short circuit. Then, it look more like a test_ or assert_


are_names_valid <- function(
  x,
  na = "f", empty = "f", dups = "f", invalid = "t",
  zero_len = "f", non_collection = "f",
  no_names = "f",
  how = "names"
) {
  nms <- switch(how,
    names = names(x),
    attr = attr(x, "names", TRUE)
  )

  if (is_null(nms)) {
    switch(no_names,
      t = return(TRUE),
      f = return(FALSE),
      stop = cli_abort("{.arg x} has no names.")
    )
  }

  trues <- rep(TRUE, length(nms))
  mask_na <- switch(na, f = is.na(nms),  t = trues)
  mask_empty <- switch(empty, f = nms == "", t = trues)
  mask_dups <- switch(dups,
    f = duplicated(nms) | duplicated(nms, fromLast = TRUE),
    t = trues
  )
  mask_invalid <- switch(invalid, f = make.names(nms) != nms, t = trues)

  ! (mask_na | mask_empty | mask_dups | mask_invalid)
}



# Dimensions -------------------------------------------------------------------


#' Attributes - Dimensions
#'
#' @description
#' Functions for the presence and number of dimensions and dimensions names of
#' an object.
#' - `n_dims()` and `n_dimnames()` return the number of dimensions and dimension
#'   names, respectively, controlling for empty dimensions and invalid names.
#' - `has_dim()`, `has_dimnames()`, and `has_rownames()` return whether the
#'   object has dimensions, dimension names, and rownames, respectively.
#'
#' @param x \[`any`] Object to check.
#' @param count_empty \[`logical(1)`] Whether to count empty dimensions and
#'   dimension names.
#' @param count_invalid \[`logical(1)`] Whether to dimension names that have only
#'   non-NA or non-empty values.
#' @param how \[`character(1)`] How to extract the attribute:
#' - For dimensions: `"dim"` for [dim()] and `"attr"` for [attr()].
#' - For dimension names: `"dimnames"` for [dimnames()] and `"attr"` for
#'   [attr()].
#' - For rownames: `"rownames"` for [rownames()], `"dimnames"` for the first
#'   element of [dimnames()], and `"attr"` for [attr()].
#'
#' @returns
#' - \[integer(1)] For `n_dims()` and `n_dimnames()`.
#' - \[`logical(1)`] For `has_dim()`, `has_dimnames()`, and `has_rownames()`.
#'
#' @name attributes-dimensions
NULL


#' @rdname attributes-dimensions
#' @export
n_dims <- function(x, count_empty = TRUE, how = "dim") {
  dim <- switch(how, dim = dim(x), attr = attr(x, "dim", TRUE))

  if (is_null(dim)) {
    0L
  } else if (count_empty) {
    length(dim)
  } else {
    sum(dim > 0)
  }
}


#' @rdname attributes-dimensions
#' @export
n_dimnames <- function(
  x, count_empty = TRUE, count_invalid = TRUE, how = "dimnames"
) {
  dimnames <- switch(how, dimnames = dimnames(x), attr = attr(x, "dimnames", TRUE))

  if (is_null(dimnames)) {
    return(0L)
  }

  n <- length(dimnames)
  if (! count_empty) {
    n <- n - sum(vapply(dimnames, length, integer(1)) == 0)
  }
  if (! count_invalid) {
    n <- n - sum(vapply(dimnames, \(x) all(is.na(x) | x == ""), logical(1)))
  }

  n
}


#' @rdname attributes-dimensions
#' @export
has_dimnames <- function(x, how = "dimnames") {
  dimnames <- switch(how,
    dimnames = dimnames(x),
    attr = attr(x, "dimnames", TRUE)
  )

  is_null(dimnames)
}
# TODO: consider adding n = NULL check? or too similar with n_dimnames()?


#' @rdname attributes-dimensions
#' @export
has_rownames <- function(x, how = "dimnames") {
  dimnames <- switch(how,
    rownames = rownames(x),
    dimnames = dimnames(x)[[1L]],
    attr = attr(x, "row.names", TRUE)
  )

  is_null(dimnames)
}


#' @rdname attributes-dimensions
#' @export
has_dim <- function(x, how = "dim") {
  dim <- switch(how,
    dim = dim(x),
    attr = attr(x, "dim", TRUE)
  )

  is_null(dim)
}
# TODO: consider adding n = NULL check? or too similar with n_dims()?
