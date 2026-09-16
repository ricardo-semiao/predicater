
# General attributes functions -------------------------------------------------

#' Attributes - Check if object allows attributes
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
#' @param x `r ROXY$x()`
#' @param cnd_match \[`character(1)`] String to match in the condition message.
#'   If no match, the condition is re-thrown. Defaults to allow any message.
#' @param warn \[`"true"` | `"false"` | `"warn"` | `"error"`]
#'   How to handle warnings: `"false"` to return `FALSE`; `"true"`` for `TRUE`;
#'   `"warn"` to return `TRUE` but re-throw the warning; and `"error"` to throw
#'   the warning as an error.
#'
#' @returns `r ROXY$test_res()` Or, re-throws the catched condition.
#'
#' @examples
#' is_attrs_allowed(mtcars) #> TRUE
#' is_attrs_allowed(NULL) #> FALSE
#'
#' @export
is_attrs_allowed <- function(x, cnd_match = NULL, warn = "tw") {
  # Checks:
  # - cnd_match should be no-NA string or NULL
  # - warn should be "f", "t", "tw", or "e"
  

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
      if (warn %in% c("f", "false") && grepl(cnd_match, cnd$message)) {
        FALSE
      } else if (warn == "error") {
        stop(cnd)
      } else {
        if (warn == "warn") warning(cnd$message)
        TRUE
      }
    }
  )
}


#' Attributes - Get attribute of object
#'
#' Identical to [attr()] but with `exact = TRUE` by default.
#'
#' @param x \[`any`] Object to get attribute from.
#' @param which \[`character(1)`] Name of attribute to get.
#' @param exact \[`TRUE` | `FALSE`] Whether to match attribute name exactly, or
#'   allow partial matching.
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



# Filter -----------------------------------------------------------------------

#' Attributes - Check for and filter attributes from object
#'
#' @description
#' Functions to test for and filter attributes from an object. Attributes can be
#' selected by abbreviation in `abbr`, by name in `exact` or `disallow`, or by
#' regex pattern in `match`.
#'
#' To remove attributes:
#' - `attrs_rmv()` returns `x` without the attributes selected.
#' - `attrs_keep()` returns `x` with only the attributes selected.
#'
#' To test for attributes:
#' - `has_attrs_any()` returns `TRUE` if any of the selected attributes are
#'   present.
#' - `has_attrs_all()` returns `TRUE` if all of the selected attributes are
#'   present.
#' - `has_attrs_only()` returns `TRUE` if all and only the selected attributes
#'   are present.
#'
#' @param x \[`any`] Object to test for or remove attributes from.
#' @param abbr \[`character(1)`] String of characters specifying attributes to
#'   keep: `"n"` for 'names'; `"d"` for 'dim'; `"c"` for 'class'; `"r"` for both
#'   rownames and dimnames.
#' @param exact \[`character()`] Other attributes to keep, specified by name.
#' @param match \[`character()`] Other attributes to keep, specified by regex
#'   pattern. They are combined with the or regex 'or' operator `|`.
#' @param perl \[`TRUE` | `FALSE`] Whether to use perl-compatible regex.
#' @param disallow \[`character()`] Attributes that, if present, will fail the
#'   test.
#'
#' @returns
#' - \[`=x`] For `attrs_rmv()` and `attrs_keep()`: Same object as `x` but with
#'   attributes some removed.
#' - \[`TRUE` | `FALSE`] For `has_attrs_any()`, `has_attrs_all()`, and
#'   `has_attrs_only()`: the scalar result of the test.
#'
#' @examples
#' x <- structure(
#'   1:3, a = 1, b = 2, c = 3,
#'   names = c("X", "Y", "Z"), dim = c(1, 3), class = "myclass",
#'   levels = c("aa", "bb", "cc")
#' )
#'
#' # Default removal:
#' attrs_keep(x) # Removes all attributes
#' attrs_rmv(x) # Removes no attributes
#'
#' # Abbreviation, exatc, and regex filtering:
#' attrs_keep(x, abbr = "ncd") # Keeps names, dim, and class
#' attrs_rmv(x, exact = c("levels", "b")) # Removes levels and b
#' attrs_keep(x, match = "^[a-c]$") # Keeps only a, b, and c
#'
#' # Testing for attributes:
#' has_attrs_any(x, "c", exact = "levels") #> TRUE (has class and levels)
#' has_attrs_any(x, "c", exact = "levels", disallow = "dim") #> FALSE (has dim)
#
#' has_attrs_all(x, exact = c("a", "b", "c")) #> TRUE
#' has_attrs_all(x, exact = c("a", "b", "c", "d")) #> FALSE (missing d)
#'
#' has_attrs_only(x, exact = c("a", "b", "c")) #> FALSE (has other attributes)
#'
#' @name attributes-filter
NULL


#' @rdname attributes-filter
#' @export
attrs_rmv <- function(
  x, abbr = "", exact = character(), match = character(), perl = FALSE
) {
  attrs_filt <- attrs_filter(
    x, abbr = abbr, exact = exact, match = match, keep = FALSE, perl = perl
  )

  tryCatch(
    attributes(x) <- attrs_filt,
    error = \(cnd) {
      names_rmv <- setdiff(names(attributes(x)), names(attrs_filt))
      for (attr_name in names_rmv) {
        attr(x, attr_name) <- NULL
      }
    }
  ) # WARN: both might fail for special attributes
  x
}


#' @rdname attributes-filter
#' @export
attrs_keep <- function(
  x, abbr = "", exact = character(), match = character(), perl = FALSE
) {
  attrs_filt <- attrs_filter(
    x, abbr = abbr, exact = exact, match = match, keep = TRUE, perl = perl
  )

  tryCatch(
    attributes(x) <- attrs_filt,
    error = \(cnd) {
      names_rmv <- setdiff(names(attributes(x)), names(attrs_filt))
      for (attr_name in names_rmv) {
        attr(x, attr_name) <- NULL
      }
    }
  ) # WARN: both might fail for special attributes
  x
}


#' @rdname attributes-filter
#' @export
has_attrs_any <- function(
  x, abbr = "", exact = character(), match = character(), disallow = character()
) {
  # Main:
  if (any(names(attributes(x)) %in% disallow)) {
    return(FALSE)
  }

  attrs_filt <- attrs_filter(
    x, abbr = abbr, exact = exact, match = match, keep = TRUE
  )

  length(attrs_filt) > 0
}


#' @rdname attributes-filter
#' @export
has_attrs_all <- function(
  x, abbr = "", exact = character(), disallow = character()
) {
  # Main:
  attrs_x <- names(attributes(x))
  if (any(attrs_x %in% disallow)) {
    return(FALSE)
  }

  attrs_goal <- c(
    if ("n" %in% abbr) "names",
    if ("d" %in% abbr) "dim",
    if ("c" %in% abbr) "class",
    if ("r" %in% abbr) c("dimnames", "row.names"),
    exact
  )

  all(attrs_goal %in% attrs_x)
}


#' @rdname attributes-filter
#' @export
has_attrs_only <- function(x, abbr = "", exact = character()) {
  # Main:
  attrs_x <- names(attributes(x))

  attrs_goal <- c(
    if ("n" %in% abbr) "names",
    if ("d" %in% abbr) "dim",
    if ("c" %in% abbr) "class",
    if ("r" %in% abbr) c("dimnames", "row.names"),
    exact
  )

  all(attrs_x %in% attrs_goal) && all(attrs_goal %in% attrs_x)
}


#' @noRd
attrs_filter <- function(
  x, abbr = "", exact = character(), match = character(), keep = TRUE, perl = FALSE
) {
  # Checks:
  # - abbr should be a no-NA string with only "[ndc0-9]"
  # - exact and match should be a no-NA character()
  # - keep should be a no-NA flag
  

  # Main:
  if (! is_attrs_allowed(x, warn = "t")) {
    return(NULL)
  }

  if (is_null(attrs <- attributes(x))) {
    return(NULL)
  }

  abbr <- strsplit(abbr, "")[[1]]

  matched_attrs <- unique(c(
    if ("n" %in% abbr) "names",
    if ("d" %in% abbr) "dim",
    if ("c" %in% abbr) "class",
    if ("r" %in% abbr) c("dimnames", "row.names"),
    exact,
    if (length(match) > 0) {
      grep(collapse_patterns(match), names(attrs), value = TRUE, perl = perl)
    }
  ))

  diff <- setdiff(names(attrs), matched_attrs)
  intersect <- intersect(names(attrs), matched_attrs)

  attrs[if (keep) intersect else diff]
}
# TODO: consider adding value-based filtering, use fns_combine



# Names ------------------------------------------------------------------------

#' Attributes - Names
#'
#' Test if `x` has names with more flexiblillity than [rlang::is_named()],
#' handling NA, empty, duplicate, and invalid names, as well as empty vectors.
#'
#' @param x \[`collection()`, `any`] For `are_*()`, any collection to test; for
#'   `is_*()`, any object to test.
#' @param how \[`"names"` | `"x"` | `"attr"` | `"names2"`] How to extract names:
#'   "names" for [names()]; `"x"` to use `x` itself; "attr" for `attr(x,
#'   "names")`; or `"names2"` for [rlang::names2()].
#' @param na,empty,dups,invalid \[`TRUE` | `FALSE`] What to return for NA,
#'   empty, duplicate, or invalid names.
#' @param zero_len \[`TRUE` | `FALSE`] What to return for an an empty `x`.
#'
#' @returns `r ROXY$test_res()`
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
#' `invalid = FALSE` checks if the names don't change after being passed to
#' `make.names()`.
#'
#' @examples
#' has_names_valid(mtcars) #> TRUE
#'
#' # NA, empty, and duplicate names return FALSE by default:
#' has_names_valid(rlang::set_names(1:3, c("a", "b", NA))) #> FALSE
#' has_names_valid(rlang::set_names(1:3, c("a", "b", NA)), na = TRUE) #> TRUE
#'
#' has_names_valid(rlang::set_names(1:3, c("a", "", "b"))) #> FALSE
#' has_names_valid(rlang::set_names(1:3, c("a", "a", "a"))) #> FALSE
#'
#' # Invalid names and zero-length x's return TRUE by default:
#' has_names_valid(rlang::set_names(1:3, c("_bad", "b", "c"))) #> TRUE
#' has_names_valid(integer(0)) #> TRUE
#'
#' # Use `how` to control how names are extracted:
#' has_names_valid(rlang::base_env(), how = "attr") #> FALSE
#' # (envs have no names attribute)
#' has_names_valid(c("a", "b", "c"), how = "x") #> TRUE (x itself was tested)
#'
#' # Vectorized test:
#' are_names_valid(c("a", "b", "b", NA, ""), how = "x")
#' #> c(TRUE, FALSE, FALSE, FALSE, FALSE)
#'
#' @export
has_names_valid <- function(
  x, na = FALSE, empty = FALSE, dups = FALSE, invalid = TRUE, zero_len = TRUE,
  how = "names"
) {
  # Main:
  if (! is_collection(x)) {
    return(FALSE)
  }

  if (length(x) == 0 && zero_len) {
    return(TRUE)
  }

  nms <- switch(how,
    names = names(x),
    x = {
      if (! is_character(x)) {
        cli_abort("{.arg x} must be a character vector when {.arg how} is 'x'.")
      }
      x
    },
    attr = attr(x, "names", TRUE),
    names2 = names2(x)
  )

  if (is_null(nms)) {
    return(FALSE)
  }

  all(are_names_valid(
    nms, na = na, empty = empty, dups = dups, invalid = invalid, how = "x"
  ))
}
# NOTE: language and promise accept names<-; ... and environment have names();
# S4 accepts attr(x, "names")<- but object doesnt
# NOTE: we could fix na and empty to "f" to simplify the API

#' @rdname has_names_valid
#' @export
are_names_valid <- function(
  x, na = FALSE, empty = FALSE, dups = FALSE, invalid = TRUE,
  how = "names"
) {
  nms <- switch(how,
    names = names(x),
    x = {
      if (! is_character(x)) {
        cli_abort("{.arg x} must be a character vector when {.arg how} is 'x'.")
      }
      x
    },
    attr = attr(x, "names", TRUE),
    names2 = names2(x)
  )

  if (is_null(nms)) { # Could also run !is_collection to be sure
    cli_abort("{.arg x} has no names.")
  }

  res <- rep_len(TRUE, length(nms))

  if (!na) res <- res & !is.na(nms)
  if (!empty) res[!is.na(nms) & nms == ""] <- FALSE
  if (!dups) res <- res & !are_duplicated(x)
  if (!invalid) res[!is.na(nms) & make.names(nms) != nms] <- FALSE

  res
}
# CHECK: add NA option to na?
# Setting `na = NA` could make sense if any of `empty`, `dups`, or `invalid`
# is set to F, in which the true value of the NA name is relevant.



# Dimensions -------------------------------------------------------------------

#' Attributes - Dimensions existence and sizes
#'
#' @description
#' Functions for the presence and number of dimensions and dimensions names of
#' an object.
#' - `n_dims()` and `n_dimnames()` return the number of dimensions and dimension
#'   names, respectively, controlling for empty dimensions and invalid names.
#' - `has_dim()`, `has_dimnames()`, and `has_rownames()` return whether the
#'   object has dimensions, dimension names, and rownames, respectively.
#'
#' @param x `r ROXY$x()`
#' @param empty \[`TRUE` | `FALSE`] Whether to count empty dimensions and
#'   dimension names.
#' @param invalid \[`TRUE` | `FALSE`] Whether to count dimensions with NA,
#'   empty, or duplicate names.
#' @param how \[`character(1)`]
#'   How to extract the attribute:
#'   - For dimensions: `"dim"` for [dim()] and `"attr"` for `attr(x, "dim")`.
#'   - For dimension names: `"dimnames"` for [dimnames()] and `"attr"` for
#'     `attr(x, "dimnames")`.
#'   - For rownames: `"rownames"` for [rownames()], `"dimnames"` for the first
#'     element of [dimnames()], and `"attr"` for `attr(x, "row.names")`.
#' @param n \[`integer(1)`] Number of dimensions to check for. Set to `NULL` to
#'   not test.
#'
#' @returns
#' - \[integer(1)] For `n_dims()` and `n_dimnames()`.
#' - \[`TRUE` | `FALSE`] For `has_dim()`, `has_dimnames()`, and
#'   `has_rownames()`: the sacalar result of the test.
#'
#' @examples
#' x <- structure(
#'   1:6, dim = c(2, 3, 1),
#'   dimnames = list(c("a", "a"), c("x", "y", "z"), NULL)
#' )
#'
#' # Counting dimensions and dimension names:
#' n_dims(x) #> 3
#' n_dims(x, empty = FALSE) #> 2
#'
#' n_dimnames(x) #> 2
#' n_dimnames(x, invalid = FALSE) #> 1 (rownames have duplicates)
#'
#' # Testing for dimensions and dimension names:
#' has_dimnames(x) #> TRUE
#' has_rownames(mtcars) #> TRUE
#' has_dim(mtcars) #> TRUE
#'
#' # Data frames often have no actual dimension attribues:
#' has_dim(mtcars, how = "attr") #> FALSE
#' has_dimnames(mtcars, how = "attr") #> FALSE
#' has_rownames(mtcars, how = "row.names") #> TRUE (but have row.names one)
#'
#' @name attributes-dimensions
NULL


#' @rdname attributes-dimensions
#' @export
n_dims <- function(x, empty = TRUE, how = "dim") {
  dim <- switch(how, dim = dim(x), attr = attr(x, "dim", TRUE))

  if (is_null(dim)) {
    0L
  } else if (empty) {
    length(dim)
  } else {
    sum(dim > 1)
  }
}


#' @rdname attributes-dimensions
#' @export
n_dimnames <- function(
  x, invalid = TRUE, how = "dimnames"
) {
  dimnames <- switch(how, dimnames = dimnames(x), attr = attr(x, "dimnames", TRUE))

  if (is_null(dimnames)) {
    return(0L)
  }

  n <- length(dimnames)
  n <- n - sum(vapply(dimnames, length, integer(1)) == 0)
  if (! invalid) {
    n <- n - sum(vapply_lgl(dimnames, \(x) {
      length(x) > 0 && all(if_else2(is.na(x), TRUE, x == "" | are_duplicated(x)))
    }))
  }

  n
}


#' @rdname attributes-dimensions
#' @export
has_dimnames <- function(x, n = NULL, how = "dimnames") {
  dimnames <- switch(how,
    dimnames = dimnames(x),
    attr = attr(x, "dimnames", TRUE)
  )

  is_list(dimnames, n = n)
}


#' @rdname attributes-dimensions
#' @export
has_rownames <- function(x, how = "dimnames") {
  rownames <- switch(how,
    dimnames = dimnames(x)[[1L]],
    attr = attr(x, "dimnames", TRUE)[[1L]],
    row.names = attr(x, "row.names", TRUE)
  )

  ! is_null(rownames)
}


#' @rdname attributes-dimensions
#' @export
has_dim <- function(x, n = NULL, how = "dim") {
  dim <- switch(how,
    dim = dim(x),
    attr = attr(x, "dim", TRUE)
  )

  is_numeric(dim, n = n)
}



# Attribute setters and getters ------------------------------------------------

#' Attributes - Getters and setters
#'
#' Functions to get and set names, dimnames, and class of an object, while
#' requiring a specific 'quality' of the values.
#'
#' @param x \[`any`] Object to get or set attribute from.
#' @param value \[`any`] Value to set the attribute to.
#' @inheritParams vctrs::vec_as_names
#' @param how \[`character(1)`]
#'   How to get or set the attribute:
#'   - For `names3()`: `"names"` for [names()] and `"attr"` for [attr()].
#'   - For `dimnames2()`: `"dimnames"` for [dimnames()] and `"attr"` for `attr(x,
#'     "dimnames")`.
#'   - For `rownames2()`: `"dimnames"` for the first element of [dimnames()],
#'     `"attr"` for `attr(x, "dimnames")[[1]]`, and `"row.names"` for `attr(x,
#'     "row.names")`.
#'   - For `class2()`: `"class"` for [class()] and `"attr"` for `attr(x,
#'     "class")`.
#'
#' @returns
#' - \[`character(length(x))` | `NULL`] For `names3()`.
#' - \[`list(length(dim(x)))` | `NULL`] For `dimnames2()`.
#' - \[`character(dim(x)[1])` | `NULL`] For `rownames2()`.
#' - \[`character()`] For `class2()`.
#' - \[`=value`] For the setters: The `value` passed, invisibly.
#'
#' @examples
#' x <- structure(
#'   1:6, dim = c(2, 3, 1), names = paste0("i", 1:6),
#'   dimnames = list(c("a", "a"), c("x", "y", "z"), "_bad"),
#'   class = c("classA", "", NA, "classB", "classA")
#' )
#'
#' # Getting repaired names and valid classes:
#' names3(x) #> c("i1", "i2", "i3", "i4", "i5", "i6") (no repair)
#'
#' suppressMessages(dimnames2(x))
#' #> list(c("a...1", "a...2"), c("x", "y", "z"), "_bad")
#' suppressMessages(dimnames2(x, repair = "universal"))
#' #> list(c("a...1", "a...2"), c("x", "y", "z"), "._bad")
#'
#' suppressMessages(rownames2(x)) #> c("a...1", "a...2")
#'
#' class2(x) #> c("classA", "classB")
#'
#' # Setting repaired names and valid classes:
#' class2(x) <- c("again", "duplicates", "again")
#' class2(x) #> c("again", "duplicates")
#'
#' suppressMessages(rownames2(x) <- NULL) # Names are created automatically
#' rownames(x) #> c("...1", "...2")
#'
#' @name attributes-getters-setters
NULL


#' @rdname attributes-getters-setters
#' @export
names3 <- function(x, repair = "unique", how = "names") {
  # Checks:
  # - x is a collection
  # - repair is left to vec_as_names()
  # - how is "names" or "attr"
  

  # Main:
  value <- switch(how,
    names = names(x),
    attr = attr(x, "names", TRUE)
  )

  if (is_null(value)) {
    if (repair == "minimal") {
      return(NULL)
    } else {
      value <- character(length(x))
    }
  }

  vctrs::vec_as_names(value, repair = repair)
}


#' @rdname attributes-getters-setters
#' @export
`names3<-` <- function(x, repair = "unique", how = "names", value) {
  # Checks:
  # - x is a collection
  # - value is character(length(x))
  # - repair is left to vec_as_names()
  # - how is "names" or "attr"
  

  # Main:
  if (! is_null(value)) {
    value <- vctrs::vec_as_names(value, repair = repair)
  }

  switch(how,
    names = names(x) <- value,
    attr = attr(x, "names") <- value
  )

  invisible(x)
}


#' @rdname attributes-getters-setters
#' @export
dimnames2 <- function(x, repair = "unique", how = "dimnames") {
  # Checks:
  # - x is a collection with dimensions
  # - repair is left to vec_as_names()
  # - how is "dimnames" or "attr"
  # TODO: 
  # consider dim via attr, e.g. excluding data frames


  # Main:
  value <- switch(how,
    dimnames = dimnames(x),
    attr = attr(x, "dimnames", TRUE)
  )

  if (is_null(value)) {
    if (repair == "minimal") {
      return(NULL)
    } else {
      value <- lapply(dim(x), \(n) character(n))
    }
  }

  lapply(value, \(v) vctrs::vec_as_names(v, repair = repair))
}


#' @rdname attributes-getters-setters
#' @export
`dimnames2<-` <- function(x, repair = "unique", how = "dimnames", value) {
  # Checks:
  # - x is a collection with dimensions
  # - value has lenght equal to length(dim(x)); each value[[i]] has length equal
  #   to dim(x)[i]
  # - repair is left to vec_as_names()
  # - how is "dimnames" or "attr"
  
  # Main:
  if (! is_null(value)) {
    value <- lapply(value, \(v) vctrs::vec_as_names(v, repair = repair))
  }

  switch(how,
    dimnames = dimnames(x) <- value,
    attr = attr(x, "dimnames") <- value
  )

  invisible(x)
}


#' @rdname attributes-getters-setters
#' @export
rownames2 <- function(x, repair = "unique", how = "dimnames") {
  # Checks:
  # - x is a collection with dimensions
  # - repair is left to vec_as_names()
  # - how is "dimnames", "attr" or "row.names"


  # Main:
  value <- switch(how,
    dimnames = dimnames(x)[[1L]],
    attr = attr(x, "dimnames", TRUE)[[1L]],
    row.names = attr(x, "row.names", TRUE)
  )

  if (is_null(value)) {
    if (repair == "minimal") {
      return(NULL)
    } else {
      value <- character(dim(x)[1L])
    }
  }

  vctrs::vec_as_names(value, repair = repair)
}


#' @rdname attributes-getters-setters
#' @export
`rownames2<-` <- function(x, repair = "unique", how = "dimnames", value) {
  # Checks:
  # - x is a collection with dimensions
  # - value has length equal to dim(x)[1]
  # - repair is left to vec_as_names()
  # - how is "dimnames" or "attr"


  # Main:
  dims_nms <- dimnames(x) %||% vector("list", length(dim(x)))

  if (length(value) == 0) {
    value <- character(dim(x)[1L])
  }
  dims_nms[[1L]] <- vctrs::vec_as_names(value, repair = repair)

  switch(how,
    dimnames = dimnames(x) <- dims_nms,
    attr = attr(x, "dimnames") <- dims_nms,
    row.names = attr(x, "row.names") <- dims_nms[[1L]]
  )

  invisible(x)
}


#' @rdname attributes-getters-setters
#' @export
class2 <- function(x, how = "class") {
  # Checks:
  # - how is "class" or "attr"


  # Main:
  cls <- switch(how,
    class = class(x),
    attr = attr(x, "class", TRUE)
  )

  if (is_null(cls)) {
    return(character(0)) # CHECK: reconsider returning NULL vs character(0)
  }

  cls[cls != "" & !duplicated(cls) & !is.na(cls)]
}


#' @rdname attributes-getters-setters
#' @export
`class2<-` <- function(x, how = "class", value) {
  # Checks:
  # - how is "class" or "attr"
  # - value must be character without NAs, "", and dups
  

  # Main:
  value <- value[value != "" & !duplicated(value) & !is.na(value)]
  switch(how,
    class = class(x) <- value,
    attr = attr(x, "class") <- value
  )

  invisible(x)
}
# CHECK: add message for cleaning classes as in vctrs::vec_as_names()?
