
#' Tests - Integers
#'
#' Test if an object is an integer vector. The `mode` arguments allow to
#' consider only objects of [typeof()] `"integer"` (`mode = "strict"`), or also
#' double objects that have integer values, by passing `mode` and `mode_args` to
#' [is_integer_like()] (see its help page for more details).
#'
#' @param x \[`any`] An  object to test.
#' @param mode,mode_tol \[`"strict"` | `"range"` | `"range_tol"` | `"trunc"` |
#'   `"trunc_tol"`, `double(1)`] The `mode` and `tol` arguments to pass to
#'   [is_integer_like()].
#' @param len,na_n,dup_n,nan_n,inf_n \[`integer(1)` | `integer(2)` | `\(){}` |
#'   `NULL`] Possible values for the length, data values, number of `NA` values,
#'   and number of duplicates. `nan_n` and `inf_n` are only relevant if `mode`
#'   is `"trunc"` or `"trunc_tol"`. The options are:
#'   - `NULL` to not test.
#'   - A single non-negative number to test for `. == range`.
#'   - A single negative number to test for `. == length(x) + range`.
#'   - A vector of two non-negative numbers to test for `range[1] <= . <=
#'     range[2]`. `Inf` is allowed.
#'   - A function that recieves the value to test (e.g. the number of NAs) and
#'     the length of `x`, and returns a single `TRUE` or `FALSE`.
#' @param range \[`integer(2)` | `NULL`] A vector with the upper and lower bound
#'   for `x` values. `Inf` is allowed. Set to `NULL` to not test.
#' @param set \[`integer()` | `list(yes = , no = )` | `NULL`] Test if all values
#'   of `x` are in a set of allowed values. Use a list with `yes` and `no`
#'   `integer()` elements to defined allowed and disallowed values. Set to
#'   `NULL` to not test.
#' @param sentinels \[`character()`] A named character that allows `x` to be
#'   some scalar sentinel instead. By default, none are allowed. Add each named
#'   entry to change the behaviour:
#'   - `null` (`NULL`): `"f"` to disallow, `"t"` to allow.
#'   - `na` (`NA`): `"f"` to disallow, `"t"` to allow `NA_integer_`, `"any"` to
#'     allow any NA value.
#'   - `nan` (`NaN`): `"f"` to disallow, `"t"` to allow.
#'   - `inf` (`Inf`): `"f"` to disallow, `"+"` to allow `+Inf`, `"-"` to allow
#'     `-Inf`, `"+-"` to allow both.
#'   - `t` (`TRUE`): `"f"` to disallow, `"t"` to allow.
#'   - `f` (`FALSE`): `"f"` to disallow, `"t"` to allow.
#' @param sorted \[`"asc"` | `"desc"` | `NULL`] Test if `x` is sorted in
#'   ascending (`"asc"`) or descending (`"desc"`) order. Set to `NULL` to not
#'   test. Pair with `dup_n` to test for strictly sorted values.
#' @param custom \[`\(){}` | `NULL`] A custom function that takes `x` as first
#'   argument and returns a single `TRUE` or `FALSE`. Set to `NULL` to not test.
#'
#' @returns \[`logical(1)`, `=x` | `character()`]
#'
#' @name tests-integer
NULL

core_integer <- function(
  x, mode = "strict",
  len = NULL, na_n = NULL, dup_n = NULL, nan_n = NULL, inf_n = NULL,
  range = NULL, set = NULL,
  sentinels = c(null = "f", na = "f", nan = "f", inf = "f", t = "f", f = "f"),
  sorted = NULL, custom = NULL, mode_tol = sqrt(.Machine$double.eps),
  env = caller_env()
) {
  # Checks:
  # TODO:


  # Main:
  l <- length(x)

  if (test_sentinels(x, sentinels)) {
    return(list(sentinels = TRUE))
  }

  tests <- list()

  tests$type <- if (mode == "strict") {
    is_integer(x)
  } else {
    is_integer_like(x, mode = mode, tol = mode_tol)
  }
  tests$type <- tests$type %@@% list(mode = mode, type = typeof(x))
  tests$len <- test_in_range(l, len, l) %@@% c(n = l)
  tests$range <- test_in_range(x, range, l) %@@% c(range = range)
  tests$na <- test_in_range(n_na <- sum(are_na2(x, nan = "f")), na_n, l) %@@% c(n = n_na)
  tests$dups <- test_in_range(n_dups <- sum(duplicated(x)), dup_n, l) %@@% c(n = n_dups) # TODO: incomparables = NA_integer_?
  tests$sorted <- test_sorted(x, sorted) %@@% c(sorted = sorted)
  tests$set <- test_in_set(x, set)
  tests$custom <- if (!is_null(custom)) custom(x) # TODO: try catch also if not T/F
  if (mode %in% c("trunc", "trunc_tol")) {
    tests$nan <- test_in_range(n_nan <- sum(are_nan(x, na = "f")), nan_n, l) %@@% c(n = n_nan)
    tests$inf <- test_in_range(n_inf <- sum(are_inf(x, na = "f")), inf_n, l) %@@% c(n = n_inf)
  }

  tests
}


#' @rdname tests-integer
#' @export
test_integer <- fn_core_to_test(core_integer)

#' @rdname tests-integer
#' @export
assert_integer <- fn_core_to_assert(core_integer)

MSGS$core_integer <- list(
  type = \(attrs) {
    fn <- switch(attrs$mode, strict = "is_integer", "is_integer_like")
    glue("Had type {{.val {attrs$type}}} and did not pass {{.fn {fn}}}.")
  },
  len = \(attrs) glue("Had length {{.val {{{attrs$n}}}}}."),
  range = \(attrs) glue("Had values outside of {{.val {{{attrs$range}}}}}."),
  na = \(attrs) glue("Had {{.val {{{attrs$n}}}}} NA values."),
  dups = \(attrs) glue("Had {{.val {{{attrs$n}}}}} duplicated values."),
  sorted = \(attrs) {
    order <- switch(attrs$sorted, asc = "ascending", desc = "descending")
    glue("Was not sorted in {order} order.")
  },
  set = \(attrs) glue("Had values outside of the allowed set."),
  nan = \(attrs) glue("Had {{.val {{{attrs$n}}}}} NaN values."),
  inf = \(attrs) glue("Had {{.val {{{attrs$n}}}}} Inf values."),
  custom = \(attrs) glue("Did not pass the custom test.")
)



# Helpers ----------------------------------------------------------------------

test_sorted <- function(x, sorted) {
  if (is_null(sorted)) {
    return(NULL)
  }

  if (sorted == "asc") {
    !is.unsorted(x)
  } else if (sorted == "desc") {
    !is.unsorted(rev(x))
  }
}
# TODO: what to do with na.rm = TRUE?


test_sentinels <- function(x, sentinels) {
  # Checks:
  # TODO: (maybe after checking we change the null <- scheme)


  # Main:
  if (!is_na(null <- sentinels["null"]) || null != "f") {
    if (is_null(x)) return(TRUE)
  }

  if (!is_na(na <- sentinels["na"]) || na != "f") {
    if (na == "any" && is_na2(x, 1)) return(TRUE)
    if (na == "t" && is_na2(x, 1, type = "integer")) return(TRUE)
  }

  if (!is_na(nan <- sentinels["nan"]) || nan != "f") {
    if (is_nan(x, 1, na = "f")) return(TRUE)
  }

  if (!is_na(inf <- sentinels["inf"]) || inf != "f") {
    if (is_inf(x, 1, na = "f", signs = inf)) return(TRUE)
  }

  if (!is_na(t <- sentinels["t"]) || t != "f") {
    if (is_true(x)) return(TRUE)
  }

  if (!is_na(f <- sentinels["f"]) || f != "f") {
    if (is_false(x)) return(TRUE)
  }

  FALSE
}


test_in_set <- function(x, set, type_test = "integer") {
  type_tester <- switch(type_test,
    integer = is_integer,
    numeric = is_numeric,
    # TODO: internal error
  )

  if (is_null(set)) {
    return(NULL)
  }

  if (type_tester(set)) {
    all(x %in% set)
  } else if (is_list(set)) {
    all(x %in% set$yes && ! x %in% set$no)
  }
}
# TODO: range and etc can be double, for to say len <= 1.1 (len < 1)


test_in_range <- function(n, range, l, type_test = "integer") {
  type_tester <- switch(type_test,
    integer = \(x, n) is_integer_like(x, n, mode = "trunc"),
    numeric = is_numeric,
    # TODO: internal error
  )

  if (is_null(range)) {
    return(NULL)
  }

  if (type_tester(range, 1)) {
    all(n == range)
  } else if (type_tester(range, 2)) {
    all(n >= range[1] & n <= range[2])
  } else if (is_function(range)) {
    range(n, l) # TODO: try catch also if not T/F
  } else {
    # TODO: err
  }
}
