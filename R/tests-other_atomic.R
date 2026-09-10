
#' @include tests-helpers.R
NULL



# Raw ----------------------------------------------------------

#' Tests - Raw
#'
#' @description
#' Test if an object is a raw vector.
#'
#' `test_raw()` is the predicate test, while `assert_raw()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len `r ROXY$x_n("len")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#'
#' @returns `r ROXY$test_returns("raw")`
#'
#' @name tests-raw
NULL


core_raw <- function(
  x, len = NULL, sentinels = NULL, custom = NULL, env = caller_env()
) {
  # Checks:
  # TODO:

  # Main:
  tests <- initialize_tests("type", sentinels, len, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_raw(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  l <- length(x)

  tests$len <- test_in_range(l, len, l) %@@% c(n = l)
  tests$custom <- test_custom(x, custom, env)

  tests
}

#' @rdname tests-raw
#' @export
test_double <- fn_core_to_test(core_double)

#' @rdname tests-raw
#' @export
assert_double <- fn_core_to_assert(core_double, c(
  MSGS$sub_tests,
  type = \(attrs) glue("had type {{.val {attrs$type}}}, not {.val raw}.")
))



# Logical ----------------------------------------------------------------------

#' Tests - Logical
#'
#' @description
#' Test if an object is a logical vector.
#'
#' `test_logical()` is the predicate test, while `assert_logical()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len,na_n `r ROXY$x_n("len,na_n")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#'
#' @returns `r ROXY$test_returns("logical")`
#'
#' @name tests-logical
NULL

core_logical <- function(
  x, len = NULL, na_n = NULL, sentinels = NULL, custom = NULL, env = caller_env()
) {
  # Checks:
  # TODO:

  # Main:
  tests <- initialize_tests("type", sentinels, len, na_n, custom)

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_logical(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  l <- length(x)

  tests$len <- test_in_range(l, len, l) %@@%
    c(n = l)
  tests$na_n <- test_in_range(n_na <- sum(are_na2(x, nan = "f")), na_n, l) %@@%
    c(n = n_na)
  tests$custom <- test_custom(x, custom, env)

  tests
}
# TODO: add true_n


#' @rdname tests-logical
#' @export
test_double <- fn_core_to_test(core_double)

#' @rdname tests-logical
#' @export
assert_double <- fn_core_to_assert(core_double, c(
  MSGS$sub_tests,
  type = \(attrs) glue("had type {{.val {attrs$type}}}, not {.val logical}.")
))



# Character --------------------------------------------------------------------

#' Tests - Character
#'
#' @description
#' Test if an object is a character vector.
#'
#' `test_character()` is the predicate test, while `assert_character()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len,na_n,dup_n `r ROXY$x_n("len,na_n,dup_n")`
#' @param set `r ROXY$set("character")`
#' @param match \[`character()` | `list(yes = , no = )` | `NULL`] Test if all
#'   elements of `x` match regular expression patterns. Pass a character vector of
#'   patterns (implicitly OR'd together), or a list with `yes` and `no`
#'   character vectors of patterns. Set to `NULL` to not test.
#' @param perl \[`TRUE` | `FALSE`] Should Perl-compatible regexps be used in
#'   `match`?
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#'
#' @returns `r ROXY$test_returns("character")`
#'
#' @name tests-character
NULL


core_character <- function(
  x,
  len = NULL, na_n = NULL, dup_n = NULL,
  set = NULL, match = NULL,
  sentinels = NULL, custom = NULL, perl = FALSE,
  env = caller_env()
) {
  # Checks:
  # TODO:

  # Main:
  tests <- initialize_tests(
    "type", sentinels, len, na_n, dup_n, set, match, custom
  )

  res_sentinels <- test_sentinels(x, sentinels)
  if (is_true(res_sentinels)) {
    tests$sentinels <- TRUE
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE

  tests$type <- is_character(x) %@@% c(type = typeof(x))
  if (!tests$type) {
    return(tests)
  }

  l <- length(x)

  tests$len <- test_in_range(l, len, l) %@@%
    c(n = l)
  tests$na_n <- test_in_range(n_na <- sum(are_na2(x, nan = "f")), na_n, l) %@@%
    c(n = n_na)
  tests$dup_n <- test_in_range(n_dups <- sum(duplicated(unclass(x))), dup_n, l) %@@%
    c(n = n_dups)
  tests$set <- test_in_set(x, set, type_test = "character")
  tests$match <- test_in_pattern(x, match, perl = perl)
  tests$custom <- test_custom(x, custom, env)

  tests
}
# TODO: add sorted


#' @rdname tests-character
#' @export
test_double <- fn_core_to_test(core_double)

#' @rdname tests-character
#' @export
assert_double <- fn_core_to_assert(core_double, c(
  MSGS$sub_tests,
  type = \(attrs) glue("had type {{.val {attrs$type}}}, not {.val character}."),
  match = \(attrs) glue("had matches outside of the allowed patterns.")
))



# Helpers ----------------------------------------------------------------------

test_in_pattern <- function(x, match, perl = FALSE) {
  if (is_null(match)) {
    return(NULL)
  }

  collapse_patterns <- function(p) {
    if (is_null(p) || length(p) == 0) return(NULL)
    paste0("(?:", paste(p, collapse = ")|(?:"), ")")
  }
  # TODO: turn into helper function, use elswhere instead of simple paste0("|")

  if (is_character(match)) {
    pat <- collapse_patterns(match)
    all(grepl(pat, x, perl = perl))
  } else if (is_list(match)) {
    pat_yes <- collapse_patterns(match$yes)
    pat_no  <- collapse_patterns(match$no)

    yes_pass <- if (!is_null(pat_yes)) all(grepl(pat_yes, x, perl = perl)) else TRUE
    no_pass  <- if (!is_null(pat_no))  all(!grepl(pat_no, x, perl = perl)) else TRUE

    yes_pass && no_pass
  }
}
