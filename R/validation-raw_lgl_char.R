
#' @include validation-helpers.R validation-menu.R
NULL



# Raw and logical ----------------------------------------------------------

#' Validation - Logical and Raw vectors
#'
#' @description
#' Test if an object is a logical or raw vector.
#'
#' They all are predicate tests, while the `assert_*()` functions validate their
#' input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len,na_n,true_n `r ROXY$x_n("len,na_n,true_n")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns()`
#'
#' @name test-logical_raw
NULL


# Core functions:

core_logical <- function(
  x,
  len = NULL, na_n = NULL, true_n = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, na_n, true_n, custom,
    tests_pars = list(l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_logical(x) %@@% c(type = typeof(x)),
      true_n = \(x, arg, pars) {
        n_true <- sum(x, na.rm = TRUE)
        test_in_range(n_true, arg, pars$l) %@@% c(n = n_true)
      }
    )
  )
}


core_raw <- function(
  x,
  len = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, custom,
    tests_pars = list(l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_raw(x) %@@% c(type = typeof(x))
    )
  )
}


# Test functions:

#' @rdname test-logical_raw
#' @export
test_logical <- fn_core_to_test(core_logical)

#' @rdname test-logical_raw
#' @export
test_raw <- fn_core_to_test(core_raw)


# Assert functions:

#' @rdname test-logical_raw
#' @export
assert_logical <- fn_core_to_assert(
  core_logical,
  msgs_add = list(
    type = \(attrs) glue("had type `{attrs$type}`, not `logical`."),
    true_n = \(attrs) "count of TRUE elements does not fall within the expected range."
  )
)

#' @rdname test-logical_raw
#' @export
assert_raw <- fn_core_to_assert(
  core_raw,
  msgs_add = list(
    type = \(attrs) glue("had type `{attrs$type}`, not `raw`.")
  )
)



# Character --------------------------------------------------------------------

#' Validation - Character vectors
#'
#' @description
#' Test if an object is a character vector.
#'
#' `test_character()` is the predicate test, while `assert_character()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len,na_n,dup_n,char_n `r ROXY$x_n("len,na_n,dup_n,char_n")`
#' @param set `r ROXY$set("character")`
#' @param match \[`character()` | `list(yes = , no = )` | `NULL`] Test if all
#'   elements of `x` match regular expression patterns. Pass a character vector of
#'   patterns (implicitly OR'd together), or a list with `yes` and `no`
#'   character vectors of patterns. Set to `NULL` to not test.
#' @param perl \[`TRUE` | `FALSE`] Should Perl-compatible regexps be used in
#'   `match`?
#' @param sorted `r ROXY$sorted()`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("character")`
#'
#' @name test_character
NULL

core_character <- function(
  x,
  len = NULL, na_n = NULL, dup_n = NULL, char_n = NULL,
  set = NULL, match = NULL, sorted = NULL,
  sentinels = NULL, custom = NULL, perl = FALSE,
  short_circuit
) {
  run_tests(
    x, sentinels, len, na_n, dup_n, char_n, set, match, sorted, custom,
    tests_pars = list(l = length(x), perl = perl), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_character(x) %@@% c(type = typeof(x)),
      char_n = \(x, arg, pars) {
        n_char <- nchar(x)
        test_in_range(n_char, arg, pars$l) %@@% c(n = n_char)
      },
      match = \(x, arg, pars) test_in_pattern(x, arg, perl = pars$perl)
    )
  )
}
# TODO: add empty_n to count ""


#' @rdname test_character
#' @export
test_character <- fn_core_to_test(core_character)

#' @rdname test_character
#' @export
assert_character <- fn_core_to_assert(
  core_character,
  msgs_add = list(
    type = \(attrs) glue("had type `{attrs$type}`, not `character`."),
    char_n = \(attrs) "character counts do not fall within the expected range.",
    match = \(attrs) "had matches outside of the allowed patterns."
  )
)



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
