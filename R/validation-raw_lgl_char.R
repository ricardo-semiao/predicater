
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
#' @param x `r ROXY$x()`
#' @param len,n_na,n_true `r ROXY$x_n("len,n_na,n_true")`
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
#' @examples
#' x <- c(TRUE, FALSE, NA, TRUE)
#'
#' args <- list(
#'   len = c(1, 10),              # Length must be between 1 and 10 (will pass)
#'   n_na = 0,                    # No NA values allowed (will fail)
#'   n_true = c(1, 2),            # Between 1 and 2 TRUE values (will pass)
#'   sentinels = c("null"),       # Allow NULL x (not the case of x)
#'   custom = NULL                # No custom predicate
#' )
#'
#' do.call(test_logical, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_logical, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test-logical_raw
NULL


# Core functions:

core_logical <- function(
  x,
  len = NULL, n_na = NULL, n_true = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, n_na, n_true, custom,
    tests_pars = list(l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_logical(x) %@@% c(type = typeof(x)),
      n_true = \(x, arg, pars) {
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
    n_true = \(attrs) "count of TRUE elements does not fall within the expected range."
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
#' @param x `r ROXY$x()`
#' @param len,n_na,n_dup,n_char `r ROXY$x_n("len,n_na,n_dup,n_char")`
#' @param set `r ROXY$set("character")`
#' @param match \[`character()` | `list(yes = , no = )` | `NULL`]
#'   Test if all elements of `x` match regular expression patterns. Pass a
#'   character vector of patterns (implicitly OR'd together), or a list with
#'   `yes` and `no` character vectors of patterns. Set to `NULL` to not test.
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
#' @examples
#' x <- c("apple", "banana", "cherry", "banana")
#'
#' args <- list(
#'   len = c(1, 10),              # Length must be between 1 and 10 (will pass)
#'   n_na = 0,                    # No NA values allowed (will pass)
#'   n_dup = 0,                   # No duplicate strings allowed (will fail)
#'   n_char = c(1, 10),           # Character length of each string between 1 and 10 (will pass)
#'   set = list(no = c("date")),  # Strings must not contain "date" (will pass)
#'   match = list(
#'     yes = "^[a-z]+$",          # All elements must be lowercase letters (will pass)
#'     no = "x"                   # No element can contain "x" (will pass)
#'   ),
#'   sorted = FALSE,              # Don't require vector to be sorted in alphabetical order (will pass)
#'   sentinels = c("null"),       # Allow NULL x (not the case of x)
#'   custom = NULL                # No custom predicate
#' )
#'
#' do.call(test_character, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_character, c(list(x), args))) #> Error
#'
#' @name test_character
NULL

core_character <- function(
  x,
  len = NULL, n_na = NULL, n_dup = NULL, n_char = NULL,
  set = NULL, match = NULL, sorted = NULL,
  sentinels = NULL, custom = NULL, perl = FALSE,
  short_circuit
) {
  run_tests(
    x, sentinels, len, n_na, n_dup, n_char, set, match, sorted, custom,
    tests_pars = list(l = length(x), perl = perl), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_character(x) %@@% c(type = typeof(x)),
      n_char = \(x, arg, pars) {
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
    n_char = \(attrs) "character counts do not fall within the expected range.",
    match = \(attrs) "had matches outside of the allowed patterns."
  )
)



# Helpers ----------------------------------------------------------------------

test_in_pattern <- function(x, match, perl = FALSE) {
  if (is_null(match)) {
    return(NULL)
  }

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
