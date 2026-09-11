
# General ----------------------------------------------------------------------

ROXY$test_returns <- function(fun_name = NULL) {
  fun_name <- if (is_null(fun_name)) "*" else fun_name
  glue(r"(
  - \[`TRUE` | `FALSE`] for `test_{fun_name}()`.
  - \[`=x`] `invisible(x)` for `assert_{fun_name}()`, or aborts if the test fails.
  )")
}



# Assert args ------------------------------------------------------------------

# ROXY$assert_args <- function() {
#   args <- c("action", "env", "short_circuit", "x_name", "report_untested")
#   params <- set_names(character(length(args)), args)
#   for (arg in args) {
#     params[[arg]] <- paste0("@param ", ROXY[[arg]]())
#   }

#   paste0(params, collapse = "\n")
# }
# Aparently, doesn't work

ROXY$action <- function() {
  glue(r"(
  \[`"abort"` | `"warn"` | `"inform"`] Action to take when the test fails:
    - `"abort"` to stop execution and throw an error.
    - `"warning"` to issue a warning and return `invisible(x)`.
    - `"message"` to issue a message and return `invisible(x)`.
  )")
}

ROXY$env <- function() {
  glue(r"(
  \[`environment()` | `call()` | `NULL` | `missing_arg()`] The call to inform as \
  the origin of the error, passed to [rlang::abort()]:
    - An environment in the call stack or a hard-coded defused call.
    - `NULL` for no information.
    - `missing_arg()` to use the assert function itself.
    - The default is `caller_env()`, to display the function where the assertion \
      was called.
  )")
}

ROXY$short_circuit <- function() {
  glue(r"(
  \[`TRUE` | `FALSE`] If `TRUE`, the tests results will be reported up to the \
    first failure. Else, all tests results are reported. The former is more \
    efficient, while the latter is more informative.
  )")
}

ROXY$x_name <- function() {
  glue(r"(
  \[`character(1)` | `NULL`] The name of the object to use in the error message. \
    If `NULL`, the name is inferred from the expression passed to `x`.
  )")
}

ROXY$report_untested <- function() {
  glue(r"(
  \[`TRUE` | `FALSE`] If `TRUE`, the tests that were not run due to short-\
    circuiting will be reported as untested, else, ignored.
  )")
}



# Test args --------------------------------------------------------------------

ROXY$sentinels <- function() {
  glue(r"(
  \[`character()` | `NULL`] Each entry in this character vector allows `x` to \
  also be some scalar sentinel below. Set to `NULL` to disconsider sentinels.
    - `"null"` for `NULL`
    - `"na"` for any `NA` type, or `"na_logical"` for `NA`, `"na_integer"` for \
      `NA_integer_`, `"na_real"` for `NA_real_`, `"na_complex"` for \
      `NA_complex_`, and `"na_character"` for `NA_character_`.
    - `"nan"` for `NaN`.
    - `"+inf"` for `+Inf`, `"-inf"` for `-Inf`, and `"inf"` for both.
    - `"t"` for `TRUE`, and `"f"` for `FALSE`.
  )")
}


ROXY$x_n <- function(args) {
  args <- strsplit(args, ",")[[1]]

  args_labels <- c(
    len = "length",
    na_n = "number of `NA` elements",
    dup_n = "number of duplicate elements",
    nan_n = "number of `NaN` elements",
    inf_n = "number of `Inf` elements",
    null_n = "number of `NULL` elements",
    empty_n = "number of elements with zero length",
    call_n = "number of 'language' elements",
    sym_n = "number of 'symbol' elements",
    literal_n = "number of syntactic literal elements",
    depth_n = "number of parents",
    true_n = "number of `TRUE` elements",
    char_n = "number of characters (vectorized)"
  )

  args_text <- paste0(args_labels[args], collapse = ", ") # TODO: pluralize, 'respectively'

  glue(r"(
  \[`numeric()` | `\(){{}}` | `NULL`] Possible values for the {args_text}. The \
  options {if (length(args) > 0) "of each argument 'arg' "}are:
    - `NULL` to not test.
    - A single non-negative number to test for `. == arg`.
    - A single negative number to test for `. == length(x) + arg`.
    - A vector of two non-negative numbers to test for `arg[1] <= . <=
      arg[2]` (`Inf` is allowed).
    - A vector of three or more non-negative numbers to test for `. %in% arg`.
    - A function that recieves the value to test and the length of `x`, and
      returns a single `TRUE` or `FALSE`.
  )")
}


ROXY$set <- function(type) {
  glue(r"(
  \[`{type}()` | `list(yes = , no = )` | `NULL`] Test if all values
    of `x` are in a set of allowed values. Use a list with `yes` and `no`
    elements to defined allowed and disallowed values. Set to `NULL` to not test.
  )")
}


ROXY$sorted <- function() {
  glue(r"(
  \[`"asc"` | `"desc"` | `NULL`] Test if `x` is sorted in ascending (`"asc"`) \
    or descending (`"desc"`) order. Set to `NULL` to not test. Pair with `dup_n` \
    to test for strictly sorted values.
  )")
}


ROXY$custom <- function() {
  glue(r"(
  \[`function(x)` | `NULL`] A custom function that takes `x` as first argument \
    and returns a single `TRUE` or `FALSE`. Set to `NULL` to not test.
  )")
}


ROXY$custom_map <- function() {
  glue(r"(
  \[`function(x)` | `NULL`] A custom function that is applied to each element \
    of `x`. Must return a single `TRUE` or `FALSE` for each element. Set to \
    `NULL` to not test.
  )")
}
