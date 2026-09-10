
ROXY <- list()

ROXY$test_returns <- function(fun_name = NULL) {
  fun_name <- if (is_null(fun_name)) "*" else fun_name
  glue(r"(
  - \[`TRUE` | `FALSE`] for `test_{fun_name}()`.
  - \[`=x`] for `assert_{fun_name}()`, or aborts if the test fails.
  )")
}


ROXY$sentinels <- function() {
  glue(r"(
  \[`character()`] A named character that allows `x` to be some scalar \
  sentinel instead. By default, none are allowed. Add each named entry below \
  to change the behaviour:
    - `null`: `"f"` to disallow `NULL`, `"t"` to allow.
    - `na`: `"f"` to disallow `NA`, `"t"` to allow `NA`, `"any"` to allow any \
      NA value.
    - `nan`: `"f"` to disallow `NaN`, `"t"` to allow.
    - `inf`: `"f"` to disallow, `"+"` to allow `+Inf`, `"-"` to allow `-Inf`, \
      `"+-"` to allow both.
    - `t`: `"f"` to disallow `TRUE`, `"t"` to allow.
    - `f`: `"f"` to disallow `FALSE`, `"t"` to allow.
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
    depth_n = "number of parents"
  )

  args_text <- paste0(args_labels[args], collapse = ", ") # TODO: pluralize, 'respectively'

  glue(r"(
  \[`integer()` | `\(){{}}` | `NULL`] Possible values for the {args_text}. The \
  options are:
  - `NULL` to not test.
  - A single non-negative number to test for `. == range`.
  - A single negative number to test for `. == length(x) + range`.
  - A vector of two non-negative numbers to test for `range[1] <= . <=
    range[2]` (`Inf` is allowed).
  - A vector of three or more non-negative numbers to test for `. %in% range`.
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
