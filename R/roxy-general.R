
ROXY$x <- function() {
  glue(r"(\[`any`] An object to test.)")
}

ROXY$test_res <- function(scalar = TRUE, na = FALSE) {
  if (scalar) {
    hint <- if (na) "`TRUE` | `FALSE` | `NA`" else "`TRUE` | `FALSE`"
    glue(r"(\[{hint}] The scalar result of the test.)")
  } else {
    glue(r"(\[`logical(length(x))`] The vectorized or result of the test.)")
  }
}
# CHECK: add a for argument to pass 'For fun1(): the scalar ...'

ROXY$na <- function(na = TRUE) {
  hint <- if (na) "`TRUE` | `FALSE` | `NA`" else "`TRUE` | `FALSE`"
  glue(r"(\[{hint}] What to return for `NA` values.)")
}
