
# Package objects --------------------------------------------------------------

CNDS <- list()
ROXY <- list()



# General helpers --------------------------------------------------------------

vapply_lgl <- function(.x, .f = as.logical, ..., .n = 1L) {
  vapply(X = .x, FUN = .f, FUN.VALUE = logical(.n), ...)
}


`%@@%` <- function(x, attrs) {
  if (! is_null(x)) {
    attributes(x) <- attrs # Could be c(attributes(x), attrs) but we only call
    # it with no attributes x
  }
  x
}


atomic_from_list_of_scalars <- function(x, type = NULL) {
  res <- vector(type %||% typeof(x[[1]]))
  j <- 0
  for (i in seq_along(x)) {
    if (! is_null(x[[i]])) {
      j <- j + 1
      res[[j]] <- x[[i]]
    }
  }
  res
}


collapse_patterns <- function(p) {
  if (is_null(p) || length(p) == 0) return(NULL)
  paste0("(?:", paste(p, collapse = ")|(?:"), ")")
}



# Cli helpers ------------------------------------------------------------------

# Cli theme:
CLI_THEME <- list(
  ".bold" = list("font-weight" = "bold"),
  ".italic" = list("font-style" = "italic"),
  #".blue" = list("color" = "blue"),
  ".fail" = list("color" = "red"),
  h1 = list(
    "font-weight" = "bold",
    "margin-top" = 1,
    "margin-bottom" = 0,
    fmt = \(x, rule_width = NULL) {
      w <- clamp(round(cli::console_width() / 2), 20, cli::console_width())
      cli::rule(left = x, width = w)
    }
  ),
  ".m1" = list("margin-bottom" = 1),
  ".m0" = list("margin-bottom" = 0)
)


# General helpers:
#' @noRd
cli_par_text <- function(..., cl = "m1") {
  env <- caller_env()
  cli::cli_par(class = cl)

  for (x in list(...)) {
    if (! is_null(x)) cli::cli_text(x, .envir = env)
  }
}


#' @noRd
cli_output <- function(x, colors = 256, ..., cat = TRUE) {
  out <- with_options(
    utils::capture.output(x),
    cli.num_colors = colors, ...
  )
  if (cat) cli::cli_verbatim(out)
}


#' @noRd
cli_br <- function(n = 1) {
  cli::cli_verbatim(strrep("\n", n))
}
# Avoid, use a div() with bottom margin if possible


cli_capture <- function(x) {
  messages_env <- new_environment(list(ms = character()))
  withCallingHandlers(
    utils::capture.output(x, type = "message"),
    message = \(cnd) messages_env$ms <- c(messages_env$ms, cnd$message)
  )
  messages_env$ms
}



# Constants --------------------------------------------------------------------

NAS <- list(
  logical = NA,
  integer = NA_integer_,
  double = NA_real_,
  complex = NA_complex_,
  character = NA_character_
)

COERCERS <- list(
  logical = as.logical,
  integer = as.integer,
  double = as.double,
  complex = as.complex,
  character = as.character,
  raw = as.raw
)

TYPES <- c(
  "logical", "integer", "double", "complex", "character", "raw", "list",
  "pairlist", "expression", "S4", "object", "symbol", "language", "closure",
  "primitive", "builtin", "environment", "promise", "...", "char",
  "bytecode", "externalptr", "weakref", "NULL", "any"
)
# CHECK: all could be uppercase
