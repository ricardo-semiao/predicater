
# Package objects --------------------------------------------------------------

CNDS <- list()
MSGS <- list()
# TODO: document



# General helpers --------------------------------------------------------------

`%@@%` <- function(x, attrs) {
  if (!is_null(x)) {
    attributes(x) <- c(attributes(x), as.list(attrs))
  }
  x
}
# TODO: export in rstools?

atomic_from_list_scalars <- function(x, type) {
  res <- vector(type)
  j <- 0
  for (i in seq_along(x)) {
    if (! is_null(x[[i]])) {
      j <- j + 1
      res[[j]] <- x[[i]]
    }
  }
  res
}
# same

fn_core_to_test <- function(core, env = caller_env()) {
  core_sym <- ensym(core)

  args <- fn_fmls(core)
  args$env <- NULL
  args_syms <- syms(names(fn_fmls(core)))

  body <- expr({
    tests <- (!!core_sym)(!!!args_syms)
    all(atomic_from_list_scalars(tests, "logical"))
  })

  new_function(args, body, env)
}

fn_core_to_assert <- function(core, env = caller_env()) {
  core_sym <- ensym(core)
  msgs_expr <- parse_expr(paste0("MSGS$", as_string(core_sym)))
  assert_name <- gsub("^core_", "assert_", as_string(core_sym))

  args <- fn_fmls(core)
  args_syms <- syms(names(args))

  body <- expr({
    assert_name <- !!assert_name

    x_expr <- enexpr(x)
    if (is_null(x_name) && !is_symbol(x_expr)) {
      cli_abort("Either supply {.arg x_name} or pass a symbol to {.arg x}.", call = env)
    }
    x_name <- x_name %||% as_string(x_expr)

    tests <- (!!core_sym)(!!!args_syms)

    if (all(atomic_from_list_scalars(tests, "logical"))) {
      return(invisible(x))
    }

    tests_n <- length(tests)
    tests_msgs <- character(tests_n)
    tests_names <- names(tests)
    for (i in seq_len(tests_n)) {
      ti <- tests[[i]]
      if (ti) {
        tests_msgs[i] <- paste0(tests_names[i], ": ", "Ok.")
        names(tests_msgs)[i] <- "v"
      } else {
        tests_msgs[i] <- (!!msgs_expr)[[tests_names[i]]](attributes(ti))
        names(tests_msgs)[i] <- "x"
      }
    }

    msgs <- c(
      glue("{{.arg {x_name}}} failed {{.fn {assert_name}}}:"),
      tests_msgs, "",
      "i" = "See this condition's {.code rs_assert_error} attribute for details."
    )

    if (raise) {
      cli_abort(
        msgs, call = env,
        rs_assert_error = list(args = list(!!!args_syms), tests = tests)
      )
    } else {
      msgs
    }
  })

  args["x_name"] <- list(NULL)
  args$raise <- TRUE

  new_function(args, body, env)
}
# TODO: create helper functions? Turn into a factory that creates a new env instead of !!?


#' Resolve based on a 'categorical' argument
#'
#' For the common case where some categorical argument dictates the return value
#' of a function, with options:
#' - `"tw"` for `TRUE` with a warning, `"t"` without.
#' - `"fw"` for `FALSE` with a warning, `"f"` without.
#' - `"naw"` or `"na"` for `NA` with the appropriate type.
#' - `"abort"` to abort with a message. Avoid, it is usually an anti-pattern
#'
#' @noRd
resolve_category <- function(
  x, msg,
  ..., env = caller_env(), warn = FALSE, x_name = NULL,
  na_type = typeof(x)
) {
  force(env)
  x_name <- x_name %||% ensym(x)

  if (grepl("w", x)) {
    cli_warn(msg, ..., call = env)
    x <- sub("w", "", x)
  }

  switch(x,
    t = TRUE,
    f = FALSE,
    na = NAS[[na_type]],
    abort = cli_abort(msg, ..., call = env),
    cli_abort(
      "Invalid value for argument {.arg {x_name}}: {.val {x}}.",
      .internal = TRUE, call = env
    )
  )
}



# Cli helpers ------------------------------------------------------------------

# Cli theme:
CLI_THEME <- list(
  ".bold" = list("font-weight" = "bold"),
  ".italic" = list("font-style" = "italic"),
  ".blue" = list("color" = "blue"),
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
