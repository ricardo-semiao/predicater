
# Package objects --------------------------------------------------------------

CNDS <- list()
MSGS <- list()
# TODO: document



# General helpers --------------------------------------------------------------

vapply_lgl <- function(.x, .f = as.logical, ..., .n = 1L) {
  vapply(X = .x, FUN = .f, FUN.VALUE = logical(.n), ...)
}


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



# Test helpers -----------------------------------------------------------------

# TODO: move to test helpers

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

fn_core_to_assert <- function(core, msgs_fns, env = caller_env()) {
  core_sym <- ensym(core)
  assert_name <- gsub("^core_", "assert_", as_string(core_sym))

  args <- fn_fmls(core)
  args_syms <- syms(names(args))

  args <- append(args, c(action = "abort"), after = 1)
  args["x_name"] <- list(NULL)
  args$report_untested <- TRUE
  args$`...` <- expr() # CHECK: consider args_cnd = list()

  body <- expr({
    # TODO: checks
    assert_name <- !!assert_name
    x_name <- x_name %||% expr_name(enexpr(x))

    tests <- (!!core_sym)(!!!args_syms)

    if (all(atomic_from_list_scalars(tests, "logical"))) {
      return(invisible(x))
    }

    tests_msgs <- create_tests_msgs(tests, msgs_fns, report_untested)
    for (i in seq_along(tests)) {
      attributes(tests[[i]]) <- list(
        test_info = attributes(tests[[i]]),
        test_msg = tests_msgs[i]
      )
    }

    msgs <- c(
      glue("{{.arg {x_name}}} failed {{.fn {assert_name}}}:"),
      tests_msgs, "",
      "i" = "See this condition's {.code rs_assert_error} attribute for details."
    )

    if (action == "abort") {
      cli_abort(
        msgs, call = env, ...,
        rs_assert_error = list(args = list(!!!args_syms), tests = tests)
      )
    } else if (action %in% c("warn", "inform")) {
      cnd_fun <- switch(action, warn = cli_warn, inform = cli_inform)
      cnd_fun(
        msgs, call = env, ...,
        rs_assert_error = list(args = list(!!!args_syms), tests = tests)
      )
    }

    invisible(x)
  })

  new_function(args, body, new_environment(list(msgs_fns = msgs_fns), env))
}
# TODO: add functionality to recieve modifiers for each test's message and the
# top message
# TODO: prune msgs_funs based on core args


create_tests_msgs <- function(tests, msgs_fns, report_untested) {
  tests_n <- length(tests)
  tests_msgs <- msgs_names <- character(tests_n)
  tests_names <- names(tests)

  for (i in seq_len(tests_n)) {
    ti <- tests[[i]]
    ni <- tests_names[i]

    if (is.na(ti) && report_untested) {
      tests_msgs[i] <- paste0(ni, ": ", "not tested due to previous failure.")
      msgs_names[i] <- "*"
    } else if (ti) {
      tests_msgs[i] <- paste0(ni, ": ", "ok.")
      msgs_names[i] <- "v"
    } else {
      tests_msgs[i] <- paste0(ni, ": ", msgs_fns[[ni]](attributes(ti)))
      msgs_names[i] <- "x"
    }
  }

  names(tests_msgs) <- msgs_names
  tests_msgs
}


initialize_tests <- function(pre, ..., post = character()) {
  syms <- ensyms(...)
  env <- caller_env()

  names <- vapply(syms, \(x) as_string(x), character(1))
  are_null <- vapply(syms, \(x) is_null(eval(x, env)), logical(1))
  test_names <- c(pre, names[!are_null], post)

  out <- vector("list", length(test_names))
  names(out) <- test_names
  for (nm in test_names) {
    out[[nm]] <- NA
  }
  out
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
