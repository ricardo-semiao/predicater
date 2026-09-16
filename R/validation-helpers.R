
# run_tests --------------------------------------------------------------------

run_tests <- function(x, ..., tests_pars, short, menu_add) {
  # Initialize tests:
  tests_names <- c(
    "sentinels", "type", # WARN: assuming sentinels are always in 1st position
    vapply(ensyms(...), as_string, character(1))[-1]
  )
  args <- set_names(c(type = NA, list(...)), tests_names)

  tests_args <- tests <- list()
  for (nm in tests_names) {
    if (! is_null(args[[nm]])) {
      tests[[nm]] <- NA
      tests_args[[nm]] <- args[[nm]]
    }
  }


  # Tests menu:
  menu <- c(menu_add, TESTS_MENU)[tests_names]
  # menu_add first takes precendece over TESTS_MENU


  # Always short circuit if passed sentinels:
  if (is_null(tests_args$sentinels)) {
    tests$sentinels <- NULL
  } else if (tests$sentinels <- menu$sentinels(x, tests_args$sentinels)) {
    return(tests)
  } else {
    tests$sentinels <- TRUE # Sentinels is not a 'failable' test
  }

  # Always short circuit if fails type, which is always present (and non-null):
  if (! (tests$type <- menu$type(x, tests_args$type, tests_pars))) {
    return(tests)
  }
  # tests_args$type is always NULL, use tests_pars instead


  # Run the rest:
  tests_names <- setdiff(tests_names, c("sentinels", "type"))

  if (short) {
    for (nm in tests_names) {
      arg <- tests_args[[nm]]
      if (is_null(arg)) {
        tests[[nm]] <- NULL
      } else if (! (tests[[nm]] <- menu[[nm]](x, arg, tests_pars))) {
        return(tests)
      }
    }

  } else {
    for (nm in tests_names) {
      arg <- tests_args[[nm]]
      tests[[nm]] <- if (is_null(arg)) {
        NULL
      } else {
        menu[[nm]](x, arg, tests_pars)
      }
    }
  }

  tests
}
# CHECK: see performance cost of ensyms(...), we could manually pass the named
# list of tests_args



# Factories --------------------------------------------------------------------

fn_core_to_test <- function(core) {
  core_sym <- ensym(core)

  args <- fn_fmls(core)
  args$env <- NULL
  args$short_circuit <- NULL
  args_syms <- syms(names(args))

  body <- expr({
    # Tests left to core
    tests <- (!!core_sym)(!!!args_syms, short_circuit = TRUE)
    all(atomic_from_list_of_scalars(tests, "logical"))
  })

  new_function(args, body, caller_env())
}


fn_core_to_assert <- function(core, msgs_add) {
  core_sym <- ensym(core)
  assert_name <- gsub("^core_", "assert_", as_string(core_sym))

  args <- fn_fmls(core)
  args_core_nms <- union(
    setdiff(names(args), c("x", "env", "short_circuit")),
    "sentinels"
  )
  args_core_syms <- syms(names(args))

  msgs_fns <- c(msgs_add, TESTS_MSGS[intersect(names(TESTS_MSGS), args_core_nms)])

  args <- c(
    x = expr(), args[args_core_nms],
    action = "abort", env = expr(caller_env()), x_name = list(NULL),
    short_circuit = TRUE, report_untested = TRUE,
    args_cnd = list(list())
  )


  body <- expr({
    # TODO: checks
    assert_name <- !!assert_name
    x_expr <- enexpr(x)
    x_name <- x_name %||% if (is_code(x_expr)) expr_name(x_expr) else "x"

    tests <- (!!core_sym)(!!!args_core_syms)

    if (all(atomic_from_list_of_scalars(tests, "logical"))) {
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
      "i" = "See {.fn predicater::{assert_name}} and this condition's \\
      {.code rs_assert_error} attribute for details."
    )

    cli::cli_div(theme = CLI_THEME)
    if (action == "abort") {
      do.call(cli_abort, c(
        message = list(msgs), call = env, args_cnd,
        rs_assert_error = list(args = list(!!!args_core_syms), tests = tests)
      ))
    } else if (action %in% c("warn", "inform")) {
      cnd_fun <- switch(action, warn = cli_warn, inform = cli_inform)
      do.call(cnd_fun, c(
        message = list(msgs), call = env, args_cnd,
        rs_assert_error = list(args = list(!!!args_core_syms), tests = tests)
      ))
    }

    invisible(x)
  })

  new_function(args, body, new_environment(list(msgs_fns = msgs_fns), caller_env()))
}
# TODO: add functionality to recieve modifiers for each test's message and the
# top message

# CRAN dot removal:
utils::globalVariables(c("action", "args_cnd", "report_untested", "x"))


create_tests_msgs <- function(tests, msgs_fns, report_untested) {
  tests_n <- length(tests)
  tests_msgs <- msgs_names <- character(tests_n)
  tests_names <- names(tests)

  for (i in seq_len(tests_n)) {
    ti <- tests[[i]]
    ni <- tests_names[i]
    fi <- msgs_fns[[ni]]

    if (is.na(ti) && report_untested) {
      tests_msgs[i] <- paste0("(skip) ", align_ni(ni), "skiped given failure.")
      msgs_names[i] <- "*"
    } else if (ti) {
      tests_msgs[i] <- paste0("(pass) ", align_ni(ni), fi(attributes(ti), ti))
      msgs_names[i] <- "v"
    } else {
      tests_msgs[i] <- paste0("(fail) ", align_ni(ni), fi(attributes(ti), ti))
      msgs_names[i] <- "x"
    }
  }

  names(tests_msgs) <- msgs_names
  tests_msgs
}
# TODO: pass arg (the test argument) to msg_fun too. Then we dont depend on
# passing arg to the test attrs via %@@%

align_ni <- function(ni, max_nchar = 6) {
  n <- nchar(ni)
  if (n < max_nchar) {
    paste0(ni, strrep("\u2002", max_nchar - n), ": ")
  } else {
    paste0(ni, ": ")
  }
}
