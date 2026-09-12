
# TODO: add user input checks, here and inside core_ functions that have
# specific subtests

TESTS_MENU <- list()



# Range tests ------------------------------------------------------------------

TESTS_MENU$len <- function(x, arg, pars) {
  l <- pars$l
  test_in_range(l, arg, l) %@@% c(n = l)
}

TESTS_MENU$range <- function(x, arg, pars) {
  test_in_range(x, arg, pars$l) %@@% c(range = arg)
}

TESTS_MENU$na_n <- function(x, arg, pars) {
  test_in_range(n_na <- sum(are_na2(x, nan = "f")), arg, pars$l) %@@%
    c(n = n_na)
}

TESTS_MENU$dup_n <- function(x, arg, pars) {
  test_in_range(n_dups <- sum(duplicated(unclass(x))), arg, pars$l) %@@%
    c(n = n_dups)
}
# TODO: deal with NA (incomparables)?

TESTS_MENU$nan_n <- function(x, arg, pars) {
  test_in_range(n_nan <- sum(are_nan(x, na = "f")), arg, pars$l) %@@%
    c(n = n_nan)
}

TESTS_MENU$inf_n <- function(x, arg, pars) {
  test_in_range(n_inf <- sum(are_inf(x, na = "f")), arg, pars$l) %@@%
    c(n = n_inf)
}

TESTS_MENU$null_n <- function(x, arg, pars) {
  test_in_range(n_null <- sum(vapply_lgl(x, is_null)), arg, pars$l) %@@%
    c(n = n_null)
}

TESTS_MENU$empty_n <- function(x, arg, pars) {
  test_in_range(n_empty <- sum(vapply_lgl(x, is_empty)), arg, pars$l) %@@%
    c(n = n_empty)
}



# Others -----------------------------------------------------------------------

TESTS_MENU$sorted <- function(x, arg, pars) {
  if (arg == "asc") {
    !is.unsorted(x)
  } else if (arg == "desc") {
    !is.unsorted(rev(x))
  } %@@%
    c(sorted = arg)
}
# TODO: what to do with na.rm = TRUE?

TESTS_MENU$set <- function(x, arg, pars) {
  if (! is_list(arg)) {
    all(x %in% arg)

  } else {
    if (! is_empty(arg$no) && any(x %in% arg$no)) {
      return(FALSE)
    }

    if (is_empty(yes <- arg$yes)) {
      return(TRUE)
    }

    x_in_yes <- x %in% arg$yes
    switch(arg$mode %||% "all",
      all = all(x_in_yes),
      any = any(x_in_yes),
      only = all(x_in_yes) && all(arg$yes %in% x)
    )
  }
}
# CHECK: would be nice to be able to enforce order too
# CHECK: we dont allow multi-valued x's, e.g. "any of '1' or c('2', '3')"

TESTS_MENU$custom <- function(x, arg, pars) {
  test_custom(x, arg, pars$env)
}

TESTS_MENU$custom_map <- function(x, arg, pars) {
  all(vapply(x, test_custom, logical(1), custom = arg, env = pars$env))
}

TESTS_MENU$env_has <- function(x, arg, pars) {
  all(env_has(x, nms = arg, inherit = FALSE))
}

TESTS_MENU$env_sees <- function(x, arg, pars) {
  all(env_has(x, nms = arg, inherit = TRUE))
}

TESTS_MENU$sentinels <- function(x, arg, pars = list()) {
  if ("null" %in% arg) {
    is_null(x)

  } else if ("na" %in% arg) {
    is_na(x)

  } else if ("empty" %in% arg) {
    length(x) == 0

  } else if ("nan" %in% arg) {
    is_nan(x, na = "f")

  } else if ("inf" %in% arg) {
    is_inf(x, 1, na = "f")
  } else if ("+inf" %in% arg) {
    is_inf(x, 1, na = "f", signs = "+")
  } else if ("-inf" %in% arg) {
    is_inf(x, 1, na = "f", signs = "-")

  } else if ("t" %in% arg) {
    is_true(x)
  } else if ("f" %in% arg) {
    is_false(x)

  } else if ("nan" %in% arg) {
    is_nan(x, na = "f")

  } else if ("na_logical" %in% arg) {
    identical(x, NA)
  } else if ("na_integer" %in% arg) {
    identical(x, NA_integer_)
  } else if ("na_double" %in% arg) {
    identical(x, NA_real_)
  } else if ("na_complex" %in% arg) {
    identical(x, NA_complex_)
  } else if ("character" %in% arg) {
    identical(x, NA_character_)

  } else {
    FALSE
  }
}



# Sub-test helpers internals ---------------------------------------------------

#' @noRd
test_custom <- function(x, custom) {
  if (is_null(custom)) {
    return(NULL)
  }

  tryCatch(
    {
      res <- custom(x)
      if (! is_bool(res)) {
        cli_abort(
          c(
            "{.code custom(x)} must return {.val {TRUE}} or {.val {FALSE}}.",
            "i" = "Instead, with {.arg {x_name[i]}}, it returned {.val {res}}."
          ),
          class = "rs_user_fun_error",
          rs_user_fun_error = list(bad_result = res)
        )
      }
      res
    },
    rs_user_fun_error = cnd_signal,
    error = \(cnd) {
      cli_abort(
        "Evaluating {.code custom(x)} raised an error.",
        class = "rs_test_custom_error",
        parent = cnd
      )
    }
  )
}


test_in_range <- function(n, range, l) {
  if (is_function(range)) {
    range(n, l)
  } else if (length(range) == 1) {
    if (is_inf(range, signs = "+")) {
      range <- l
    }
    all(n == range)
  } else if (length(range) == 2) {
    all(n >= range[1] & n <= range[2])
  } else if (length(range) > 2) {
    all(n %in% range)
  }
}



# Messages ---------------------------------------------------------------------

TESTS_MSGS <- list(
  len = \(attrs) glue("had length {{.val {{{attrs$n}}}}}."),
  range = \(attrs) glue("had values outside of {{.val {{{attrs$range}}}}}."),
  na_n = \(attrs) glue("had {{.val {{{attrs$n}}}}} NA values."),
  dup_n = \(attrs) glue("had {{.val {{{attrs$n}}}}} duplicated values."),
  nan_n = \(attrs) glue("had {{.val {{{attrs$n}}}}} NaN values."),
  inf_n = \(attrs) glue("had {{.val {{{attrs$n}}}}} Inf values."),
  sorted = \(attrs) {
    order <- switch(attrs$sorted, asc = "ascending", desc = "descending")
    glue("was not sorted in {order} order.")
  },
  set = \(attrs) glue("had values outside of the allowed set."),
  custom = \(attrs) glue("did not pass the custom test."),
  custom_map = \(attrs) glue("not all elements passed the custom map test."),
  env_has = \(attrs) glue("supplied symbols were not found in the supplied environment."),
  env_sees = \(attrs) glue("supplied symbols were not found in the supplied environment or its parents.")
)
