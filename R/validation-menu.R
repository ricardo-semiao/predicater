
# TODO: add user input checks, here and inside core_ functions that have
# specific subtests

TESTS_MENU <- list()



# Range tests ------------------------------------------------------------------

TESTS_MENU$len <- function(x, arg, pars) {
  l <- pars$l
  test_in_range(l, arg, l) %@@% c(n = l)
}

TESTS_MENU$range <- function(x, arg, pars) {
  res <- if (length(arg) == 2) {
    all(x >= arg[1] & x <= arg[2])

  } else {
    all(x %in% arg)
  }

  res %@@% c(range = arg)
}

TESTS_MENU$n_na <- function(x, arg, pars) {
  test_in_range(n_na <- sum(are_na2(x, nan = FALSE)), arg, pars$l) %@@%
    c(n = n_na)
}

TESTS_MENU$n_dup <- function(x, arg, pars) {
  test_in_range(n_dups <- sum(duplicated(unclass(x))), arg, pars$l) %@@%
    c(n = n_dups)
}
# TODO: deal with NA (incomparables)?

TESTS_MENU$n_nan <- function(x, arg, pars) {
  test_in_range(n_nan <- sum(are_nan(x, na = FALSE)), arg, pars$l) %@@%
    c(n = n_nan)
}

TESTS_MENU$n_inf <- function(x, arg, pars) {
  test_in_range(n_inf <- sum(are_inf(x, na = FALSE)), arg, pars$l) %@@%
    c(n = n_inf)
}

TESTS_MENU$n_null <- function(x, arg, pars) {
  test_in_range(n_null <- sum(vapply_lgl(x, is_null)), arg, pars$l) %@@%
    c(n = n_null)
}

TESTS_MENU$n_empty <- function(x, arg, pars) {
  test_in_range(n_empty <- sum(vapply_lgl(x, is_empty)), arg, pars$l) %@@%
    c(n = n_empty)
}



# Others -----------------------------------------------------------------------

TESTS_MENU$sorted <- function(x, arg, pars) {
  is_sorted(x, arg, na.rm = TRUE) %@@% c(sorted = arg)
}

TESTS_MENU$set <- function(x, arg, pars) {
  if (! is_list(arg)) {
    all(x %in% arg)
  } else {
    is_matching_set(x, arg$yes, arg$no, arg$mode %||% "all")
  }
}
# CHECK: would be nice to be able to enforce order too
# CHECK: we dont allow multi-valued x's, e.g. "any of '1' or c('2', '3')"

TESTS_MENU$custom <- function(x, arg, pars) {
  test_custom(x, arg)
}

TESTS_MENU$custom_map <- function(x, arg, pars) {
  all(vapply(x, test_custom, logical(1), custom = arg))
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
    is_nan(x, na = FALSE)

  } else if ("inf" %in% arg) {
    is_inf(x, na = FALSE)
  } else if ("+inf" %in% arg) {
    is_inf(x, na = FALSE, signs = "+")
  } else if ("-inf" %in% arg) {
    is_inf(x, na = FALSE, signs = "-")

  } else if (any(c("true", "t") %in% arg)) {
    is_true(x)
  } else if (any(c("false", "f") %in% arg)) {
    is_false(x)

  } else if ("nan" %in% arg) {
    is_nan(x, na = FALSE)

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
    return(range(n, l))
  }

  for (i in seq_along(range)) {
    r <- range[[i]]
    if (is_inf(r, signs = "+")) {
      range[[i]] <- l
    } else if (r < 0 && ! is_inf(r, signs = "-")) {
      range[[i]] <- l + r
    }
  }

  if (length(range) == 1) {
    all(n == range)

  } else if (length(range) == 2) {
    all(n >= range[1] & n <= range[2])

  } else {
    all(n %in% range)
  }
}



# Messages ---------------------------------------------------------------------

TESTS_MSGS <- list(
  len = \(attrs) glue("had length {{.val {{{attrs$n}}}}}."),
  range = \(attrs) glue("had values outside of {{.val {{{attrs$range}}}}}."),
  n_na = \(attrs) glue("had {{.val {{{attrs$n}}}}} NA values."),
  n_dup = \(attrs) glue("had {{.val {{{attrs$n}}}}} duplicated values."),
  n_nan = \(attrs) glue("had {{.val {{{attrs$n}}}}} NaN values."),
  n_inf = \(attrs) glue("had {{.val {{{attrs$n}}}}} Inf values."),
  n_null = \(attrs) glue("had {{.val {{{attrs$n}}}}} NULL values."),
  n_empty = \(attrs) glue("had {{.val {{{attrs$n}}}}} empty values."),
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
