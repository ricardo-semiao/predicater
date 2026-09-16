
# TODO: add user input checks, here and inside core_ functions that have
# specific subtests
# NOTE: all tests should have a 'na.rm = TRUE' behaviour. If the user dislikes
# NAs, use n_na

TESTS_MENU <- list()



# Range tests ------------------------------------------------------------------

TESTS_MENU$len <- function(x, arg, pars) {
  l <- pars$l
  test_in_range(l, arg, l) %@@% list(arg = arg, n = l)
}

TESTS_MENU$range <- function(x, arg, pars) {
  inside <- if (length(arg) == 2) {
    x >= arg[1] & x <= arg[2]

  } else {
    x %in% arg
  }

  all(inside, na.rm = TRUE) %@@% list(arg = arg, bad = x[which(! inside)])
}

TESTS_MENU$n_na <- function(x, arg, pars) {
  test_in_range(n_na <- sum(are_na2(x, nan = FALSE)), arg, pars$l) %@@%
    list(arg = arg, n = n_na)
}

TESTS_MENU$n_dup <- function(x, arg, pars) {
  test_in_range(n_dups <- sum(duplicated(unclass(x))), arg, pars$l) %@@%
    list(arg = arg, n = n_dups)
}
# TODO: deal with NA (incomparables)?

TESTS_MENU$n_nan <- function(x, arg, pars) {
  test_in_range(n_nan <- sum(are_nan(x, na = FALSE)), arg, pars$l) %@@%
    list(arg = arg, n = n_nan)
}

TESTS_MENU$n_inf <- function(x, arg, pars) {
  test_in_range(n_inf <- sum(are_inf(x, na = FALSE)), arg, pars$l) %@@%
    list(arg = arg, n = n_inf)
}

TESTS_MENU$n_null <- function(x, arg, pars) {
  test_in_range(n_null <- sum(vapply_lgl(x, is_null)), arg, pars$l) %@@%
    list(arg = arg, n = n_null)
}

TESTS_MENU$n_empty <- function(x, arg, pars) {
  test_in_range(n_empty <- sum(vapply_lgl(x, is_empty)), arg, pars$l) %@@%
    list(arg = arg, n = n_empty)
}



# Others -----------------------------------------------------------------------

TESTS_MENU$sorted <- function(x, arg, pars) {
  is_sorted(x, arg, na.rm = TRUE) %@@% list(arg = arg)
}

TESTS_MENU$set <- function(x, arg, pars) {
  if (! is_list(arg)) {
    all(x %in% arg, na.rm = TRUE) %@@% list(arg = arg)
  } else {
    is_matching_set(x, arg$yes, arg$no, arg$mode %||% "all") %@@% list(arg = arg)
  }
}
# CHECK: would be nice to be able to enforce order too
# CHECK: we dont allow multi-valued x's, e.g. "any of '1' or c('2', '3')"

TESTS_MENU$custom <- function(x, arg, pars) {
  test_custom(x, arg)
}

TESTS_MENU$custom_map <- function(x, arg, pars) {
  all(vapply(
    names(x) %||% seq_along(x), # Envs must be subsetted by name, not by index
    \(i) test_custom(x[[i]], arg, i), logical(1)
  ))
}

TESTS_MENU$env_has <- function(x, arg, pars) {
  found <- env_has(x, nms = arg, inherit = FALSE)
  all(found, na.rm = TRUE) %@@% list(arg = arg, missing = arg[! found])
}

TESTS_MENU$env_sees <- function(x, arg, pars) {
  found <- env_has(x, nms = arg, inherit = TRUE)
  all(found, na.rm = TRUE) %@@% list(arg = arg, missing = arg[! found])
}
# Shouldnt need na.rm = TRUE but just in case

TESTS_MENU$sentinels <- function(x, arg, pars = list()) {
  res <- if ("null" %in% arg) {
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

  res %@@% list(arg = arg)
}



# Sub-test helpers internals ---------------------------------------------------

#' @noRd
test_custom <- function(x, custom, i = NULL) {
  i_lab <- if (is_integer(i)) {
    " in the {.val {i}}-th element,"
  } else if (is_character(i)) {
    " in the {.val {i}} element,"
  } else {
    ""
  }

  tryCatch(
    {
      res <- custom(x)
      if (! is_bool(res)) {
        cli_abort(
          c(
            "{.code custom(x)} must return {.val {TRUE}} or {.val {FALSE}}.",
            "i" = glue2("Instead,[i_lab] it returned {.val {res}}."),
            "i" = "See this condition's {.code rs_user_fun_error} attribute for details."
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
        class = "rs_user_fun_error", parent = cnd
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
    all(n == range, na.rm = TRUE)

  } else if (length(range) == 2) {
    all(n >= range[1] & n <= range[2], na.rm = TRUE)

  } else {
    all(n %in% range, na.rm = TRUE)
  }
}



# Messages ---------------------------------------------------------------------

msg_n_arg <- function(x) {
  \(attrs, test) {
    base <- if (is_function(attrs$arg)) {
      "[x] must satisfy a custom function."
    } else if (length(attrs$arg) == 1) {
      "[x] must be {.val {[attrs$arg]}}."
    } else if (length(attrs$arg) == 2) {
      "[x] must be in range {.val {[attrs$arg[1]]}} to {.val {[attrs$arg[2]]}}."
    } else if (length(attrs$arg) > 2) {
      "[x] must be in set [fmt_vec(attrs$arg)]."
    }
    glue2(
      base,
      fmt_postfix("Was {.val {[attrs$n]}}.", test)
    )
  }
}
# TODO: deal with negative values in length = 2


TESTS_MSGS <- list(
  sentinels = \(attrs, test) {
    if (is_null(attrs$arg)) {
      "no sentinel values allowed."
    } else {
      glue2("can be sentinels: [fmt_vec(attrs$arg, Inf)].")
    }
  },

  range = \(attrs, test) {
    base <- if (length(attrs$arg) == 2) {
      "must be in range {.val {[attrs$arg[1]]}} to {.val {[attrs$arg[2]]}}."
    } else {
      "must be in set [fmt_vec(attrs$arg)]."
    }
    glue2(base, fmt_postfix("Found [fmt_vec(attrs$bad, 2)]. ", test))
  },

  len = msg_n_arg("length"),
  n_na = msg_n_arg("#of NA values"),
  n_dup = msg_n_arg("#of duplicate values"),
  n_nan = msg_n_arg("#of NaN values"),
  n_inf = msg_n_arg("#of Inf values"),
  n_null = msg_n_arg("#of NULL values"),
  n_empty = msg_n_arg("#of empty values"),

  sorted = \(attrs, test) {
    order <- switch(attrs$arg, asc = "ascending", desc = "descending")
    glue2(
      "must be in [order] order.",
      fmt_postfix("Did not.", test)
    )
  },

  set = \(attrs, test)  {
    glue2(
      "must be in a custom set.",
      fmt_postfix("Was not.", test)
    )
  },

  custom = \(attrs, test) {
    glue2(
      "must pass a custom test.",
      fmt_postfix("Did not.", test)
    )
  },

  custom_map = \(attrs, test) {
    glue2(
      "all elements must pass a custom test.",
      fmt_postfix("Did not.", test)
    )
  },

  env_has = \(attrs, test) {
    glue2(
      "must contain [fmt_vec(attrs$arg, 2)].",
      fmt_postfix("Is missing [fmt_vec(attrs$missing, 2)].", test)
    )
  },

  env_sees = \(attrs, test) {
    glue2(
      "must contain or inherit [fmt_vec(attrs$arg, 2)].",
      fmt_postfix("Is missing [fmt_vec(attrs$missing, 2)].", test)
    )
  }
)



# Helpers ----------------------------------------------------------------------

glue2 <- function(...) {
  glue(..., .open = "[", .close = "]", .envir = caller_env())
}

fmt_postfix <- function(msg, test) {
  if (! test) {
    paste0(" {.fail ", msg, "}")
  } else {
    ""
  }
}

fmt_vec <- function(x, trunc = 3, ...) {
  x <- cli::cli_vec(x, list("vec-trunc" = trunc, ...))
  cli::cli_fmt(cli::cli_text("{.val {x}}"))
}
