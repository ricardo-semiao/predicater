
# Messages ---------------------------------------------------------------------

MSGS$sub_tests <- list(
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
  custom = \(attrs) glue("did not pass the custom test.")
)



# run_tests --------------------------------------------------------------------

run_tests <- function(short_circuit, params, tests_funs) {
  # Initialize tests:
  tests_names <- names(tests_funs)
  tests <- vector("list", length(tests_names))
  names(tests) <- tests_names
  for (nm in tests_names) {
    tests[[nm]] <- NA
  }

  # Always short circuit if passed sentinels:
  res_sentinels <- test_sentinels(params$x, params$sentinels)
  if (is_true(tests$sentinels <- res_sentinels)) {
    return(tests)
  }
  tests$sentinels <- res_sentinels %&&% TRUE # Sentinels is not a 'failable' test

  # Always short circuit if fails type:
  if (! (tests$type <- tests_funs$type(params))) {
    return(tests)
  }

  # Run the rest:
  tests_names <- setdiff(tests_names, c("type", "sentinels"))
  if (short_circuit) {
    for (nm in tests_names) {
      if (is_false(tests[[nm]] <- tests_funs[[nm]](params))) {
        return(tests)
      }
    }
  } else {
    for (nm in tests_names) {
      tests[[nm]] <- tests_funs[[nm]](params)
    }
  }

  tests
}



# Sub-test helpers -------------------------------------------------------------

#' @noRd
test_custom <- function(x, custom, env) {
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
            "i" = "Instead, it returned {.val {res}}."
          ),
          class = "rs_user_fun_error",
          call = env,
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
        parent = cnd,
        call = env
      )
    }
  )
}

test_custom_map <- function(x, custom_map, env = caller_env()) {
  if (is_null(custom_map)) {
    return(NULL)
  }

  all(vapply(x, test_custom, logical(1)))
}


#' @noRd
test_sentinels <- function(x, sentinels) {
  # Checks:
  # TODO: (maybe after checking we change the null <- scheme)


  # Main:
  if (is_null(sentinels)) {
    return(NULL)
  }

  if (!is_na(null <- sentinels["null"]) || null != "f") {
    if (is_null(x)) return(TRUE)
  }

  if (!is_na(na <- sentinels["na"]) || na != "f") {
    if (na == "any" && is_na2(x, 1)) return(TRUE)
    if (na == "t" && is_na2(x, 1, type = "integer")) return(TRUE)
  }

  if (!is_na(nan <- sentinels["nan"]) || nan != "f") {
    if (is_nan(x, 1, na = "f")) return(TRUE)
  }

  if (!is_na(inf <- sentinels["inf"]) || inf != "f") {
    if (is_inf(x, 1, na = "f", signs = inf)) return(TRUE)
  }

  if (!is_na(t <- sentinels["t"]) || t != "f") {
    if (is_true(x)) return(TRUE)
  }

  if (!is_na(f <- sentinels["f"]) || f != "f") {
    if (is_false(x)) return(TRUE)
  }

  FALSE
}


test_env_has <- function(env, has, inherit = FALSE) {
  if (is_null(has)) {
    return(NULL)
  }

  all(env_has(env, nms = has, inherit = inherit))
}
