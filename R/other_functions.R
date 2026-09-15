
# Unrelated functions ----------------------------------------------------------

#' Combine many vectors into one vector
#'
#' @description
#' Combine all arguments into a new vector of common type. This function is a
#' renaming of [vctrs::vec_c()].
#'
#' @inheritParams vctrs::vec_c
#'
#' @inherit vctrs::vec_c return
#'
#' @examples
#' c2(FALSE, 1L, 1.5) #> c(0, 1.0, 1.5)
#'
#' # Date/times:
#' c(Sys.Date(), Sys.time()) # Two Date-s
#' c(Sys.time(), Sys.Date()) # Two POSIXct-s
#' c2(Sys.Date(), Sys.time()) # Two POSIXct-s
#'
#' # Factors:
#' c(factor("a"), factor("b")) # Factor with levels "a" and "b"
#' c2(factor("a"), factor("b")) # Factor with levels "a" and "b"
#'
#' # By default, named inputs must be length 1:
#' c2(name = 1) #> c(name = 1)
#' try(c2(name = 1:3)) #> Error
#'
#' # Pass a name specification to work around this:
#' c2(name = 1:3, .name_spec = "{outer}_{inner}")
#'
#' @export
c2 <- function(
  ..., .ptype = NULL, .name_spec = NULL, .name_repair = "minimal"
) {
  vctrs::vec_c(
    ..., .ptype = .ptype, .name_spec = .name_spec, .name_repair = "minimal"
  )
}



# Parallel and NA --------------------------------------------------------------

#' Parallel any and all
#'
#' @description
#' These functions are wrappers around [vctrs::vec_pany()] and
#' [vctrs::vec_pall()], varyiants of [any()] and [all()] that work in parallel
#' on multiple inputs at once. They work similarly to how [pmin()] and [pmax()]
#' are parallel variants of [min()] and [max()].
#'
#' `pany_na()` and `pall_na()` are variants that check for `NA` values in
#' parallel.
#'
#' @param ... \[`logical()` each] Logical vectors with the same size.
#' @param .na `r ROXY$na()`
#' @param .nan \[`TRUE` | `FALSE`] Should `NaN` values be treated as `NA`?
#'
#' @returns \[`logical()`] A logical vector of the same size as the inputs.
#'
#' @examples
#' a <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
#' b <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)
#'
#' # Default behavior treats missings like `|` does:
#' pany(a, b)
#' a | b
#'
#' # Default behavior treats missings like `&` does:
#' pall(a, b)
#' a & b
#'
#' # Remove missings from the computation, like `na_rm = TRUE`:
#' pany(a, b, .na = FALSE)
#' (a & !is.na(a)) | (b & !is.na(b))
#'
#' pall(a, b, .na = TRUE)
#' (a | is.na(a)) & (b | is.na(b))
#'
#' # Check for missings in parallel:
#' pany_na(a, b) #> c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
#' pall_na(a, b) #> c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
#'
#' @export
pany <- function(..., .na = NA) {
  vctrs::vec_pany(..., .missing = .na)
}

#' @rdname pany
#' @export
pall <- function(..., .na = NA) {
  vctrs::vec_pall(..., .missing = .na)
}

#' @rdname pany
#' @export
pany_na <- function(..., .nan = FALSE) {
  vctrs::vec_pany(!!!lapply(list2(...), are_na2, nan = .nan))
}

#' @rdname pany
#' @export
pall_na <- function(..., .nan = FALSE) {
  vctrs::vec_pall(!!!lapply(list2(...), are_na2, nan = .nan))
}

# any_na <- function(x) {
#   any(are_na2(x))
# }

# all_na <- function(x) {
#   all(are_na2(x))
# }



# Functional -------------------------------------------------------------------

#' Reduce binary predicate over a list
#'
#' @description
#' This function applies a binary predicate function `.f` over a list `.l`,
#' checking if the result is `TRUE` for any or all ordered pairs within `.l`.
#'
#' `accumulate_predicate()` is similar, but returns a logical vector of the
#' accumulated results of the tests.
#'
#' @param .l \[`list()`] A list of objects to compare.
#' @param .f \[`function()`] A binary predicate function, that uses their first
#'   two arguments for the operation, and returns a single `TRUE` or `FALSE`.
#' @param ... Additional arguments passed to `.f`.
#' @param .op \[`"or"` | `"and"`] Whether to check if the predicate is `TRUE`
#'   for any or any or all ordered pairs within `.l`.
#'
#' @returns
#' - \[`TRUE` | `FALSE` | `NA`] For `reduce_predicate()`: the scalar reduced
#'   result.
#' - \[`logical(length(.l) - 1)`] For `accumulate_predicate()`: the accumulated
#'   results.
#'
#' @examples
#' reduce_predicate(list(1L, 1.0, 3), `==`, .op = "or") #> TRUE
#' reduce_predicate(list(1L, 1.0, 3), `==`, .op = "and") #> FALSE
#'
#' accumulate_predicate(list(1L, 1.0, 3), `==`, .op = "and") #> c(TRUE, FALSE)
#' accumulate_predicate(list(1L, 1.0, 3), identical, .op = "and") #> c(FALSE, FALSE)
#'
#' @export
reduce_predicate <- function(.l, .f, ..., .op = "or") {
  i <- 1
  n <- length(.l)

  if (.op == "or") {
    res <- FALSE
    while (! res && i <= n) {
      res <- .f(.l[[i]], .l[[i + 1]], ...)
      i <- i + 1
    }
  } else if (.op == "and") {
    res <- TRUE
    while (res && i <= n) {
      res <- .f(.l[[i]], .l[[i + 1]], ...)
      i <- i + 1
    }
  }

  res
}

#' @rdname reduce_predicate
#' @export
accumulate_predicate <- function(.l, .f, ..., .op = "or") {
  n <- length(.l)
  res <- logical(n - 1)

  if (.op == "or") {
    res[[1]] <- .f(.l[[1]], .l[[2]], ...)
    for (i in seq(2, n - 1)) {
      res[[i]] <- res[[i - 1]] || .f(.l[[i]], .l[[i + 1]], ...)
    }
  } else if (.op == "and") {
    res[[1]] <- .f(.l[[1]], .l[[2]], ...)
    for (i in seq(2, n - 1)) {
      res[[i]] <- res[[i - 1]] && .f(.l[[i]], .l[[i + 1]], ...)
    }
  }

  res
}
# TODO: wrap in user provided try catch
# NOTE: purrr has other predicate functionals at
# https://purrr.tidyverse.org/reference/index.html#predicate-functionals



# check_installed2 -------------------------------------------------------------

#' Check if user accepted a package installation
#'
#' @description
#' This function returns `TRUE` if `pkg` is already installed, and prompt the
#' user to install it otherwise. If the user accepts, returns `TRUE`, and
#' `FALSE` if he rejects. This is different from [rlang::check_installed()]
#' which will exit the function if the user rejects installation.
#'
#' Useful for suggesting the user a more featureful path of the function, while
#' still allowing a fallback path.
#'
#' Any installation errors, e.g. packages not found, still bubble up.
#'
#' @inheritParams rlang::check_installed
#'
#' @returns \[`TRUE` | `FALSE`] The scalar result of the test.
#'
#' @examples
#' # Safely use inside functions without exiting if the package is not installed:
#' f <- function(x) {
#'   if (check_installed2("crazy_print_package")) {
#'     # If the package is installed or the user chooses to install it
#'     # Some code such as `crazy_print_package::crazy_print(x)`
#'   } else {
#'     # If the package is not installed and the user refused to install it
#'     print(x)
#'   }
#' }
#'
#' @export
check_installed2 <- new_function(
  args = fn_fmls(check_installed),
  body = expr({
    tryCatch(
      withRestarts(
        {
          rlang::check_installed(!!!fn_fmls_syms(check_installed))
          TRUE
        },
        abort = \(cnd) FALSE
        # Catches invokeRestart("abort", cnd) when the user selects "No"
      ),
      error = \(cnd) {
        if (! inherits(cnd, "rlib_error_package_not_found")) {
          stop(cnd)
        }
        FALSE
      }
      # Other errors (e.g. from install.packages) bubble up. We can't use a
      # rlib_error_package_not_found handler because that would catch a pre-prompt
      # condition signaling in check_installed
    )
  }),
  env = ns_env("rlang")
)
