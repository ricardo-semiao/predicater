
# check_installed2 ----------------------------------------------------------

args_check_installed <- set_names(syms(fn_fmls_names(check_installed)))
names(args_check_installed)[names(args_check_installed) == "..."] <- ""

#' Check if the user accepted a package installation
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
#'     # If the package is the user refused to install it
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
          rlang::check_installed(!!!args_check_installed)
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
