
#' @include tests-helpers.R tests-menu.R
NULL



# Expression -------------------------------------------------------------------

#' Tests - Expression
#'
#' @description
#' Test if an object is an expression vector or expression object.
#'
#' `test_expression()` is the predicate test, while `assert_expression()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param len,null_n,call_n,sym_n,literal_n,invalid_n
#'   `r ROXY$x_n("len,null_n,call_n,sym_n,literal_n,invalid_n")`
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_map `r ROXY$custom_map()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("expression")`
#'
#' @name test_expression
NULL

core_expression <- function(
  x,
  len = NULL, null_n = NULL, call_n = NULL, sym_n = NULL, literal_n = NULL, invalid_n = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, null_n, call_n, sym_n, literal_n, custom, custom_map,
    tests_pars = list(l = length(x), x_list = as.list(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_expression2(x) %@@% c(type = typeof(x)),
      call_n = \(x, arg, pars) {
        n_call <- sum(vapply_lgl(pars$x_list, is_call))
        test_in_range(n_call, arg, pars$l) %@@% c(n = n_call)
      },
      sym_n = \(x, arg, pars) {
        n_sym <- sum(vapply_lgl(pars$x_list, is_symbol))
        test_in_range(n_sym, arg, pars$l) %@@% c(n = n_sym)
      },
      literal_n = \(x, arg, pars) {
        n_lit <- sum(vapply_lgl(pars$x_list, is_syntactic_literal))
        test_in_range(n_lit, arg, pars$l) %@@% c(n = n_lit)
      },
      invalid_n = \(x, arg, pars) {
        n_invalid <- sum(vapply_lgl(pars$x_list, \(x) !is_parseable(x)))
        test_in_range(n_invalid, arg, pars$l) %@@% c(n = n_invalid)
      }
    )
  )
}
# TODO: invalid_n (unparseable)?

#' @rdname test_expression
#' @export
test_expression <- fn_core_to_test(core_expression)

#' @rdname test_expression
#' @export
assert_expression <- fn_core_to_assert(
  core_expression,
  msgs_add = list(
    type = \(attrs) "must be an expression.",
    call_n = \(attrs) "count of call elements does not fall within the expected range.",
    sym_n = \(attrs) "count of symbol elements does not fall within the expected range.",
    literal_n = \(attrs) "count of literal elements does not fall within the expected range."
  )
)



# Symbol -----------------------------------------------------------------------

#' Tests - Symbol
#'
#' @description
#' Test if an object is a symbol (name).
#'
#' `test_symbol()` is the predicate test, while `assert_symbol()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param char_n `r ROXY$x_n("char_n")`
#' @param valid \[`TRUE` | `FALSE` | `NULL`] Test if the symbol name is a valid
#'   syntactic R name (i.e. unchanged when processed by [make.names()]).
#'   Set to `NULL` to not test.
#' @param empty \[`TRUE` | `FALSE` | `NULL`] Test if the symbol is empty (i.e.
#'   `""`). Set to `NULL` to not test.
#' @param env_has,env_seen \[`environment` | `NULL`] Environment in which the
#'   symbol exists directly, or inherited from one of its parents, respectively
#'   (see [rlang::env_has()]). Set to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("symbol")`
#'
#' @name test_symbol
NULL

core_symbol <- function(
  x,
  char_n = NULL, valid = NULL, empty = NULL, env_has = NULL, env_seen = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  sym_str <- if (is_symbol(x)) as_string(x) else NULL

  run_tests(
    x, sentinels, char_n, valid, env_has, env_seen, custom,
    tests_pars = list(sym_str = sym_str), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_symbol(x) %@@% c(type = typeof(x)),
      char_n = \(x, arg, pars) {
        n_char <- nchar(pars$sym_str)
        test_in_range(n_char, arg, n_char) %@@% c(n = n_char)
      },
      valid = \(x, arg, pars) {
        (make.names(pars$sym_str) == pars$sym_str) == arg
      },
      empty = \(x, arg, pars) {
        (pars$sym_str == "") == arg
      },
      env_has = \(x, arg, pars) {
        TESTS_MENU$test_env_has(arg, pars$sym_str, inherit = FALSE)
      },
      env_seen = \(x, arg, pars) {
        TESTS_MENU$test_env_has(arg, pars$sym_str, inherit = TRUE)
      }
    )
  )
}

#' @rdname test_symbol
#' @export
test_symbol <- fn_core_to_test(core_symbol)

#' @rdname test_symbol
#' @export
assert_symbol <- fn_core_to_assert(
  core_symbol,
  msgs_add = list(
    type = \(attrs) "must be a symbol.",
    char_n = \(attrs) "character count does not fall within the expected range.",
    valid = \(attrs) "is not a valid syntactic R name."
  )
)



# Call -------------------------------------------------------------------------

#' Tests - Language
#'
#' @description
#' Test if an object is a call (language object).
#'
#' `test_language()` is the predicate test, while `assert_language()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param name,ns \[`character(1)` | `NULL`] Expected function name and
#'   namespace of the call, via [rlang::call_name()] and [rlang::call_ns()]. Set
#'   to `NULL` to not test.
#' @param args_n `r ROXY$x_n("args_n")`
#' @param arg_names \[`character()` | `NULL`] Expected exact names of the call
#'   arguments. Set to `NULL` to not test.
#' @param simple \[`TRUE` | `FALSE` | `NULL`] Test if the call is simple via
#'   [rlang::is_call_simple()]. Set to `NULL` to not test.
#' @param valid \[`TRUE` | `FALSE` | `NULL`] Test if the call is parseable. Set
#'   to `NULL` to not test.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("language")`
#'
#' @name test_language
NULL

core_language <- function(
  x,
  name = NULL, ns = NULL, args_n = NULL, arg_names = NULL, simple = NULL, valid = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, name, ns, args_n, arg_names, simple, valid, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_language(x) %@@% c(type = typeof(x)),
      name = \(x, arg, pars) identical(call_name(x), arg),
      ns = \(x, arg, pars) identical(call_ns(x), arg),
      args_n = \(x, arg, pars) {
        n_args <- length(x) - 1L
        test_in_range(n_args, arg, n_args) %@@% c(n = n_args)
      },
      arg_names = \(x, arg, pars) {
        identical(names(call_args(x)), arg)
      },
      simple = \(x, arg, pars) is_call_simple(x) == arg,
      valid = \(x, arg, pars) is_parseable(x) == arg
    )
  )
}

#' @rdname test_language
#' @export
test_language <- fn_core_to_test(core_language)

#' @rdname test_language
#' @export
assert_language <- fn_core_to_assert(
  core_language,
  msgs_add = list(
    type = \(attrs) "must be a language object (call).",
    name = \(attrs) "function name does not match expected value.",
    ns = \(attrs) "namespace does not match expected value.",
    args_n = \(attrs) "argument count does not fall within the expected range.",
    arg_names = \(attrs) "argument names do not match expected values.",
    simple = \(attrs) "simple call check failed.",
    valid = \(attrs) "call parseability check failed."
  )
)



# Code -------------------------------------------------------------------------

#' Tests - Code Objects
#'
#' @description
#' Test if an input is a R language code object (symbol, language/call, or
#' syntactic literal), and optionally check if it is valid parseable code or
#' an empty symbol.
#'
#' `test_code()` is the predicate test, while `assert_code()` validates
#' its input, aborting if it fails the test.
#'
#' @param x \[`any`] An object to test.
#' @param sym,lang,literal \[`TRUE` | `FALSE` | `NULL`] Whether to allow symbols, language objects (calls), or syntatic literals.
#' @param valid \[`logical(1)` | `NULL`] Whether the code object must be
#'   parseable code.
#' @param empty \[`logical(1)` | `NULL`] Whether empty symbols are permitted.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("code")`
#'
#' @name test_code
NULL

core_code <- function(
  x,
  sym = TRUE, lang = TRUE, literal = TRUE,
  valid = NULL, empty = NULL,
  sentinels = NULL, custom = NULL
) {
  run_tests(
    x, sentinels, valid, empty, custom,
    tests_pars = list(), short = TRUE,
    menu_add = list(
      type = \(x, arg, pars) is_code(x, sym, lang, literal) %@@% c(type = typeof(x)),
      valid = \(x, arg, pars) {
        is_code(x, sym, lang, literal, valid = TRUE) == arg
      },
      empty = \(x, arg, pars) {
        is_code(x, sym, lang, literal, empty = TRUE) == arg
      }
    )
  )
}

#' @rdname test_code
#' @export
test_code <- fn_core_to_test(core_code)

#' @rdname test_code
#' @export
assert_code <- fn_core_to_assert(
  core_code,
  msgs_add = list(
    type = \(attrs) glue("had type `{attrs$type}`, which is not valid language code."),
    valid = \(attrs) "code validity check failed.",
    empty = \(attrs) "empty symbol status does not match expected setting."
  )
)
