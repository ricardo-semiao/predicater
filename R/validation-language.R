
#' @include validation-helpers.R validation-menu.R
NULL

# CHECK: why do.call with a symbol/language arguments fails



# Expression -------------------------------------------------------------------

#' Validation - Expression vectors
#'
#' @description
#' Test if an object is an expression vector or expression object.
#'
#' `test_expression()` is the predicate test, while `assert_expression()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param len,n_null,n_call,n_sym,n_literal,n_invalid
#'   `r ROXY$x_n("len,n_null,n_call,n_sym,n_literal,n_invalid")`
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
  len = NULL, n_null = NULL, n_call = NULL, n_sym = NULL, n_literal = NULL,
  n_invalid = NULL,
  sentinels = NULL, custom = NULL, custom_map = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, len, n_null, n_call, n_sym, n_literal, custom, custom_map,
    tests_pars = list(l = length(x), x_list = as.list(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_expression2(x) %@@% c(type = typeof(x)),
      n_call = \(x, arg, pars) {
        n_call <- sum(vapply_lgl(pars$x_list, is_call))
        test_in_range(n_call, arg, pars$l) %@@% c(n = n_call)
      },
      n_sym = \(x, arg, pars) {
        n_sym <- sum(vapply_lgl(pars$x_list, is_symbol))
        test_in_range(n_sym, arg, pars$l) %@@% c(n = n_sym)
      },
      n_literal = \(x, arg, pars) {
        n_lit <- sum(vapply_lgl(pars$x_list, is_syntactic_literal))
        test_in_range(n_lit, arg, pars$l) %@@% c(n = n_lit)
      },
      n_invalid = \(x, arg, pars) {
        n_invalid <- sum(vapply_lgl(pars$x_list, \(x) !is_parseable(x)))
        test_in_range(n_invalid, arg, pars$l) %@@% c(n = n_invalid)
      }
    )
  )
}

#' @rdname test_expression
#' @export
test_expression <- fn_core_to_test(core_expression)

#' @rdname test_expression
#' @export
assert_expression <- fn_core_to_assert(
  core_expression,
  msgs_add = list(
    type = \(attrs) "must be an expression.",
    n_call = \(attrs) "count of call elements does not fall within the expected range.",
    n_sym = \(attrs) "count of symbol elements does not fall within the expected range.",
    n_literal = \(attrs) "count of literal elements does not fall within the expected range."
  )
)



# Symbol -----------------------------------------------------------------------

#' Validation - Symbol
#'
#' @description
#' Test if an object is a symbol (name).
#'
#' `test_symbol()` is the predicate test, while `assert_symbol()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param n_char `r ROXY$x_n("n_char")`
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
#' @examples
#' x <- quote(my_var)
#'
#' args <- list(
#'   n_char = c(1, 10),      # Symbol string length must be between 1 and 10 (will pass)
#'   valid = TRUE,           # Must be a valid syntactic R name (will pass)
#'   empty = FALSE,          # Symbol must not be the empty symbol (will pass)
#'   env_has = rlang::global_env(),
#'   # Symbol must exist directly in global environment (will fail)
#'   env_seen = NULL,        # Don't test environment inheritance
#'   sentinels = c("null"),  # Allow NULL symbol (not the case of x)
#'   custom = NULL           # No custom test predicate
#' )
#'
#' rlang::exec(test_symbol, !!!c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(rlang::exec(assert_symbol, !!!c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_symbol
NULL

core_symbol <- function(
  x,
  n_char = NULL, valid = NULL, empty = NULL, env_has = NULL, env_seen = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  sym_str <- if (is_symbol(x)) as_string(x) else NULL

  run_tests(
    x, sentinels, n_char, valid, env_has, env_seen, custom,
    tests_pars = list(sym_str = sym_str), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_symbol(x) %@@% c(type = typeof(x)),
      n_char = \(x, arg, pars) {
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
        TESTS_MENU$env_has(arg, pars$sym_str) # Oposite order from test_env
      },
      env_seen = \(x, arg, pars) {
        TESTS_MENU$env_seen(arg, pars$sym_str)
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
    n_char = \(attrs) "character count does not fall within the expected range.",
    valid = \(attrs) "is not a valid syntactic R name."
  )
)



# Call -------------------------------------------------------------------------

#' Validation - Language
#'
#' @description
#' Test if an object is a call (language object).
#'
#' `test_language()` is the predicate test, while `assert_language()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param name,ns \[`character(1)` | `NULL`] Expected function name and
#'   namespace of the call, via [rlang::call_name()] and [rlang::call_ns()]. Set
#'   to `NULL` to not test.
#' @param n_args `r ROXY$x_n("n_args")`
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
#' @examples
#' x <- quote(rlang::env(a = 1, b = 2))
#'
#' args <- list(
#'   name = "fn",             # Function name must be "fn" (will pass)
#'   ns = "otherpkg",         # Namespace must be "otherpkg" (will fail)
#'   n_args = c(1, 5),        # Number of arguments must be between 1 and 5 (will pass)
#'   arg_names = c("a", "b"), # Argument names must match exact character vector (will pass)
#'   simple = FALSE,          # Must not be a simple call without names/namespace (will pass)
#'   valid = TRUE,            # Must be a parseable, valid call (will pass)
#'   sentinels = c("null"),   # Allow NULL call (not the case of x)
#'   custom = NULL            # No custom test predicate
#' )
#'
#' rlang::exec(test_language, !!!c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(rlang::exec(assert_language, !!!c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_language
NULL

core_language <- function(
  x,
  name = NULL, ns = NULL, n_args = NULL, arg_names = NULL, simple = NULL, valid = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, name, ns, n_args, arg_names, simple, valid, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_language(x) %@@% c(type = typeof(x)),
      name = \(x, arg, pars) identical(call_name(x), arg),
      ns = \(x, arg, pars) identical(call_ns(x), arg),
      n_args = \(x, arg, pars) {
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
    n_args = \(attrs) "argument count does not fall within the expected range.",
    arg_names = \(attrs) "argument names do not match expected values.",
    simple = \(attrs) "simple call check failed.",
    valid = \(attrs) "call parseability check failed."
  )
)



# Code -------------------------------------------------------------------------

#' Validation - Code Objects
#'
#' @description
#' Test if an input is a R language code object (symbol, language/call, or
#' syntactic literal), and optionally check if it is valid parseable code or
#' an empty symbol.
#'
#' `test_code()` is the predicate test, while `assert_code()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param sym,lang,literal \[`TRUE` | `FALSE` | `NULL`] Whether to allow
#' symbols, language objects (calls), or syntatic literals.
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
#' @examples
#' x <- 42L
#'
#' args <- list(
#'   sym = FALSE,            # Disallow symbols (will pass)
#'   lang = FALSE,           # Disallow language objects/calls (will pass)
#'   literal = TRUE,         # Allow syntactic literals (will pass)
#'   valid = TRUE,           # Must be parseable code (will pass)
#'   empty = FALSE,          # Disallow empty symbols (will pass)
#'   sentinels = c("null"),  # Allow NULL code object (not the case of x)
#'   custom = \(x) x > 100   # Literal value must be > 100 (will fail)
#' )
#'
#' rlang::exec(test_code, !!!c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(rlang::exec(assert_code, !!!c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_code
NULL

core_code <- function(
  x,
  sym = TRUE, lang = TRUE, literal = TRUE,
  valid = NULL, empty = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, valid, empty, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) is_code(x, sym, lang, literal) %@@% c(type = typeof(x)),
      valid = \(x, arg, pars) {
        is_code(x, sym, lang, literal, valid = TRUE) == arg
      },
      empty = \(x, arg, pars) {
        if (is_symbol(x)) {
          identical(x, expr()) == arg
        } else {
          TRUE
        }
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
