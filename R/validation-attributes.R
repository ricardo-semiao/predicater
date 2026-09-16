
#' @include validation-helpers.R validation-menu.R
NULL

# NOTE: the type test could be !is_null(), but this would allow how = x to pass
# a weird object



# Names ------------------------------------------------------------------------

#' Validation - Names attribute
#'
#' @description
#' Test if an object's names (or character vector of names) satisfies some
#' conditions.
#'
#' `test_names()` is the predicate test, while `assert_names()` validates
#' its input, aborting if it fails the test.
#'
#' Hint: use `sentinels = c("null")` to allow no (`NULL`) names, and `empty =
#' TRUE` to always pass the test if the underlying vector `x` is empty.
#'
#' @param x \[`any`] An object to get names from, or a character vector of names
#'   to test.
#' @param n_na,n_dup,n_empty,n_invalid `r ROXY$x_n("n_na,n_dup,n_empty,n_invalid")`
#' @param set `r ROXY$set("character")`
#' @param tests_char \[`list`] A list of additional arguments passed to
#'   [test_character()].
#' @param how \[`"names"` | `"x"` | `"attr"` | `"colnames"` | `"row.names"` |
#'   `integer(1)`]
#'   How to extract names from `x`: `"x"` for `x` directly; `"names"` for
#'   `names(x)`; `"attr"` for `attr(x, "names")`; `"colnames"` for
#'   `colnames(x)`; `"row.names"` for `attr(x, "row.names")`; or a positive
#'   integer for `dimnames(x)[[how]]`.
#' @param empty \[`TRUE` | `NULL`] Whether to early pass the test if the
#'   underlying vector `x` is empty.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("names")`
#'
#' @examples
#' x <- rlang::set_names(1:6, c("a", "b", "c", NA, "", ""))
#'
#' args <- list(
#'   n_na = 0,              # No NA names (will fail)
#'   n_dup = NULL,          # Don't test for duplicates
#'   n_empty = c(0, -1),    # Between 0 and length(x) - 1 empty names (will pass)
#'   n_invalid = c(0, Inf), # Between 0 and Inf invalid names (same as not testing)
#'   set = list(yes = c("a", "b"), no = c("d", "e")),
#'   # Names must be only "a" or "b", and not "d" nor "e" (will fail)
#'   how = "names",         # Use `names(x)` as the names vector to test
#'   empty = NULL,          # Don't allow empty `x` (will pass)
#'   sentinels = c("null"), # Allow `NULL` names (not the case of x)
#'   custom = \(x) isTRUE(all(nchar(x) == 1))
#'   # All names must be a single character (will fail)
#' )
#'
#' do.call(test_names, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_names, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_names
NULL

core_names <- function(
  x,
  n_na = NULL, n_empty = NULL, n_dup = NULL, n_invalid = NULL,
  set = NULL, tests_char = NULL, how = "names",
  empty = NULL, sentinels = NULL, custom = NULL,
  short_circuit = TRUE
) {
  x <- switch(how,
    names = names(x),
    x = x,
    attr = attr(x, "names", TRUE),
    colnames = colnames(x),
    row.names = attr(x, "row.names", TRUE),
    dimnames(x)[[as.integer(how)]]
  )

  run_tests(
    x, sentinels, empty, n_na, n_empty, n_dup, n_invalid, set, tests_char, custom,
    tests_pars = list(l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        is_character(x) %@@% list(type = typeof(x))
      },
      n_empty = \(x, arg, pars) {
        n_empty <- sum(x == "", na.rm = TRUE)
        test_in_range(n_empty, arg, pars$l) %@@%
          list(arg = arg, n = n_empty)
      },
      n_invalid = \(x, arg, pars) {
        n_invalid <- sum(make.names(x) != x, na.rm = TRUE)
        test_in_range(n_invalid, arg, pars$l) %@@%
          list(arg = arg, n = n_invalid)
      },
      tests_char = \(x, arg, pars) {
        exec(test_character, x, !!!arg) %@@%
          list(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      },
      empty = \(x, arg, pars) {
        is_empty2(x)
      }
    )
  )
}
# TODO: cite sentinels NULL and empty
# TODO: order would be nice. set doesnt support it
# WARN: if x is empty and short_circuit = FALSE, we might get errors. Maybe
# create a menu_short arg to run_tests that always short_circuits (type and
# sentinels there, instead of manually). Then, lets generalize the idea of a non
# failable test, maybe one that returns NA. Ideas are good, but: if x is empty,
# names are null, and we short-circuit in type test

#' @rdname test_names
#' @export
test_names <- fn_core_to_test(core_names)

#' @rdname test_names
#' @export
assert_names <- fn_core_to_assert(
  core_names,
  msgs_add = list(
    type = \(attrs, test) {
      glue2(
        "names must be of type {.val character}.",
        fmt_postfix("Had type {.val [attrs$type]}.", test)
      )
    },
    empty = \(attrs, test) {
      glue2("vector can be empty.") # Non-failable test
    },
    n_empty = msg_n_arg("#of empty string names"),
    n_invalid = msg_n_arg("#of syntactically invalid names"),
    tests_char = \(attrs, test) {
      ts <- if (length(attrs$tests) == 1) "test" else "tests"
      glue2(
        "names must pass custom {.fn predicater::test_names} [ts].",
        fmt_postfix("Failed [fmt_vec(attrs$tests)].", test)
      )
    }
  )
)
# CHECK: pass how via %@@% to be used in messages? Doesn't seem easy to inform



# Dimensions -------------------------------------------------------------------

#' Validation - Matrix and array attributes
#'
#' @description
#' Test if an object has matrix/array-related attributes that pass some
#' conditions.
#'
#' `test_matrix()` is a predicate test, while `assert_matrix()` validates their
#' input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param n_dims `r ROXY$x_n("n_dims")`
#' @param dims_shape \[`list()` | `integer()` | `NULL`] Expected size
#'   constraints for each dimension. Can be a vector of dimension sizes or a
#'   list of range specs (as for `n_dims`). Set to `NULL` to not test.
#' @param names_apply \[`list()` | `NULL`]
#'   A list of arguments passed to [test_names()] to test each dimension's
#'   names. For separate tests for each dimension, use a list of formulas, with
#'   the LHS being the dimension integer index, and the RHS being the list of
#'   arguments to `test_names()`. An empty list() test for the presence of names.
#' @param how \[`"dim"` | `"x"` | `"attr"`] How to extract dimensions from `x`:
#'   `"dim"` for [dim()]; `"x"` ofr `x` directly `"attr"` for `attr(x, "dim")`.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param custom_apply \[`list()` | `NULL`] A list of formulas. For each margin
#'   in the LHS (as in `MARGIN` in [apply()]), test the function in the RHS
#'   across that margin.
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns()`
#'
#' @examples
#' x <- matrix(
#'   1:6, nrow = 2, ncol = 3,
#'   dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
#' )
#'
#' args <- list(
#'   n_dims = 2, # Must be exactly 2-dimensional (will pass)
#'   dims_shape = list(2, c(1, Inf)),
#'   # 2 rows, and cols between 1 and Inf (will pass)
#'   names_apply = list(
#'     1 ~ list(n_na = 0, n_dup = 0),
#'     # Row names must have no NAs or duplicates (will pass)
#'     2 ~ list(set = list(no = c("c4")))
#'     # Column names must not contain "c4" (will pass)
#'   ),
#'   how = "dim",                # Use `dim(x)` to extract dimensions (will pass)
#'   sentinels = c("null"),      # Allow NULL x (not the case of x)
#'   custom = \(x) is.matrix(x), # Must be a standard matrix (will pass)
#'   custom_apply = list(1 ~ \(row) sum(row) > 10)
#'   # Sum of elements across each row must exceed 10 (will fail)
#' )
#'
#' do.call(test_matrix, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_matrix, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_matrix
NULL

core_matrix <- function(
  x,
  n_dims = NULL, dims_shape = NULL, names_apply = NULL,
  how = "dim",
  sentinels = NULL, custom = NULL, custom_apply = NULL,
  short_circuit
) {
  dims <- switch(how,
    dim = dim(x),
    x = x,
    attr = attr(x, "dim", exact = TRUE)
  )

  run_tests(
    x, sentinels, n_dims, dims_shape, names_apply, custom, custom_apply,
    tests_pars = list(dims = dims, l = length(x)), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        is_integer(pars$dims) %@@% list(type = typeof(pars$dims))
        # TODO: maybe use is_integer_like for more leeway
      },
      n_dims = \(x, arg, pars) {
        n_dims <- length(pars$dims)
        test_in_range(n_dims, arg, pars$l) %@@% list(arg = arg, n = n_dims)
      },
      dims_shape = \(x, arg, pars) {
        res <- logical(length(arg))
        for (i in seq_along(arg)) {
          res[i] <- test_in_range(pars$dims[i], arg[[i]], pars$l)
        }
        all(res) %@@% list(dims = which(! res))
      },
      names_apply = \(x, arg, pars) {
        dimnames <- dimnames(x)

        if (! is_formula(arg[[1]])) {
          res <- logical(length(dimnames))
          for (i in seq_along(dimnames)) {
            res[i] <- exec(test_names, x = dimnames[[i]], !!!arg)
          }
        } else {
          res <- logical(length(arg))
          for (i in seq_along(arg)) {
            margin <- eval(f_lhs(arg[[i]]))
            args <- eval(f_rhs(arg[[i]]))
            res[i] <- exec(test_names, x = dimnames[[margin]], !!!args)
          }
        }
        all(res) %@@% list(dims = which(! res))
      },
      custom_apply = \(x, arg, pars) {
        res <- logical(length(arg))
        margins <- integer(length(arg))
        for (i in seq_along(arg)) {
          margins[i] <- eval(f_lhs(arg[[i]]))
          fun <- eval(f_rhs(arg[[i]]))
          res[i] <- all(apply(x, margins[i], \(x) test_custom(x, fun)))
        }
        all(res) %@@% list(margin = margins[which(! res)])
      }
    )
  )
}


#' @rdname test_matrix
#' @export
test_matrix <- fn_core_to_test(core_matrix)


#' @rdname test_matrix
#' @export
assert_matrix <- fn_core_to_assert(
  core_matrix,
  msgs_add = list(
    type = \(attrs, test) {
      glue2(
        "dim must be of type {.val integer}.",
        fmt_postfix("Had type {.val [attrs$type]}.", test)
      )
    },
    n_dims = msg_n_arg("#of dimensions"),
    dims_shape = \(attrs, test) {
      ds <- if (length(attrs$dims) == 1) "dimension" else "dimensions"
      glue2(
        "each dimension size must be in custom range.",
        fmt_postfix("Failed for [ds] [fmt_vec(attrs$dims)].", test)
      )
    },
    names_apply = \(attrs, test) {
      ds <- if (length(attrs$dims) == 1) "dimension" else "dimensions"
      glue2(
        "each dimension names must pass custom {.fn predicater::test_names} test.",
        fmt_postfix("Failed for [ds] [fmt_vec(attrs$dims)].", test)
      )
    },
    custom_apply = \(attrs, test) {
      ms <- if (length(attrs$margin) == 1) "margin" else "margins"
      glue2(
        "must pass custom tests along some margins.",
        fmt_postfix("Failed for [ms] {.val {[attrs$margin]}}.", test)
      )
    }
  )
)



# Classes ----------------------------------------------------------------------

#' Validation - Class attribute
#'
#' @description
#' Test if an object inherits from specific classes or has a valid class vector.
#'
#' Invalid classes fail the test: non-character vectors, empty character
#' vectors, or a vector with `NA` values.
#'
#' `test_class()` is the predicate test, while `assert_class()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param classes \[`list()` | `character()` | `NULL`]
#'   A named list specifying possible class inheritance criterias. The elements
#'   are the classes to test against, and the names are which test to do:
#'   `"any"` for [rlang::inherits_any()], `"all"` for [rlang::inherits_all()],
#'   `"only"` for [rlang::inherits_only()], and `"none"` for `!inherits_any()`.
#'   If any of the test passes, the overall test passes. If a single character,
#'   it is tested with `inherits_any()`. Set to `NULL` to not test.
#' @param tests_char \[`list`] A list of additional character tests passed to
#'   [test_character()].
#' @param how \[`"class"` | `"x"` | `"attr"`] How to extract class names for
#'   `tests_char`: `"x"` for `x` directly; `"class"` for [class()]; and `"attr"`
#'   for `attr(x, "class")`.
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("class")`
#'
#' @examples
#' x <- structure(
#'   list(a = 1),
#'   class = c("another_class", "custom_df", "data.frame")
#' )
#'
#' args <- list(
#'   classes = list(
#'     all = c("custom_df", "data.frame"),
#'     none = "matrix"
#'   ),
#'   # Must inherit from both custom_df and data.frame, and not matrix (will pass)
#'   tests_char = list(n_na = 0, n_dup = 0),
#'   # Class names vector must contain no NAs or duplicates (will pass)
#'   how = "class",         # Extract class vector via `class(x)` (will pass)
#'   sentinels = c("null"), # Allow NULL class attribute (not the case of x)
#'   custom = \(x) has_dim(x)
#'   # Object must have a dim() value (will fail)
#' )
#'
#' do.call(test_class, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_class, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_class
NULL

core_class <- function(
  x,
  classes = NULL, tests_char = NULL, how = "class",
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  x <- switch(how,
    class = class(x),
    x = x,
    attr = attr(x, "class", exact = TRUE)
  )

  run_tests(
    x, sentinels, classes, tests_char, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        test_character(x, len = c(1, Inf), n_na = 0) %@@% list(c = x)
      },
      classes = \(x, arg, pars) {
        if (is_character(arg)) {
          return(inherits_any(x, arg) %@@% list(arg = arg))
        }

        res <- logical(length(arg))
        hows <- names(arg)
        for (i in seq_along(arg)) {
          res[i] <- switch(hows[i],
            any = inherits_any(x, arg[[i]]),
            all = inherits_all(x, arg[[i]]),
            only = inherits_only(x, arg[[i]]),
            none = !inherits_any(x, arg[[i]])
          )
        }
        any(res) %@@% list(c = x)
      },
      tests_char = \(x, arg, pars) {
        exec(test_character, x, !!!arg) %@@%
          list(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      }
    )
  )
}

#' @rdname test_class
#' @export
test_class <- fn_core_to_test(core_class)

#' @rdname test_class
#' @export
assert_class <- fn_core_to_assert(
  core_class,
  msgs_add = list(
    type = \(attrs, test) {
      glue2(
        "class must be a non-empty no-na character vector.",
        fmt_postfix("Had class [fmt_vec(attrs$c)].", test)
      )
    },
    classes = \(attrs, test) {
      glue2(
        "class must satisfy class inheritance constraints.",
        fmt_postfix("Had class [fmt_vec(attrs$c)].", test)
      )
    },
    tests_char = \(attrs, test) {
      ts <- if (length(attrs$tests) == 1) "test" else "tests"
      glue2(
        "class must pass the specified character [ts].",
        fmt_postfix("Failed [fmt_vec(attrs$tests)].", test)
      )
    }
  )
)



# Objects ----------------------------------------------------------------------

#' Validation - Object-related attributes
#'
#' @description
#' Test if an input is an 'object' (see [predicates-objects]), and fits a 
#' specific Object-Oriented (OO) system (see [object_system()]).
#'
#' `test_object()` is the predicate test, while `assert_object()` validates
#' its input, aborting if it fails the test.
#'
#' @param x `r ROXY$x()`
#' @param oo_system \[`character(1)` | `NULL`] Expected Object-Oriented system
#'   name, passed to [object_system()]. Set to `NULL` to not test.
#' @param s4_bit \[`TRUE` | `FALSE` | `NULL`] Whether the underlying S4 object
#'   bit must (`TRUE`) or musnt't be set. Set to `NULL` to not test.
#' @param tests_class \[`list`] A list of additional class tests passed directly
#'   to [test_class()].
#' @param sentinels `r ROXY$sentinels()`
#' @param custom `r ROXY$custom()`
#' @param action `r ROXY$action()`
#' @param env `r ROXY$env()`
#' @param x_name `r ROXY$x_name()`
#' @param short_circuit `r ROXY$short_circuit()`
#' @param report_untested `r ROXY$report_untested()`
#' @param args_cnd `r ROXY$args_cnd()`
#'
#' @returns `r ROXY$test_returns("object")`
#'
#' @examples
#' x <- structure(
#'   list(a = 1),
#'   class = c("my_s3_class")
#' )
#'
#' args <- list(
#'   oo_system = "S3",  # Must be an S3 object (will pass)
#'   s4_bit = FALSE,    # Object S4 bit must not be set (will pass)
#'   tests_class = list(
#'     classes = list(all = c("my_s3_class", "another_class"))
#'   ),
#'   # Class vector must contain all specified classes (will fail)
#'   sentinels = c("null"),    # Allow NULL/unclassed objects (not the case of x)
#'   custom = \(x) is_list(x)  # Underlying object structure must be a list (will pass)
#' )
#'
#' do.call(test_object, c(list(x), args)) #> FALSE (not all tests passed)
#'
#' try(do.call(assert_object, c(list(x), args, short_circuit = FALSE))) #> Error
#'
#' @name test_object
NULL

core_object <- function(
  x,
  oo_system = NULL, s4_bit = NULL, tests_class = NULL,
  sentinels = NULL, custom = NULL,
  short_circuit
) {
  run_tests(
    x, sentinels, oo_system, s4_bit, tests_class, custom,
    tests_pars = list(), short = short_circuit,
    menu_add = list(
      type = \(x, arg, pars) {
        is_object_like(x, bad = "false")
      },
      oo_system = \(x, arg, pars) {
        sys <- object_system(x)
        (arg == sys) %@@% list(arg = arg, system = sys)
      },
      s4_bit = \(x, arg, pars) {
        bit <- has_s4_bit(x)
        (bit == arg) %@@% list(arg = arg, bit = bit)
      },
      tests_class = \(x, arg, pars) {
        exec(test_class, x = x, !!!arg) %@@%
          list(tests = names(arg)[vapply_lgl(arg, \(p) !is_null(p))])
      }
    )
  )
}
# TODO: allow an any-like test in oo_system
# CHECK: consider adding s4_type, object_type tests

#' @rdname test_object
#' @export
test_object <- fn_core_to_test(core_object)

#' @rdname test_object
#' @export
assert_object <- fn_core_to_assert(
  core_object,
  msgs_add = list(
    type = \(attrs, test) {
      glue2(
        "must have a consistent class (see {.fn predicater::is_object_like}).",
        fmt_postfix("Did not.", test)
      )
    },
    oo_system = \(attrs, test) {
      glue2(
        "must belong to the {.val [attrs$arg]} object system.",
        fmt_postfix("Was from {.val [attrs$system]}.", test)
      )
    },
    s4_bit = \(attrs, test) {
      glue2(
        "S4 bit must be {.val [attrs$arg]}.",
        fmt_postfix("Was {.val [attrs$bit]}.", test)
      )
    },
    tests_class = \(attrs, test) {
      ts <- if (length(attrs$tests) == 1) "test" else "tests"
      glue2(
        "must pass the custom {.fn predicater::test_class} [ts].",
        fmt_postfix("Failed [fmt_vec(attrs$tests)].", test)
      )
    }
  )
)
