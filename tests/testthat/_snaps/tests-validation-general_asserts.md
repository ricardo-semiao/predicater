# Simple msg function

    Code
      try(assert_from_msg(msg_fun, x))
    Output
      Error in eval(code, test_env) : 
        Error with argument `x`: x must be numeric and have no missing values
      i See this condition's `rs_assert_from_error` attribute for details.

# Simple error function

    Code
      try(assert_from_error(err_fun, x))
    Output
      Error in eval(code, test_env) : 
        Error with argument `x`: x must be numeric and have no missing values
      i See this condition's `rs_assert_from_error` attribute for details.

# Examples - assert_ptype snapshot

    Code
      try(assert_ptype(numeric(), "a"))
    Output
      Error in eval(code, test_env) : 
        Argument `a` is not of prototype `numeric()`.
      i See this condition's `rs_assert_ptype_error` attribute for details.

# Examples - assert_predicate snapshot

    Code
      try(assert_predicate(function(x) all(x > 0), c(-1, 0, 1)))
    Output
      Error in eval(code, test_env) : Argument `c(-1, 0, 1)` fails `fun`.
      i See this condition's `rs_assert_predicate_error` attribute for details.

