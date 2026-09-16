# Examples - assert_integer_like snapshot

    Code
      try(do.call(assert_integer_like, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(expr, envir) : object 'assert_integer_like' not found

# Examples - assert_double snapshot

    Code
      try(do.call(assert_double, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_double()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must pass `predicater::is_numeric()`.
      v (pass) len   : length must be in range 1 to 10.
      v (pass) n_na  : #of NA values must be 0.
      v (pass) n_dup : #of duplicate values must be 0.
      v (pass) n_nan : #of NaN values must be 0.
      v (pass) n_inf : #of Inf values must be 0.
      v (pass) range : must be in range 1 to 100.
      x (fail) set   : must be in a custom set. Was not.
      v (pass) sorted: must be in ascending order.
      
      i See `predicater::assert_double()` and this condition's `rs_assert_error` attribute for details.

