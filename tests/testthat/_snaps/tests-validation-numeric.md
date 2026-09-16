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
      v sentinels: ok.
      v type: ok.
      v len: ok.
      v n_na: ok.
      v n_dup: ok.
      v n_nan: ok.
      v n_inf: ok.
      v range: ok.
      x set: had values outside of the allowed set.
      v sorted: ok.
      
      i See this condition's `rs_assert_error` attribute for details.

