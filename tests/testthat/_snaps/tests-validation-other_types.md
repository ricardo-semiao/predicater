# Examples - assert_null snapshot

    Code
      try(assert_null(x))
    Output
      Error in eval(code, test_env) : `x` failed `assert_null()`:
      v (pass) sentinels: no sentinel values allowed.
      x (fail) type  : must be "NULL". Had type "integer".
      
      i See `predicater::assert_null()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_function snapshot

    Code
      try(assert_function(x, !!!args, short_circuit = FALSE))
    Output
      Error in !args : invalid argument type

