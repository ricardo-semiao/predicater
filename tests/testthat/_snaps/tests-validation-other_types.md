# Examples - assert_null snapshot

    Code
      try(assert_null(x))
    Output
      Error in eval(code, test_env) : `x` failed `assert_null()`:
      v sentinels: ok.
      x type: `integer` is not `NULL`
      
      i See this condition's `rs_assert_error` attribute for details.

