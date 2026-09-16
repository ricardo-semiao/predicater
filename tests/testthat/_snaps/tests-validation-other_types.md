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
      try(do.call(assert_function, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_function()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of type "closure".
      x (fail) args_names: must have argument names "a" and "b". Had "a", "b", and "...".
      v (pass) fn_env: must have the expected environment.
      v (pass) dots  : must accept `...`.
      
      i See `predicater::assert_function()` and this condition's `rs_assert_error` attribute for details.

