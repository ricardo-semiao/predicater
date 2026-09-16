# Examples - assert_list snapshot

    Code
      try(do.call(assert_list, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_list()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of type "list".
      v (pass) len   : length must be in range 1 to 10.
      x (fail) n_null: #of NULL values must be 0. Was 1.
      x (fail) n_empty: #of empty values must be in range 0 to 1. Was 2.
      x (fail) n_dup : #of duplicate values must be 0. Was 1.
      v (pass) custom_map: all elements must pass a custom test.
      
      i See `predicater::assert_list()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_environment snapshot

    Code
      try(do.call(assert_environment, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_environment()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of type "environment".
      v (pass) len   : length must be in range 1 to 5.
      x (fail) namespace:
      v (pass) parents: must be child of specific parents.
      v (pass) env_has: must contain "a" and "b".
      x (fail) env_sees: must contain or inherit "__x__". Is missing "__x__".
      v (pass) custom_map: all elements must pass a custom test.
      
      i See `predicater::assert_environment()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_vector snapshot

    Code
      try(do.call(assert_vector, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_vector()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of 'type' "list".
      v (pass) len   : length must be in range 1 to 10.
      v (pass) n_na  : #of NA values must be 0.
      v (pass) n_null: #of NULL values must be in range 0 to 1.
      x (fail) n_empty: #of empty values must be 0. Was 1.
      x (fail) n_dup : #of duplicate values must be 0. Was 1.
      v (pass) custom_map: all elements must pass a custom test.
      
      i See `predicater::assert_vector()` and this condition's `rs_assert_error` attribute for details.

