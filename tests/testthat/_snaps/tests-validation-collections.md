# Examples - assert_list snapshot

    Code
      try(do.call(assert_list, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in FUN(X[[i]], ...) : Evaluating `custom(x)` raised an error.
      Caused by error:
      ! Could not evaluate cli `{}` expression: `x_name[i]`.
      Caused by error:
      ! object 'x_name' not found

# Examples - assert_environment snapshot

    Code
      try(do.call(assert_environment, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_environment()`:
      v sentinels: ok.
      v type: ok.
      v len: ok.
      x namespace: namespace status check failed.
      v parents: ok.
      v env_has: ok.
      x env_sees: supplied symbols were not found in the supplied environment or its parents.
      v custom_map: ok.
      
      i See this condition's `rs_assert_error` attribute for details.

# Examples - assert_vector snapshot

    Code
      try(do.call(assert_vector, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_vector()`:
      v sentinels: ok.
      v type: ok.
      v len: ok.
      v n_na: ok.
      v n_null: ok.
      x n_empty: had 1 empty values.
      x n_dup: had 1 duplicated values.
      v custom_map: ok.
      
      i See this condition's `rs_assert_error` attribute for details.

