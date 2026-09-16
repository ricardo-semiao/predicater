# Examples - assert_symbol snapshot

    Code
      try(rlang::exec(assert_symbol, !!!c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : 
        `.Primitive("quote")(my_var)` failed `assert_symbol()`:
      v sentinels: ok.
      v type: ok.
      v n_char: ok.
      v valid: ok.
      x env_has: supplied symbols were not found in the supplied environment.
      
      i See this condition's `rs_assert_error` attribute for details.

# Examples - assert_language snapshot

    Code
      try(rlang::exec(assert_language, !!!c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : 
        `.Primitive("quote")(rlang::env(a = 1, b = 2))` failed `assert_language()`:
      v sentinels: ok.
      v type: ok.
      x name: function name does not match expected value.
      x ns: namespace does not match expected value.
      v n_args: ok.
      v arg_names: ok.
      x simple: simple call check failed.
      v valid: ok.
      
      i See this condition's `rs_assert_error` attribute for details.

# Examples - assert_code snapshot

    Code
      try(rlang::exec(assert_code, !!!c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `42` failed `assert_code()`:
      v sentinels: ok.
      v type: ok.
      v valid: ok.
      v empty: ok.
      x custom: did not pass the custom test.
      
      i See this condition's `rs_assert_error` attribute for details.

