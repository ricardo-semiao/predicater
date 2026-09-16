# Examples - assert_symbol snapshot

    Code
      try(rlang::exec(assert_symbol, x = quote(my_var), !!!c(args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : 
        `.Primitive("quote")(my_var)` failed `assert_symbol()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of type "symbol".
      v (pass) n_char: #of characters must be in range 1 to 10.
      v (pass) valid : must be a syntactically valid R name.
      x (fail) env_has: must contain "my_var". Is missing "my_var".
      
      i See `predicater::assert_symbol()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_language snapshot

    Code
      try(rlang::exec(assert_language, !!!c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : 
        `.Primitive("quote")(rlang::env(a = 1, b = 2))` failed `assert_language()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of type "language".
      x (fail) name  : must have call name "fn". Was "env".
      x (fail) ns    : must have call namespace "otherpkg". Was "rlang".
      v (pass) n_args: #of arguments must be in range 1 to 5.
      v (pass) arg_names: must have argument names: "a" and "b".
      x (fail) simple: must be not a simple call. Was simple.
      v (pass) valid : must be a valid call.
      
      i See `predicater::assert_language()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_code snapshot

    Code
      try(rlang::exec(assert_code, !!!c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `42` failed `assert_code()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of 'type' "syntactic literal".
      v (pass) valid : must be valid code.
      v (pass) empty :
      x (fail) custom: must pass a custom test. Did not.
      
      i See `predicater::assert_code()` and this condition's `rs_assert_error` attribute for details.

