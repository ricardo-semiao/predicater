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
      Error in attributes(x) <- attrs : 
        'names' attribute [2] must be the same length as the vector [1]

# Examples - assert_code snapshot

    Code
      try(rlang::exec(assert_code, !!!c(list(x), args, short_circuit = FALSE)))
    Output
      Error in if (attrs$sym) "symbol" : argument is of length zero

