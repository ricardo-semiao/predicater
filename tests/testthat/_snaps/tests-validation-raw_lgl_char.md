# Examples - assert_logical snapshot

    Code
      try(do.call(assert_logical, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_logical()`:
      v sentinels: ok.
      v type: ok.
      v len: ok.
      x n_na: had 1 NA values.
      v n_true: ok.
      
      i See this condition's `rs_assert_error` attribute for details.

# Examples - assert_character snapshot

    Code
      try(do.call(assert_character, c(list(x), args)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_character()`:
      v sentinels: ok.
      v type: ok.
      v len: ok.
      v n_na: ok.
      x n_dup: had 1 duplicated values.
      * n_char: not tested due to previous failure.
      * set: not tested due to previous failure.
      * match: not tested due to previous failure.
      * sorted: not tested due to previous failure.
      
      i See this condition's `rs_assert_error` attribute for details.

