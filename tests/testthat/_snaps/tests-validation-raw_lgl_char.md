# Examples - assert_logical snapshot

    Code
      try(do.call(assert_logical, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_logical()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of type "logical".
      v (pass) len   : length must be in range 1 to 10.
      x (fail) n_na  : #of NA values must be 0. Was 1.
      v (pass) n_true: #of TRUE values must be in range 1 to 2.
      
      i See `predicater::assert_logical()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_character snapshot

    Code
      try(do.call(assert_character, c(list(x), args)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_character()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must be of type "character".
      v (pass) len   : length must be in range 1 to 10.
      v (pass) n_na  : #of NA values must be 0.
      x (fail) n_dup : #of duplicate values must be 0. Was 1.
      * (skip) n_char: skiped given failure.
      * (skip) set   : skiped given failure.
      * (skip) match : skiped given failure.
      * (skip) sorted: skiped given failure.
      
      i See `predicater::assert_character()` and this condition's `rs_assert_error` attribute for details.

