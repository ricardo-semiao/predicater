# Examples - assert_names snapshot

    Code
      try(do.call(assert_names, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_names()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : names must be of type "character".
      x (fail) empty : vector can be empty.
      x (fail) n_na  : #of NA values must be 0. Was 1.
      v (pass) n_empty: #of empty string names must be in range 0 to -1.
      v (pass) n_invalid: #of syntactically invalid names must be in range 0 to Inf.
      x (fail) set   : must be in a custom set. Was not.
      x (fail) custom: must pass a custom test. Did not.
      
      i See `predicater::assert_names()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_matrix snapshot

    Code
      try(do.call(assert_matrix, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_matrix()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : dim must be of type "integer".
      v (pass) n_dims: #of dimensions must be 2.
      v (pass) dims_shape: each dimension size must be in custom range.
      x (fail) names_apply: each dimension names must pass custom `predicater::test_names()` test. Failed for dimensions 1 and 2.
      v (pass) custom: must pass a custom test.
      x (fail) custom_apply: must pass custom tests along some margins. Failed for margin 1.
      
      i See `predicater::assert_matrix()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_class snapshot

    Code
      try(do.call(assert_class, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_class()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : class must be a non-empty no-na character vector.
      v (pass) classes: class must satisfy class inheritance constraints.
      v (pass) tests_char: class must pass the specified character tests.
      x (fail) custom: must pass a custom test. Did not.
      
      i See `predicater::assert_class()` and this condition's `rs_assert_error` attribute for details.

# Examples - assert_object snapshot

    Code
      try(do.call(assert_object, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_object()`:
      v (pass) sentinels: no sentinel values allowed.
      v (pass) type  : must have a consistent class (see `predicater::is_object_like()`).
      v (pass) oo_system:
      v (pass) s4_bit:
      x (fail) tests_class: must pass the custom `predicater::test_class()` test. Failed "classes".
      v (pass) custom: must pass a custom test.
      
      i See `predicater::assert_object()` and this condition's `rs_assert_error` attribute for details.

