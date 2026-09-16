# Examples - assert_names snapshot

    Code
      try(do.call(assert_names, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in range[[i]] <- l + r : replacement has length zero

# Examples - assert_matrix snapshot

    Code
      try(do.call(assert_matrix, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_matrix()`:
      v sentinels: ok.
      v type: ok.
      v n_dims: ok.
      v dims_shape: ok.
      v custom: ok.
      x custom_apply:
      
      i See this condition's `rs_assert_error` attribute for details.

# Examples - assert_class snapshot

    Code
      try(do.call(assert_class, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_class()`:
      v sentinels: ok.
      v type: ok.
      v classes: ok.
      v tests_char: ok.
      x custom: did not pass the custom test.
      
      i See this condition's `rs_assert_error` attribute for details.

# Examples - assert_object snapshot

    Code
      try(do.call(assert_object, c(list(x), args, short_circuit = FALSE)))
    Output
      Error in eval(code, test_env) : `x` failed `assert_object()`:
      v sentinels: ok.
      v type: ok.
      v oo_system: ok.
      v s4_bit: ok.
      x tests_class: failed object class tests.
      v custom: ok.
      
      i See this condition's `rs_assert_error` attribute for details.

