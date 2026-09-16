# Simple msg function

    Code
      try(assert_from_msg(msg_fun, x))
    Output
      Error in eval(code, test_env) : 
        Error with argument `x`: x must be numeric and have no missing values

# Simple error function

    Code
      try(assert_from_error(err_fun, x))
    Output
      Error in value[[3L]](cond) : object 'res' not found

# Examples - assert_ptype snapshot

    Code
      try({
        f <- (function(x) assert_ptype(numeric(), x))
        f("a")
      })
    Output
      Error in "fun(..., .envir = .envir)" : 
        ! Could not evaluate cli `{}` expression: `x_name[i]`.
      Caused by error in `eval(expr, envir = envir)`:
      ! object 'x_name' not found

# Examples - assert_predicate snapshot

    Code
      try({
        f <- (function(x) assert_predicate(function(x) all(x > 0), x))
        f(c(-1, 0, 1))
      })
    Output
      Error in "fun(..., .envir = .envir)" : 
        ! Could not evaluate cli `{}` expression: `x_name[i]`.
      Caused by error in `eval(expr, envir = envir)`:
      ! object 'x_name' not found

# Examples - assert_multiple snapshot

    Code
      try(do.call(assert_multiple, c(list(x), args)))
    Output
      Error in core_multiple(x, types, ..., short_circuit) : 
        argument "short_circuit" is missing, with no default

