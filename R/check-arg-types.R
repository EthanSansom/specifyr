the <- rlang::new_environment()

check_arg_types <- function(uid = NULL) {

  call_env <- rlang::caller_env()
  fun <- rlang::frame_fn(call_env)

  # A function will potentially be hashed twice:
  # - once when it is first called and hasn't been byte compiled
  # - once more after it has been byte complied (i.e. has a `<bytecode: ...>`)
  fun_hash <- uid %||% rlang::hash(fun)
  typed_fmls <- rlang::env_cache(the, fun_hash, NULL)

  call_arg_nms <- rlang::names2(
    rlang::call_match(
      call = rlang::frame_call(call_env),
      fn = fun
    )[-1]
  )

  if (!is.null(typed_fmls)) {
    # The calling function's required typed args are stored in the first element
    # of `typed_fmls`. These need to be checked manually, since a typed argument
    # will never be missing (i.e. it always has a default, the type function).
    #
    # Example (of bad path):
    #> my_mean <- function(x = specifyr::int) { base::mean(x) }
    #> my_mean() # the body evaluates as `base::mean(specifyr::int)`
    if (!checkmate::test_subset(typed_fmls[[1]], call_arg_nms)) {
      stop_missing_args(
        setdiff(typed_fmls[[1]], call_arg_nms),
        error_call = call_env
      )
    }
    return(lapply(typed_fmls[-1], eval, envir = call_env))
  }

  # Turn the following into a function `cache_typed_args()`
  # start ----

  # Standardize the formals
  fun_fmls <- formals(fun)
  typed_fmls <- .mapply(
    standardize_typed_fml,
    dots = list(
      fml = fun_fmls,
      fml_arg_name = names(fun_fmls)
    ),
    MoreArgs = list(
      fml_env = call_env
    )
  )
  is_typed_at <- !vapply(typed_fmls, is.null, logical(1L))

  # Required args are typed formals with no default, e.g. `bool()` not `bool(x = TRUE)`
  is_required_arg <- vapply(
    X = fun_fmls,
    FUN = fml_is_required,
    FUN.VALUE = logical(1L),
    fml_env = call_env
  )

  # `standardize_typed_fml` is NULL for non-typed formals
  names(typed_fmls) <- names(fun_fmls)
  required_args <- names(fun_fmls)[is_required_arg & is_typed_at]
  typed_fmls <- typed_fmls[is_typed_at]

  the[[fun_hash]] <- append(list(required_args), typed_fmls)

  # return(typed_fmls)
  # end ----

  if (!checkmate::test_subset(required_args, call_arg_nms)) {
    stop_missing_args(
      setdiff(required_args, call_arg_nms),
      error_call = call_env
    )
  }
  lapply(typed_fmls, eval, envir = call_env)
}

fcheck_arg_types <- function(uid) {

  call_env <- rlang::caller_env()
  typed_fmls <- rlang::env_cache(the, uid, NULL)

  if (!is.null(typed_fmls)) {
    # The second element of the `type_fmls` is a function definition to match.
    call_arg_nms <- rlang::names2(
      rlang::call_match(
        call = rlang::frame_call(call_env),
        fn = typed_fmls[[2]]
      )[-1]
    )

    # The calling function's required typed args are stored in the first element
    # of `typed_fmls`. These need to be checked manually, since a typed argument
    # will never be missing (i.e. it always has a default, the type function).
    #
    # Example (of bad path):
    #> my_mean <- function(x = specifyr::int) { base::mean(x) }
    #> my_mean() # the body evaluates as `base::mean(specifyr::int)`
    if (!checkmate::test_subset(typed_fmls[[1]], call_arg_nms)) {
      stop_missing_args(
        setdiff(typed_fmls[[1]], call_arg_nms),
        error_call = call_env
      )
    }
    lapply(typed_fmls[-c(1:2)], eval, envir = call_env)
  }

  # Turn the following into a function `cache_typed_args()`
  # start ----

  # Standardize the formals
  fun <- rlang::frame_fn(call_env)
  fun_fmls <- formals(fun)
  typed_fmls <- .mapply(
    standardize_typed_fml,
    dots = list(
      fml = fun_fmls,
      fml_arg_name = names(fun_fmls)
    ),
    MoreArgs = list(
      fml_env = call_env
    )
  )
  is_typed_at <- !vapply(typed_fmls, is.null, logical(1L))

  # Required args are typed formals with no default, e.g. `bool()` not `bool(x = TRUE)`
  is_required_arg <- vapply(
    X = fun_fmls,
    FUN = fml_is_required,
    FUN.VALUE = logical(1L),
    fml_env = call_env
  )

  # `standardize_typed_fml` is NULL for non-typed formals
  names(typed_fmls) <- names(fun_fmls)
  required_args <- names(fun_fmls)[is_required_arg & is_typed_at]
  typed_fmls <- typed_fmls[is_typed_at]

  # Version of `fun` with no body, to use in `rlang::call_match`
  rlang::fn_body(fun) <- NULL
  rlang::fn_env(fun) <- emptyenv()

  the[[uid]] <- append(list(required_args, fun), typed_fmls)

  # return(typed_fmls)
  # end ----

  call_arg_nms <- rlang::names2(
    rlang::call_match(
      call = rlang::frame_call(call_env),
      fn = fun
    )[-1]
  )

  if (!checkmate::test_subset(required_args, call_arg_nms)) {
    stop_missing_args(
      setdiff(required_args, call_arg_nms),
      error_call = call_env
    )
  }
  lapply(typed_fmls, eval, envir = call_env)
}

fassert_arg_types <- function(uid) {

  call_env <- rlang::caller_env()
  typed_fmls <- rlang::env_cache(the, uid, NULL)

  if (!is.null(typed_fmls)) {
    # The second element of the `type_fmls` is a function definition to match.
    call_arg_nms <- rlang::names2(
      rlang::call_match(
        call = rlang::frame_call(call_env),
        fn = typed_fmls[[2]]
      )[-1]
    )

    # The calling function's required typed args are stored in the first element
    # of `typed_fmls`. These need to be checked manually, since a typed argument
    # will never be missing (i.e. it always has a default, the type function).
    #
    # Example (of bad path):
    #> my_mean <- function(x = specifyr::int) { base::mean(x) }
    #> my_mean() # the body evaluates as `base::mean(specifyr::int)`
    if (!checkmate::test_subset(typed_fmls[[1]], call_arg_nms)) {
      stop_missing_args(
        setdiff(typed_fmls[[1]], call_arg_nms),
        error_call = call_env
      )
    }
    lapply(typed_fmls[call_arg_nms], eval, envir = call_env)
    return(invisible())
  }

  # Turn the following into a function `cache_typed_args()`
  # start ----

  # Standardize the formals
  fun <- rlang::frame_fn(call_env)
  fun_fmls <- formals(fun)
  typed_fmls <- .mapply(
    standardize_typed_fml,
    dots = list(
      fml = fun_fmls,
      fml_arg_name = names(fun_fmls)
    ),
    MoreArgs = list(
      fml_env = call_env
    )
  )
  is_typed_at <- !vapply(typed_fmls, is.null, logical(1L))

  # Required args are typed formals with no default, e.g. `bool()` not `bool(x = TRUE)`
  is_required_arg <- vapply(
    X = fun_fmls,
    FUN = fml_is_required,
    FUN.VALUE = logical(1L),
    fml_env = call_env
  )

  # `standardize_typed_fml` is NULL for non-typed formals
  names(typed_fmls) <- names(fun_fmls)
  required_args <- names(fun_fmls)[is_required_arg & is_typed_at]
  typed_fmls <- typed_fmls[is_typed_at]

  # Version of `fun` with no body, to use in `rlang::call_match`
  rlang::fn_body(fun) <- NULL
  rlang::fn_env(fun) <- emptyenv()

  the[[uid]] <- append(list(required_args, fun), typed_fmls)

  # return(typed_fmls)
  # end ----

  call_arg_nms <- rlang::names2(
    rlang::call_match(
      call = rlang::frame_call(call_env),
      fn = fun
    )[-1]
  )

  if (!checkmate::test_subset(required_args, call_arg_nms)) {
    stop_missing_args(
      setdiff(required_args, call_arg_nms),
      error_call = call_env
    )
  }
  lapply(typed_fmls[call_arg_nms], eval, envir = call_env)
  return(invisible())
}

f <- function(x = int(len = 1L, nas = FALSE), y = int, ..., z = TRUE) {
  check_arg_types("f")
}
f_fast <- function(x = int(len = 1L, nas = FALSE), y = int, ..., z = TRUE) {
  fcheck_arg_types("f_fast")
}

if (FALSE) {
  rm(list = ls())
  load_all()
  as.list(the)
  f()
  f(NA_integer_, 12)

  # TODO: Okay, `f_fast` is weirdly much slower than `f`. Maybe just abandon.
  bench::mark(
    f(10L, 20L),
    f_fast(10L, 20L)
  )
}


# helpers ----------------------------------------------------------------------

standardize_typed_fml <- function(fml, fml_arg_name, fml_env) {

  # This is the case when the argument is `...`
  if (rlang::is_missing(fml)) {
    return(NULL)
  }

  # `fml` is a symbol (`int`) or a namespaced symbol (`specifyr::int`)
  if (rlang::is_symbol(fml) || rlang::is_call(fml, "::")) {
    if (is_type_check(eval(fml, fml_env))) {
      type_call <- rlang::call2(
        fml,
        x = rlang::sym(fml_arg_name),
        x_name = fml_arg_name
      )
      return(type_call)
    }
  }

  # `fml` is a typed function call `int()`, `specifyr::int(x = 10L)`
  if (rlang::is_call(fml)) {
    fml_fun <- eval(fml[[1]], fml_env)
    if (is_type_check(fml_fun)) {
      type_call <- rlang::call_match(call = fml, fn = fml_fun)
      type_call <- rlang::call_modify(
        type_call,
        x = rlang::sym(fml_arg_name),
        x_name = fml_arg_name
      )
      return(type_call)
    }
  }
  NULL
}

fml_is_required <- function(fml, fml_env) {
  if (!rlang::is_call(fml)) {
    return(TRUE)
  }
  fml_call <- rlang::call_match(call = fml, fn = eval(fml[[1]], fml_env))
  "x" %notin% rlang::call_args_names(fml_call)
}

stop_missing_args <- function(arg_names, error_call = rlang::caller_env()) {
  cli::cli_abort(
    "{.arg {arg_names}} {?is/are} absent but must be supplied.",
    call = error_call
  )
}
