# constructor ------------------------------------------------------------------

check <- function(
    predicate,
    message,
    description,
    wrap_is_true = FALSE,
    check_env = rlang::caller_env(),
    check_return = rlang::sym("x")
  ) {

  # Prevents a confusing error from `rlang::enquo(arg)` when arg is missing
  rlang::check_required(predicate)
  rlang::check_required(message)

  # `rlang::enquo` is used intentionally, since we want to track the environment
  # during validation.
  predicate_call <- evalidate_predicate(rlang::enquo(predicate))
  message_call <- evalidate_message(rlang::enquo(message))
  if (wrap_is_true) predicate_call <- rlang::call2("isTRUE", predicate_call)

  description <- if (rlang::is_missing(description)) {
    paste(rlang::expr_label(predicate_call), "evalutates to TRUE.")
  } else {
    check_is_string(description, error_class = "specifyr_malformed_check_error")
  }
  check_env <- check_is_env(check_env, error_class = "specifyr_malformed_check_error")

  # There is no validation here, the user can opt to return whatever expression
  # (valid or otherwise) that they want.
  check_return_expr <- rlang::enexpr(check_return)
  if (identical(check_return_expr, rlang::expr(rlang::sym("x")))) {
    check_return_expr <- check_return
  }

  structure(
    .Data = new_check_fn(
      predicate = predicate_call,
      message = message_call,
      check_return = check_return_expr,
      check_env = check_env
    ),
    blueprint = new_check_blueprint(
      predicate = predicate_call,
      message = message_call,
      description = description,
      check_return = check_return_expr
    ),
    class = "specifyr_check"
  )
}

check_must <- function(
    predicate,
    must,
    wrap_is_true = FALSE,
    check_env = rlang::caller_env(),
    check_return = rlang::sym("x")
  ) {

  rlang::check_required(predicate)

  predicate <- evalidate_predicate(rlang::enquo(predicate))
  must <- check_is_string(must)
  check_env <- check_is_env(check_env)
  check_return <- evalidate_check_return(rlang::enexpr(check_return))

  if (wrap_is_true) predicate_call <- rlang::call2("isTRUE", predicate_call)
  message <- paste0("{.arg {x_name}} must ", must, ".")
  description <- paste0("Must ", must, ".")

  structure(
    .Data = new_check_fn(
      predicate = predicate,
      message = message,
      check_return = check_return,
      check_env = check_env
    ),
    blueprint = new_check_blueprint(
      predicate = predicate,
      message = message,
      description = description,
      check_return = check_return
    ),
    class = "specifyr_check"
  )
}

check_must_not <- function(
    predicate,
    must,
    not,
    wrap_is_true = FALSE,
    check_env = rlang::caller_env(),
    check_return = rlang::sym("x")
) {

  rlang::check_required(predicate)

  predicate <- evalidate_predicate(rlang::enquo(predicate))
  must <- check_is_string(must)
  not <- check_is_string(not)
  check_env <- check_is_env(check_env)
  check_return <- evalidate_check_return(rlang::enexpr(check_return))

  if (wrap_is_true) predicate_call <- rlang::call2("isTRUE", predicate_call)
  message <- paste0("{.arg {x_name}} must ", must, ", not ", not, ".")
  description <- paste0("Must ", must, ".")

  structure(
    .Data = new_check_fn(
      predicate = predicate,
      message = message,
      check_return = check_return,
      check_env = check_env
    ),
    blueprint = new_check_blueprint(
      predicate = predicate,
      message = message,
      description = description,
      check_return = check_return
    ),
    class = "specifyr_check"
  )
}

new_check_fn <- function(
    predicate,
    message,
    check_return,
    check_env
  ) {

  specifyr_internal_error(predicate, "is_expression", "rlang")
  if (!(rlang::is_call(message) || is.character(message))) {
    cli::cli_abort(
      "{.arg message} must be a call or a character vector.",
      .internal = TRUE
    )
  }
  specifyr_internal_error(check_return, "is_expression", "rlang")
  specifyr_internal_error(check_env, "is.environment", "base")

  args <- rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::caller_env()),
    error_class = "specifyr_check_fail_error"
  )
  body <- predicate_check_body(
    x = rlang::sym("x"),
    error_index = list(),
    predicate = predicate,
    message = message
  )
  body <- rlang::expr({
    !!body
    !!check_return
  })

  rlang::new_function(
    args = args,
    body = body,
    env = check_env
  )
}

new_check_blueprint <- function(
    predicate,
    message,
    description,
    check_return
  ) {

  specifyr_internal_error(predicate, "is_expression", "rlang")
  if (!(rlang::is_call(message) || is.character(message))) {
    cli::cli_abort(
      "{.arg message} must be a call or a character vector.",
      .internal = TRUE
    )
  }
  specifyr_internal_error(description, "is.character", "base")
  specifyr_internal_error(check_return, "is_expression", "rlang")

  structure(
    .Data = list(
      predicate = predicate,
      message = message,
      description = description,
      check_return = check_return
    ),
    class = "specifyr_check_blueprint"
  )
}

# TODO: Fix these internal errors
blueprint_to_check <- function(blueprint, check_env) {
  specifyr_internal_error(check, "is_check_blueprint")
  specifyr_internal_error(check_env, "is.environment", "base")
  new_check_fn(
    predicate = blueprint$predicate,
    message = blueprint$message,
    check_return = blueprint$check_return,
    check_env = check_env
  )
}

blueprint_to_check_body <- function(blueprint) {
  specifyr_internal_error(check, "is_check_blueprint")
  predicate_check_body(
    predicate = blueprint$predicate,
    message = blueprint$message
  )
}

is_check_blueprint <- function(x) {
  inherits(x, "specifyr_check_blueprint")
}

evalidate_predicate <- function(predicate_quo, error_call = rlang::caller_env()) {

  specifyr_internal_error(predicate_quo, "is_quosure", "rlang")
  predicate_expr <- rlang::quo_get_expr(predicate_quo)

  # `predicate` may be a name-spaced function, in which case `predicate_expr`
  # will still be call, but a call to `::`.
  if (!is.call(predicate_expr) || rlang::is_call(predicate_expr, "::")) {
    # If `predicate` is a function, convert it to a call with first argument `x`
    predicate <- rlang::eval_tidy(predicate_quo)
    if (is.function(predicate)) {
      return(rlang::call2(predicate_expr, rlang::sym("x")))
    }
    specifyr_error(
      "{.arg predicate} must be a one-sided formula, function, or a function call.",
      i = "{.arg predicate} is {.obj_type_friendly {predicate}}.",
      .error_call = error_call
    )
  }

  # TODO:
  # We definitely want some AST walker that just looks for the symbol `x`. Then we can:
  # - check whether some expression (i.e. the `predicate` formula) contains the symbol `x`
  # - replace the symbol `x` with some other symbol if required (see TODO in `predicate_test_body`)
  #
  # If `predicate` is a one sided formula, we take the users word that it will
  # evaluate to something we want and use the formula body without validation.
  if (rlang::is_call(predicate_expr, "~")) {
    predicate <- rlang::eval_tidy(predicate_quo)
    if (!is.null(rlang::f_lhs(predicate))) {
      specifyr_error(
        "{.arg predicate} must be a one-sided formula, function, or a function call.",
        x = "{.arg predicate} is a two-sided formula.",
        .error_call = error_call
      )
    }
    return(rlang::f_rhs(predicate))
  }

  # `predicate` might be an anonymous function, in which case the outer call will
  # be `function()`. If that call has only one argument `x`, take the body of the
  # function (i.e `\(x) { f(x) }` becomes `f(x)`), otherwise treat as you would a
  # named function.
  if (rlang::is_call(predicate_expr, "function")) {
    predicate <- rlang::eval_tidy(predicate_quo)
    if (identical(rlang::fn_fmls_names(predicate), "x")) {
      return(rlang::fn_body(predicate))
    }
    return(rlang::call2(predicate_expr, rlang::sym("x")))
  }

  # When `predicate` is a call, we want to make sure that the symbol `x` is one
  # of it's supplied arguments. If `predicate` is a name-spaced call, we inspect
  # the arguments of next call (since the first will be to `::`).
  if (rlang::is_call(predicate_expr, "::")) {
    predicate_args <- rlang::call_args(predicate_expr[[2]])
  } else {
    predicate_args <- rlang::call_args(predicate_expr)
  }
  if (any(vapply(predicate_args, identical, logical(1L), y = rlang::sym("x")))) {
    return(predicate_expr)
  }

  # TODO Ethan:
  # Redefine this message for binary calls and other weird calls.
  # Try: `check(1 + 1, "")`
  # The message doesn't make much sense.
  # Also, since you capture the expressions of everything already, why not
  # show the captured expression in the error message.
  #
  # `predicate` must be a function or a call with some argument equal to `x`.
  # i = `predicate = is_good(arg = arg)`
  # x = `predicate` is a function call with 1 argument:
  # * `arg = arg`

  n_args <- length(predicate_args)
  error_msg <- "{.arg predicate} is a function call with {n_args} argument{?s}:"
  if (n_args == 0) {
    error_msg <- "{.arg predicate} is a function call with 0 arguments."
    bullets <- NULL
  } else if (n_args > 5) {
    args <- predicate_args[1:5]
    bullets <- c(
      rlang::set_names(paste(names(args), " = ", args), nm = "*"),
      paste("... and", n_args - 5, "more.")
    )
  } else {
    bullets <- rlang::set_names(
      paste(names(predicate_args), " = ", predicate_args),
      nm = "*"
    )
  }
  example <- c(
    " ",
    " " = "# Good",
    " " = 'specifyr::check(predicate = is_good(arg = x), message = "{{.arg x}} must be good.")',
    " ",
    " " = "# Bad",
    " " = 'specifyr::check(predicate = is_good(arg = arg), message = "{{.arg arg}} must be good.")',
    " "
  )
  specifyr_error(
    c(
      "{.arg predicate} must be a function or a call with some argument equal to {.arg x}.",
      x = error_msg,
      bullets,
      example
    ),
    .error_call = error_call
  )
}

evalidate_message <- function(message_quo, error_call = rlang::caller_env()) {

  specifyr_internal_error(message_quo, "is_quosure", "rlang")
  message <- rlang::eval_tidy(message_quo)

  if (is.function(message)) {
    fmls <- evalidate_message_fmls(message, error_call = error_call)
    message_call <- rlang::call2(rlang::quo_get_expr(message_quo), !!!fmls)
  } else if (is.character(message)) {
    # Not using `quo_get_expr(message_quo)` here in case the user's `message` was
    # a symbol referring to a character vector or a call to create a character.
    message_call <- rlang::expr(!!message)
  } else {
    cli::cli_abort(
      c(
        "{.arg message} must be a character vector or a function.",
        x = "{.arg message} is {.obj_type_friendly {message}."
      ),
      call = error_call,
      class = "specifyr_malformed_check_error"
    )
  }
  message_call
}

evalidate_message_fmls <- function(message_fn, error_call = rlang::caller_env()) {
  fmls <- rlang::fn_fmls_syms(message_fn)
  fmls_names <- names(fmls)
  if (!all(fmls_names %in% c("x", "x_name"))) {
    bad_fmls <- fmls_names[fmls_names %notin% c("x", "x_name")]
    cli::cli_abort(
      c(
        "Can't evaluate {.cls specifyr_check} function.",
        i = "`message` must be a character or a function with formals `x` and/or `x_name`.",
        x = "`message` is a function with {length(bad_fmls)} invalid formal{?s}: {.arg {bad_fmls}}."
      ),
      call = error_call,
      class = "specifyr_malformed_check_error"
    )
  }
  fmls
}

evalidate_check_return <- function(check_return_expr) {
  if (identical(check_return_expr, rlang::expr(rlang::sym("x")))) {
    check_return_expr <- eval(check_return_expr)
  }
  check_return_expr
}

# helpers ----------------------------------------------------------------------

is_check <- function(x) {
  inherits(x, "specifyr_check")
}

# TODO: Implement. Used to add checks to a specification. You'll need to re-create
# the specification from it's blueprint.
add_checks <- function(spec, ...) {

  dots <- rlang::list2(...)
  spec <- check_is_spec(spec)

}

combine_checks <- function(..., .check_env, .check_return = rlang::sym("x")) {

  dots <- rlang::list2(...)
  if (is_empty(dots)) {
    cli::cli_abort(
      c(
        "Must supply at least one {.cls specifyr_check} object to `...`.",
        i = "`...` is empty."
      ),
      class = "specifyr_malformed_check_error"
    )
  }

  checks <- purrr::map2(
    dots,
    paste0("..", seq_along(dots)),
    \(x, x_name) {
      check_is_check(
        x = x,
        x_name = x_name,
        error_class = "specifyr_malformed_check_error"
      )
    }
  )
  check_return <- evalidate_check_return(rlang::enexpr(.check_return))
  check_env <- if (rlang::is_missing(.check_env)) {
    rlang::fn_env(checks[[1]])
  } else {
    check_is_env(.check_env)
  }

  check_blueprints <- lapply(checks, blueprint)
  bodies <- lapply(
    check_blueprints,
    \(blueprint) {
      predicate_check_body(
        x = rlang::sym("x"),
        error_index = list(),
        predicate = blueprint$predicate,
        message = blueprint$message
      )
    }
  )

  args <- rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::caller_env()),
    error_class = "specifyr_check_fail_error"
  )
  body <- expr_squash(!!!bodies, check_return)
  check_fn <- rlang::new_function(
    args = args,
    body = body,
    env = check_env
  )

  check_blueprint <- structure(
    .Data = list(
      predicates = lapply(check_blueprints, `[[`, "predicate"),
      messages = lapply(check_blueprints, `[[`, "message"),
      descriptions = lapply(check_blueprints, `[[`, "description"),
      check_return = check_return
    ),
    class = c("specifyr_multi_check_blueprint", "specifyr_check_blueprint")
  )

  structure(
    .Data = check_fn,
    blueprint = check_blueprint,
    class = c("specifyr_multi_check", "specifyr_check")
  )
}

# factories --------------------------------------------------------------------

predicate_check_body <- function(x, error_index, predicate, message) {
  rlang::expr(
    if (!!predicate_test_body(x, predicate, negate = TRUE)) {
      specifyr::emit_predicate_error(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class,
        message = !!message
      )
    }
  )
}

predicate_check_body_multi <- function(x, x_indices, error_index, predicate, message) {

  if (!rlang::is_empty(x_indices)) {
    x <- rlang::call2("[", x, x_indices)
  }

  rlang::expr(
    if (!!predicate_test_body_multi(x, predicate, negate = TRUE)) {
      specifyr::emit_predicate_error_multi(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class,
        message = !!message,
        predicate = !!predicate
      )
    }
  )
}

predicate_test_body <- function(x, predicate, negate = FALSE) {
  specifyr_internal_error(predicate, "is_expression", "rlang")

  # TODO Ethan: Make this into a non-hacky version, by walking the AST and replacing
  # the symbol `x` with some other desired symbol. Go through:
  # https://adv-r.hadley.nz/expressions.html#ast-funs
  # and some of the other meta-programming chapters.
  #
  # `predicate` is an expression containing the symbol `x` (ex.`is.numeric(x) && x > 10`).
  # This is a hack to make the expression refer to some other symbol, in this case
  # the symbol we've provided to the argument `x = new_symbol`. We convert the
  # `predicate` to a function of `x` and then call it with the `new_symbol`.
  # Ex. `is.numeric(x) && x > 10` -> `(\(x) is.numeric(x) && x > 10)(new_symbol)`
  if (!rlang::is_symbol(x, "x")) {
    predicate <- rlang::call2(rlang::expr(\(x) !!predicate), x)
  }
  if (negate) rlang::call2("!", predicate) else predicate
}

predicate_test_body_multi <- function(x, predicate, negate = FALSE) {

  specifyr_internal_error(x, "is.symbol", "base")

  predicate_test <- rlang::expr(\(x) !!predicate)
  if (negate) {
    rlang::expr(any(vapply(!!x, !!rlang::call2("!", predicate_test), logical(1L))))
  } else {
    rlang::expr(all(vapply(!!x, !!predicate_test, logical(1L))))
  }
}

# emittors ---------------------------------------------------------------------

#' @export
emit_predicate_error <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class,
    message
) {

  x <- if (is_empty(x_index)) x else purrr::pluck(x, !!!x_index)
  x_name <- paste0(x_name, format_index(x_index))

  message_expr <- rlang::enexpr(message)
  if (rlang::is_call(message_expr)) {
    message <- rlang::try_fetch(
      # The `message` call is meant to receive the arguments `x` and/or `x_name`
      # which as they are defined above, so we evaluate `message` in this frame.
      eval(message),
      error = function(cnd) {
        cli::cli_abort(
          "Can't evaluate {.cls specifyr_check} function message.",
          parent = cnd,
          call = error_call,
          class = "specifyr_malformed_check_error"
        )
      }
    )
  }

  rlang::try_fetch(
    cli::cli_abort(
      message,
      call = error_call,
      class = error_class,
      x_name = x_name,
      x_index = x_index
    ),
    # An error in `cli::cli_abort` is likely due to an invalid glue specification
    # in the `message`. Ex. `message = "{.arg {this_does_not_exist}}"` causes an
    # rlib_error: `! object 'this_does_not_exist' not found`.
    rlib_error = function(cnd) {
      cli::cli_abort(
        "Can't evaluate {.cls specifyr_check} function.",
        parent = cnd,
        call = error_call,
        class = "specifyr_malformed_check_error"
      )
    }
  )
}

#' @export
emit_predicate_error_multi <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class,
    message,
    predicate
) {

  predicate_test <- rlang::expr(\(x) !!predicate)
  error_at <- which.min(vapply(x, predicate_test, logical(1L)))
  error_index <- append(x_index, as.numeric(error_at))

  emit_predicate_error(
    x = x[[error_at]],
    x_name = x_name,
    x_index = error_index,
    error_call = error_call,
    error_class = error_class,
    message = message
  )
}

# INTERACTIVE TESTS ------------------------------------------------------------

if (FALSE) {

  rm(list = ls())
  load_all()

  # Test 0 arguments error
  check(checkmate::qassert(), "")

  # Test `blueprint_to_check`
  blueprint_to_check(blueprint(check(\(x) isTRUE(x + 1), "a message")), check_env = rlang::current_env())
  blueprint_to_check(blueprint(check(~ isTRUE(x + 1), "a message")), check_env = rlang::current_env())

  # `check_must` and `check_must_not`
  is_numeric <- check_must(~is.numeric(x), "be a {.cls numeric} vector")

  is_numeric(10)
  is_numeric("A")

  is_scalar <- check_must_not(~length(x) == 1, "be length 1", "length {length(x)}")

  is_scalar(10)
  is_scalar(1:5)
  is_scalar(logical(0L))

  # Combine checks
  scalar_numeric <- combine_checks(is_numeric, is_scalar)

  scalar_numeric(10)
  scalar_numeric("A")
  scalar_numeric(1:5)

}
