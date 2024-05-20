# constructor ------------------------------------------------------------------

# TODO:
# Remember, you'll still have to construct checks from a blueprint for recursive
# specifications, so you'll want to make this constructor flexible enough that it
# can be used within a recursive specification.
#
# - `x` the symbol must be variable
# - `x_name` must be variable
# - `x_index` must be available

# NOTE: `predicate` can be a:
# 1. function `f`       -> supply `x` to the first argument `f(x)`
# 2. call `f(arg = x)`  -> we take your word that this call will be valid

x <- rlang::expr(between(x = x))
identical(call_args(x)[[1]], sym("x"))

rlang::call_args(rlang::expr(between(x = 10, left = 2)))

check <- function(
    predicate,
    message,
    wrap_is_true = FALSE,
    check_env = rlang::caller_env(),
    check_return = rlang::sym("x")
  ) {

  # `rlang::enquo` is used intentionally, since we want to track the environment
  # of `predicate` in this case.
  predicate_call <- evalidate_predicate(rlang::enquo(predicate))
  if (wrap_is_true) predicate_call <- rlang::call2("isTRUE", predicate_call)

  # TODO Ethan: Make an `evalidate_message` as well
  defused_message <- rlang::enexpr(message)
  if (is.function(message)) {
    fmls <- evalidate_message_fmls(message)
    args <- list(x = x, x_name = x_name)[fmls]
    message_call <- rlang::call2(defused_message, !!!args)
  }

  # TODO: This is the user facing version of the check function. It always produces
  # a function of the form:
  #
  # function (
  # x,
  # x_name = rlang::caller_arg(x),
  # error_call = rlang::caller_env(),
  # error_class = "specifyr_check_fail_error"
  # ) {
  #   if (!isTRUE(predicate(x))) {
  #     emit_check_error(...)
  #   }
  #   return(x)
  # }
  #
  # However! The constructor, `new_check`, will need to be much more flexible,
  # since I want to be able to re-create checks within recursive specifications,
  # where the thing I'm checking is not neccisarily `x`. ACTUALLY, it's not really
  # the job of `new_check`, but I'll defo want other functions to all for the building
  # of checks within a specification, from the check blueprint.

}

evalidate_predicate <- function(defused_predicate, error_call = rlang::caller_env()) {

  specifyr_internal_error(defused_predicate, "is_quosure", "rlang")
  defused_predicate <- rlang::quo_get_expr(defused_predicate)

  # `predicate` may be a name-spaced function, in which case `defused_predicate`
  # will still be call, but a call to `::`.
  if (!is.call(defused_predicate) || rlang::is_call(defused_predicate, "::")) {
    # If `predicate` is a function, convert it to a call with first argument `x`
    predicate <- rlang::eval_tidy(defused_predicate)
    if (is.function(predicate)) {
      return(rlang::call2(defused_predicate, rlang::sym("x")))
    }
    specifyr_error(
      "{.arg predicate} must be a function or a function call.",
      i = "{.arg predicate} is {.obj_type_friendly {predicate}}.",
      .error_call = error_call
    )
  }

  # `predicate` might be an anonymous function, in which case the outer call will
  # be `function()`.
  if (rlang::is_call(defused_predicate, "function")) {
    # predicate <- rlang::eval_tidy(defused_predicate)
    # predicate_fmls <- rlang::fn_fmls_names(predicate)
    # if (!identical(predicate_fmls, "x")) {
    #   n_fmls <- length(predicate_fmls)
    #   has_fmls <- if (n_fmls > 5) {
    #     paste("has formal arguments {.arg {predicate_fmls[1:5]}} and", n_fmls - 5, "more")
    #   } else {
    #     "has {n_fmls} formal argument{?s} {.arg {predicate_fmls}}"
    #   }
    #   example <- c(
    #     " ",
    #     " " = "# Good",
    #     " " = 'specifyr::check(predicate = \(x) isTRUE(x == 1), message = "{{.arg x}} must be 1.")',
    #     " ",
    #     " " = "# Bad",
    #     " " = 'specifyr::check(predicate = \(arg) isTRUE(arg == 1), message = "{{.arg arg}} must be 1.")',
    #     " " = 'specifyr::check(predicate = \(x, val = 1) isTRUE(x == val), message = "{{.arg arg}} must be {{val}}.")',
    #     " "
    #   )
    #   specifyr_error(
    #     c(
    #       "Can't convert the anonymous function {.arg predicate} to a call.",
    #       i = "If {.arg predicate} is an anonymous function, it must have one formal argument {.arg x}.",
    #       x = "{.arg predicate} {has_fmls}.",
    #       example
    #     ),
    #     .error_call = error_call
    #   )
    # }
    return(rlang::call2(defused_predicate, rlang::sym("x")))
  }

  # When `predicate` is a call, we want to make sure that the symbol `x` is one
  # of it's supplied arguments. If `predicate` is a name-spaced call, we'll want
  # to inspect the next call.
  if (rlang::is_call(defused_predicate, "::")) {
    predicate_args <- rlang::call_args(defused_predicate[[2]])
  }

  predicate_args <- rlang::call_args(defused_predicate)
  if (!any(vapply(predicate_args, identical, logical(1L), y = sym("x")))) {
    # TODO: Fix the bad message when there are 0 arguments to the call!
    n_args <- length(predicate_args)
    if (n_args > 5) {
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
        "{.arg predicate} must be a function or a call with an argument equal to {.arg x}.",
        x = "{.arg predicate} is a function call with {n_args} argument{?s}:",
        rlang::set_names(bullets, nm = "*"),
        example
      ),
      .error_call = error_call
    )
  }
  defused_predicate
}

new_check <- function(
    predicate,
    message,
    wrap_is_true,
    x = rlang::sym("x"),
    x_name = "x",
    x_index = list(),
    return_success = x
  ) {

  # TODO: This message function creation might be better off in it's own function,
  # depending on how often it's used. Actually, maybe `new_check` should expect
  # an expression (either a call or a character vector), and `check` will take
  # care of the processing of `message`.
  defused_message <- rlang::enexpr(message)
  if (is.function(message)) {
    fmls <- evalidate_message_fmls(message)
    args <- list(x = x, x_name = x_name)[fmls]
    message <- rlang::call2(defused_message, !!!args)
  }

  test <- check_test_body(x, rlang::enexpr(predicate), wrap_is_true)
  body <- rlang::expr({
    if (!(!!test)) {
      emit_check_error(
        x = x,
        x_name = x_name,
        x_index = !!x_index,
        error_call = error_call,
        error_class = error_class,
        message = !!message
      )
    }
    !!return_success
  })
  args <- rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::caller_env()),
    error_class = "specifyr_check_fail_error"
  )

  rlang::new_function(args = args, body = body)
}

# TODO Ethan: This implies that by the blueprint step, we've ensured that the
#             `predicate` is a defused function call. Same with `message`.
new_check_blueprint <- function(predicate, message) {
  specifyr_internal_error_if_not(
    rlang::is_expression(predicate) && is.call(predicate),
    "{.arg predicate} must be a defused call."
  )
  specifyr_internal_error_if_not(
    rlang::is_expression(message) && (is.character(message) || is.call(message)),
    "{.arg message} must be a character or defused function call."
  )
  structure(
    .Data = list(predicate = predicate, message = message),
    class = "specifyr_check_blueprint"
  )
}

check_between_0_1 <- new_check(
  predicate = \(x) 0 <= x && x <= 1,
  message = c(
    "{.arg {x_name}} must be in range [0, 1].",
    i = "{.arg {x_name}} is {x}."
  ),
  wrap_is_true = TRUE
)

check_between_0_1 <- new_check(
  predicate = \(x) 0 <= x && x <= 1,
  message = function(x, x_name) paste0("{.arg {x_name}} is no good."),
  wrap_is_true = TRUE
)

z <- 70
check_between_0_1(z)

check_test_body <- function(x, predicate, wrap_is_true) {
  specifyr_internal_error(x, "is.symbol")
  specifyr_internal_error(wrap_is_true, "is_bool", "rlang")

  # The `predicate` may already be a defused function, rather than a function
  # itself (i.e. when the caller is `check_test_body_multi`).
  defused_predicate <- rlang::enexpr(predicate)
  if (rlang::is_symbolic(predicate)) {
    test_call <- rlang::call2(predicate, x)
  } else if (is.function(predicate)) {
    test_call <- rlang::call2(defused_predicate, x)
  } else {
    cli::cli_abort(
      c(
        "{.arg predicate} must be a function or a symbolic object.",
        i = "{.arg predicate} is {.obj_type_friendly {predicate}}."
      ),
      .internal = TRUE
    )
  }

  if (wrap_is_true) {
    rlang::call2(quote(isTRUE), test_call)
  } else {
    test_call
  }
}

check_test_body_multi <- function(x, x_indices, predicate, wrap_is_true) {
  specifyr_internal_error(x_indices, "is.integer")

  if (!rlang::is_empty(x_indices)) {
    x <- rlang::call2("[", x, x_indices)
  }
  test_call <- check_test_body(rlang::sym("x"), rlang::enexpr(predicate), wrap_is_true)
  rlang::expr(all(vapply(!!x, function(x) { !!test_call }, logical(1L))))
}

# TODO: This is what I think the output should look like of:
# check(
#   predicate = \(x) 0 <= x && x <= 1,
#   message = c(
#      "{.arg {x_name}} must be in range [0, 1].",
#      i = "{.arg {x_name}} is {x}."
#   ),
#   wrap_is_true = TRUE
# )
check_between_0_1 <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_check_fail_error"
  ) {

  if (isTRUE((\(x) 0 <= x && x <= 1)(x))) {
    emit_check_error(
      x = x,
      x_name = x_name,
      x_index = list(),
      error_call = error_call,
      error_class = error_class,
      message = c(
        "{.arg {x_name}} must be in range [0, 1].",
        i = "{.arg {x_name}} is {x}."
      )
    )
  }
  x
}

# emission ---------------------------------------------------------------------

emit_check_error <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class,
    message
    ) {

  x_name <- paste0(x_name, format_index(x_index))
  if (is.function(message)) {
    args <- list(x = x, x_name = x_name)
    fmls <- names(evalidate_message_fmls(message, error_call = error_call))
    message_call <- rlang::call2(message, !!!args[fmls])
    message <- eval(message_call)
    stop_bad_check_message(message, is_fn_result = TRUE)
  } else {
    stop_bad_check_message(message, is_fn_result = FALSE)
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

evalidate_message_fmls <- function(message_fn, error_call = rlang::caller_env()) {
  fmls <- names(fn_fmls(message_fn))
  if (!all(fmls %in% c("x", "x_name"))) {
    bad_fmls <- fmls[fmls %notin% c("x", "x_name")]
    cli::cli_abort(
      c(
        "Can't evaluate {.cls specifyr_check} function.",
        i = "`message` must be a character or a function with formals `x` and/or `x_name`.",
        x = "`message` is a function with invalid formals {.arg {bad_fmls}}."
      ),
      call = error_call,
      class = "specifyr_malformed_check_error"
    )
  }
  fmls
}

stop_bad_check_message <- function(
    message,
    error_call = rlang::caller_env(),
    is_fn_result = FALSE
  ) {
  if (!is.character(message)) {
    error_msg <- if (is_fn_result) {
      "{.arg message} is a function which produced {.obj_type_friendly {message}}."
    } else {
      "{.arg message} is {.obj_type_friendly {message}}."
    }
    cli::cli_abort(
      c(
        "Can't evaluate {.cls specifyr_check} function.",
        i = "{.arg message} must be a {.cls character} vector or a function which produces a {.cls character} vector.",
        x = error_msg
      ),
      call = error_call,
      class = "specifyr_malformed_check_error"
    )
  }
}
