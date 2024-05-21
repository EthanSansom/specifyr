# user -------------------------------------------------------------------------

stop_wrong_vec <- function(
    arg,
    cls,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    missing = FALSE,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {
  if ((missing && rlang::is_missing(arg)) || (null && is.null(arg))) {
    return(invisible())
  }
  stop_wrong_cls()
  stop_wrong_len()
  stop_contains_nas()
}

stop_wrong_cls <- function(
    arg,
    cls,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {

}

stop_wrong_len <- function(
    arg,
    cls,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {

}

stop_contains_nas <- function(
    arg,
    cls,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {

}

# TODO Ethan: Improve this function. Look at some other packages for how you
# might want to standardize your errors in a better way. Post debugging, you'll
# probably want to strip down the internal errors as well.
stop_must <- function(
    must,
    info = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {
  cli::cli_abort(
    c(must, x = info),
    call = error_call,
    class = error_class
    # .envir = rlang::caller_env()
  )
}

specifyr_error <- function(
    ...,
    .error_call = rlang::caller_env(),
    .error_class = "specifyr_error_api"
  ) {
  cli::cli_abort(
    c(...),
    call = .error_call,
    class = .error_class,
    .envir = rlang::caller_env()
  )
}

# check ------------------------------------------------------------------------

check_is_string <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
    ) {

  if (!rlang::is_string(x)) {
    x_is <- if (is.character(x)) {
      x_len <- length(x)
      if (x_len == 1) {
        "NA."
      } else {
        "length {x_len} character."
      }
    } else {
      "{.obj_type_friendly {x}}."
    }
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a non-NA length-1 character.",
        x = paste("{.arg {x_name}} is", x_is)
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_bool <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
  ) {

  if (!rlang::is_bool(x)) {
    x_is <- if (is.logical(x)) {
      x_len <- length(x)
      if (x_len == 1) {
        "NA."
      } else {
        "length {x_len} logical vector."
      }
    } else {
      "{.obj_type_friendly {x}}."
    }
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a `TRUE` or `FALSE` value.",
        x = paste("{.arg {x_name}} is", x_is)
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_env <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {

  if (!is.environment(x)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be an environment.",
        x = paste("{.arg {x_name}} is {.obj_type_friendly {x}}.")
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_spec <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
  ) {
   if (!is_object_spec(x)) {
     cli::cli_abort(
       c(
         "{.arg {x_name}} must be a {.cls obj_spec} object.",
         x = paste("{.arg {x_name}} is {.obj_type_friendly {x}}.")
       ),
       call = error_call,
       class = error_class
     )
   }
  x
}

check_is_check <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
  ) {

  if (!is_check(x)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a {.cls specifyr_check}.",
        x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

# internal ---------------------------------------------------------------------

# This is inspired by `assertthat` (https://github.com/hadley/assertthat). While
# it feels a little illegal, does make for some more informative internal errors
# for my benefit while keeping the call terse.

specifyr_internal_error_if_not <- function(
    predicate,
    message,
    error_call = rlang::caller_env()
  ) {
  if (!isTRUE(predicate)) {
    cli::cli_abort(
      message,
      call = error_call,
      .internal = TRUE
    )
  }
}

specifyr_internal_error <- function(
    x,
    fn_name,
    fn_ns = "base",
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {
  # `quote(arg)` prevents a confusing chain of events: If `x = sym("y")` then
  # `call2(is.symbol, x)` produces `is.symbol(y)` instead of `is.symbol(x)`.
  test_call <- rlang::call2(fn_name, quote(x), .ns = fn_ns)
  if (!isTRUE(eval(test_call))) {
    cli::cli_abort(
      specifyr_internal_error_message(arg_name, fn_name),
      call = error_call,
      .internal = TRUE
    )
  }
}

# See how `assertthat` does this. You can do something much less fancy... since
# this is just for internal error handling.
specifyr_internal_error_message <- function(arg_name, fn_name) {
  expected <- switch(
    fn_name,
    # base::
    "is.numeric" = "a numeric vector.",
    "is.integer" = "an integer vector.",
    "is.logical" = "a logical vector.",
    "is.character" = "a character vector.",
    "is.list" = "a list.",
    "is.symbol" = "a symbol.",
    "is.function" = "a function.",
    "is.call" = "a call.",
    # rlang::
    "is_bool" = "a single TRUE of FALSE value.",
    "is_string" = "a non-NA length-1 character.",
    "is_integerish" = "an integerish number.",
    "is_scalar_integerish" = "a length-1 integerish number.",
    "is_quosure" = "a quosure.",
    "is_expression" = "a defused expression.",
    cli::cli_abort(
      "Can't construct message for `fn_name = {.val {fn_name}}`.",
      .internal = TRUE
    )
  )
  c(
    paste0("{.arg {x_name}} must be ", expected),
    i = "{.arg {x_name}} is {.obj_type_friendly {x}}."
  )
}
