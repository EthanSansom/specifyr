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

# internal ---------------------------------------------------------------------

# This is inspired by `assertthat` (https://github.com/hadley/assertthat). While
# it feels a little illegal, does make for some more informative internal errors
# for my benefit while keeping the call terse.

specifyr_internal_error <- function(
    arg,
    fn_name,
    fn_ns = "base",
    arg_name = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {
  # `quote(arg)` prevents a confusing chain of events: If `arg = sym("x")` then
  # `call2(is.symbol, arg)` produces `is.symbol(x)` instead of `is.symbol(arg)`.
  test_call <- rlang::call2(fn_name, quote(arg), .ns = fn_ns)
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
    # rlang::
    "is_bool" = "a single TRUE of FALSE value.",
    "is_string" = "a non-NA length-1 character.",
    "is_integerish" = "an integerish number.",
    "is_scalar_integerish" = "a length-1 integerish number.",
    cli::cli_abort(
      "Can't construct message for `fn_name = {.val {fn_name}}`.",
      .internal = TRUE
    )
  )
  c(
    paste("{.arg {arg_name}} must be", expected),
    i = "{.arg {arg_name}} is {.obj_type_friendly {x}}."
  )
}
