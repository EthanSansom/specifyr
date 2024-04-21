stop_wrong_vec <- function(
    arg,
    cls,
    len = NULL,
    nas = TRUE,
    opt = FALSE,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {

  if (opt && is.null(arg)) {
    return(invisible())
  }
  check_cls(arg, cls, arg_name, error_call, error_class)
  if (!is.null(len)) check_len(arg, len, arg_name, error_call, error_class)
  if (!nas) check_nas(arg, arg_name, error_call, error_class)

}

stop_not_integerish <- function(
    arg,
    len = NULL,
    nas = TRUE,
    opt = FALSE,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
  ) {

  if (opt && is.null(arg)) {
    return(invisible())
  }
  if (!is.null(len)) check_len(arg, len, arg_name, error_call, error_class)
  if (!nas) check_nas(arg, arg_name, error_call, error_class)
  if (!rlang::is_integerish(arg)) {
    must <- if (!is.numeric(arg)) {
      "be an integer"
    } else {
      "be coercible to an integer without loss of precision"
    }
    cli::cli_abort(
      "{.arg {arg_name}} must {must}.",
      call = error_call,
      class = error_class
    )
  }
}
