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
