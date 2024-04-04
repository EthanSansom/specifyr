stop_wrong_vec <- function(
    arg,
    cls,
    len = NULL,
    nas = TRUE,
    opt = FALSE,
    arg_name = caller_arg(arg),
    error_call = caller_env(),
    error_class = "objspec_error_api"
) {

  if (opt && is.null(arg)) {
    return(invisible())
  }
  check_cls(arg, cls, arg_name, error_call, error_class)
  if (!is.null(len)) check_len(arg, len, arg_name, error_call, error_class)
  if (!nas) check_nas(arg, arg_name, error_call, error_class)

}
