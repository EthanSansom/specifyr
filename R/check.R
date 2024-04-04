# class ------------------------------------------------------------------------

new_check <- function(check) {
  stopifnot(rlang::is_function(check))
  structure(check, class = "obj_check")
}

check <- function(predicate, message) {

  stop_wrong_vec(message, cls = "character", nas = FALSE)
  stopifnot(rlang::is_function(predicate))

  check_fn <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "objspec_error_mispecified"
  ) {

    results <- predicate(arg)
    if (!rlang::is_logical(results)) {
      cli::abort("`check` must return a logical vector.")
    }

    loc <- which(is.na(results) | !results)
    cli_env <- env(
      base_env(),
      arg = arg,
      arg_name = arg_name,
      loc = loc,
      at_loc = at_locations(loc)
    )

    if (!isTRUE(all(results))) {
      cli::cli_abort(
        message,
        call = error_call,
        class = c(error_class, "objspec_error"),
        .envir = cli_env
      )
    }
    TRUE
  }

  new_check(check_fn)

}

is_check <- function(x) inherits(x, "obj_check")

# internal ---------------------------------------------------------------------

check_cls <- function(
    arg,
    cls,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "objspec_error_mispecified"
) {

  if (!inherits(arg, cls)) {
    arg_cls <- class(arg)
    cli::cli_abort(
      "{.arg {arg_name}} must be class {.cls {cls}}, not class {.cls {arg_cls}}.",
      cls = cls, arg_cls = arg_cls,
      call = error_call,
      class = c(error_class, "objspec_error")
    )
  }

}

check_len <- function(
    arg,
    len,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "objspec_error_mispecified"
) {

  len_length <- length(len)
  if (len_length == 1) {
    check_len_exact(
      arg = arg,
      len = len,
      arg_name = arg_name,
      error_call = error_call,
      error_class = error_class
    )
  } else if (len_length == 2) {
    check_len_range(
      arg = arg,
      min_len = len[[1]],
      max_len = len[[2]],
      arg_name = arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }

}

check_len_exact <- function(
    arg,
    len,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "objspec_error_mispecified"
) {

  arg_len <- length(arg)
  if (arg_len != len) {
    cli::cli_abort(
      "{.arg {arg_name}} must be length {len}, not length {arg_len}.",
      len = len,
      arg_len = arg_len,
      call = error_call,
      class = c(error_class, "objspec_error")
    )
  }

}

check_len_range <- function(
    arg,
    min_len,
    max_len,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "objspec_error_mispecified"
) {

  arg_len <- length(arg)
  if (min_len > arg_len || arg_len > max_len) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must have length in range [{min_len}, {max_len}].",
        x = "{.arg {arg_name}} has length {arg_len}."
      ),
      min_len = min_len, max_len = max_len, arg_len = arg_len,
      call = error_call,
      class = c(error_class, "objspec_error")
    )
  }

}

check_nas <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "objspec_error_mispecified"
) {

  arg_nas <- is.na(arg)
  if (any(arg_nas)) {
    locations <- which(arg_nas)
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must contain no {.val {NA}} values.",
        x = "{.arg {arg_name}} is {.val {NA}} {at_locations(locations)}."
      ),
      locations = locations,
      call = error_call,
      class = c(error_class, "objspec_error")
    )
  }

}

check_nm1 <- function(
    actual_name,
    target_name,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "objspec_error_mispecified"
) {

  if (target_name == "") {
    if (actual_name == "") {
      return(TRUE)
    }
    cli::cli_abort(
      "{.arg {arg_name}} must be unnamed, not named {.val {actual_name}}.",
      call = error_call,
      class = c(error_class, "objspec_error")
    )
  }

  if (target_name != actual_name) {
    not <- if (actual_name == "") "be unamed." else "{.val {actual_name}}."
    cli::cli_abort(
      paste("{.arg {arg_name}} must be named {.val {target_name}}, not", not),
      call = error_call,
      class = c(error_class, "objspec_error")
    )
  }
  TRUE

}
