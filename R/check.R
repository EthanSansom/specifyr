# class ------------------------------------------------------------------------

new_check <- function(check) {
  stopifnot(rlang::is_function(check))
  structure(check, class = "specifyr_obj_check")
}

# TODO Ethan:
# Add a `title` argument (by default rlang::caller_arg(.pred)) which can be used
# when formatting the check.
check <- function(.pred, .msg, .msg_env = rlang::base_env()) {

  stop_wrong_vec(.msg, cls = "character", nas = FALSE)
  if (!rlang::is_function(.pred)) {
    cli::cli_abort(
      "{.arg .pred} must be a function, not {.obj_type_friendly {(.pred)}.",
      class = c("specifyr_error_api", "specifyr_error")
    )
  }

  check_fn <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
  ) {

    # TODO Ethan: Improve this internal error
    results <- .pred(arg)
    if (!rlang::is_logical(results)) {
      cli::abort("Can't check {.arg {arg_name}}.")
    }

    loc <- which(is.na(results) | !results)
    cli_env <- rlang::env(
      .msg_env,
      arg = arg,
      arg_name = arg_name,
      loc = loc,
      at_loc = at_locations(loc)
    )

    if (!isTRUE(all(results))) {
      cli::cli_abort(
        c(.msg, error_spec_call_prompt(error_class)),
        call = error_call,
        class = c(error_class, "specifyr_error"),
        .envir = cli_env
      )
    }
    TRUE
  }

  new_check(check_fn)

}

is_check <- function(x) inherits(x, "specifyr_obj_check")

# internal ---------------------------------------------------------------------

check_cls <- function(
    arg,
    cls,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  if (!inherits(arg, cls)) {
    arg_cls <- class(arg)
    cli::cli_div(
      theme = list(
        .multi_cls = list(
          color = "blue",
          before = "<",
          after = ">",
          `vec-trunc` = 5,
          `vec-sep` = ", ",
          `vec-sep2` = " or ",
          `vec-last` = " or "
        )
      )
    )
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must be class {.multi_cls {cls}}, not class {.cls {arg_cls}}.",
        error_spec_call_prompt(error_class)
      ),
      cls = cls,
      arg_cls = arg_cls,
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }

}

check_vctr <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
  ) {

  if (!vctrs::obj_is_vector(arg)) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must be a vector, not {.obj_type_friendly {arg}}.",
        error_spec_call_prompt(error_class)
      ),
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }

}

check_len <- function(
    arg,
    len,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
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
    error_class = "specifyr_error_object_mispecified"
) {

  arg_len <- length(arg)
  if (arg_len != len) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must be length {len}, not length {arg_len}.",
        error_spec_call_prompt(error_class)
      ),
      len = len,
      arg_len = arg_len,
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }

}

check_len_range <- function(
    arg,
    min_len,
    max_len,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  arg_len <- length(arg)
  if (min_len > arg_len || arg_len > max_len) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must have length in range [{min_len}, {max_len}].",
        x = "{.arg {arg_name}} has length {arg_len}.",
        error_spec_call_prompt(error_class)
      ),
      min_len = min_len, max_len = max_len, arg_len = arg_len,
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }

}

check_nas <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  arg_nas <- is.na(arg)
  if (any(arg_nas)) {
    locations <- which(arg_nas)
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must contain no {.val {NA}} values.",
        x = "{.arg {arg_name}} is {.val {NA}} {at_locations(locations)}.",
        error_spec_call_prompt(error_class)
      ),
      locations = locations,
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }

}

check_nm1 <- function(
    actual_name,
    target_name,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  if (target_name == "") {
    if (actual_name == "") {
      return(TRUE)
    }
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must be unnamed, not named {.val {actual_name}}.",
        error_spec_call_prompt(error_class)
      ),
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }

  if (target_name != actual_name) {
    not <- if (actual_name == "") "be unamed." else "{.val {actual_name}}."
    cli::cli_abort(
      c(
        paste("{.arg {arg_name}} must be named {.val {target_name}}, not", not),
        error_spec_call_prompt(error_class)
      ),
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }
  TRUE

}

check_attatched <- function(
    checks,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  for (predicate in checks) {
    predicate(
      arg = arg,
      arg_name = arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }

}

# helpers ----------------------------------------------------------------------

# Output message contains a reference `{arg_name}` without knowing anything
# about `arg_name`, which is a little suspect...
error_spec_call_prompt <- function(error_class) {
  correct_class <- isTRUE(error_class == "specifyr_error_object_mispecified")
  interactive <- rlang::is_interactive()
  if (correct_class && interactive) {
    cli::col_silver(paste0(
      "Run {.run specifyr::error_spec()} to ",
      "get the expected specificiation of {.arg {arg_name}}."
    ))
  }
}
