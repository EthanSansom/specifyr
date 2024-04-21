# class ------------------------------------------------------------------------

check_blueprint <- function(predicate, message, title, message_env) {

  stopifnot(rlang::is_function(predicate))
  stopifnot(rlang::is_character(message))
  stopifnot(rlang::is_scalar_character(title))
  stopifnot(rlang::is_environment(message_env))
  structure(
    .Data = predicate,
    message = message,
    title = title,
    message_env = message_env,
    class = "specifyr_obj_check_blueprint"
  )

}

#' @export
format.specifyr_obj_check_blueprint <- function(x) {
  format(attr(x, "title"))
}

check <- function(
  .p,
  .msg,
  .title = rlang::caller_arg(.p),
  .msg_env = rlang::caller_env()
  ) {

  stop_wrong_vec(.msg, cls = "character", nas = FALSE)
  stop_wrong_vec(.title, cls = "character", len = 1L, nas = FALSE)
  if (!rlang::is_environment(.msg_env)) {
    cli::cli_abort(
      paste0(
        "{.arg .msg_env} must be an environment, ",
        "not {.obj_type_friendly {(.msg_env)}."
      ),
      class = c("specifyr_error_api", "specifyr_error")
    )
  }
  if (!(rlang::is_function(.p) || rlang::is_formula(.p, lhs = FALSE))) {
    not <- if (rlang::is_formula(.p)) {
      "a two sided formula."
    } else {
      "{.obj_type_friendly {(.p)}}."
    }
    cli::cli_abort(
      paste("{.arg .p} must be a function or a one sided formula, not", not),
      class = c("specifyr_error_api", "specifyr_error")
    )
  }

  blueprint <- check_blueprint(
    predicate = rlang::as_function(.p),
    message = .msg,
    title = .title,
    message_env = .msg_env
  )
  rm(.p, .msg, .msg_env)
  structure(
    .Data = purrr::partial(assert_check_blueprint, blueprint = !!blueprint),
    title = .title,
    class = "specifyr_obj_check"
  )

}

get_check_blueprint <- function(x) {
  x_env <- rlang::fn_env(x)
  x_env$blueprint
}

assert_check_blueprint <- function(blueprint, ...) {
  UseMethod("assert_check_blueprint")
}

assert_check_blueprint.specifyr_obj_check_blueprint <- function(
    blueprint,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_failed_check"
  ) {

  results <- blueprint(arg)
  if (!rlang::is_logical(results)) {
    cli::cli_warn(
      paste0(
        "Checking argument {.arg {arg_name}} produced a {.cls {class(results)}} ",
        "vector. Coercing to a {.cls logical} vector."
      ),
      call = error_call,
      class = c("specifyr_warning_api", "specifyr_warning")
    )
    results <- as.logical(results)
  }

  if (all(results)) {
    return(TRUE)
  }

  loc <- which(is.na(results) | !results)
  cli_env <- rlang::env(
    attr(blueprint, "message_env"),
    arg = arg,
    arg_name = arg_name,
    loc = loc,
    at_loc = at_locations(loc)
  )

  cli::cli_abort(
    c(attr(blueprint, "message"), error_spec_call_prompt(error_class)),
    call = error_call,
    class = c(error_class, "specifyr_error"),
    .envir = cli_env
  )

}

is_check <- function(x) inherits(x, "specifyr_obj_check")

#' @export
format.specifyr_obj_check <- function(x) {
  attr(x, "title")
}

#' @export
print.specifyr_obj_check <- function(x) {
  cli::cat_line("<obj_check>")
  cli::cat_line(format(x))
  invisible(x)
}

check_must <- function(
    .p,
    .must,
    .title = rlang::caller_arg(.p),
    .msg_env = rlang::base_env()
  ) {

  stop_wrong_vec(.must, "character", len = 1L, nas = FALSE)
  msg <- paste0("{.arg {arg_name}} must ", .must, ".")
  rlang::try_fetch(
    check(
      .p = .p,
      .msg = msg,
      .title = .title,
      .msg_env = .msg_env
    ),
    error = function(cnd) cli::cli_abort("", parent = cnd, .inherit = TRUE)
  )
}

check_must_not <- function(
    .p,
    .must,
    .not,
    .title = rlang::caller_arg(.p),
    .msg_env = rlang::base_env()
) {

  stop_wrong_vec(.must, "character", len = 1L, nas = FALSE)
  stop_wrong_vec(.not, "character", len = 1L, nas = FALSE)
  msg <- paste0("{.arg {arg_name}} must ", .must, ", not ", .not, ".")
  rlang::try_fetch(
    check(
      .p = .p,
      .msg = msg,
      .title = .title,
      .msg_env = .msg_env
    ),
    error = function(cnd) cli::cli_abort("", parent = cnd, .inherit = TRUE)
  )
}

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

  if (!vctrs::obj_is_vector(arg) || inherits(arg, "data.frame")) {
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

check_attached <- function(
    checks,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  for (check in checks) {
    assert_check_blueprint(
      check,
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
