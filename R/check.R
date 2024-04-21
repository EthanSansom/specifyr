check <- function(
    p,
    msg,
    title = rlang::caller_arg(p),
    msg_env = rlang::caller_env()
) {

  stop_wrong_vec(msg, cls = "character", nas = FALSE)
  stop_wrong_vec(title, cls = "character", len = 1L, nas = FALSE)
  if (!rlang::is_environment(msg_env)) {
    cli::cli_abort(
      paste0(
        "{.arg msg_env} must be an environment, ",
        "not {.obj_type_friendly {(msg_env)}."
      ),
      class = c("specifyr_error_api", "specifyr_error")
    )
  }
  if (!(rlang::is_function(p) || rlang::is_formula(p, lhs = FALSE))) {
    not <- if (rlang::is_formula(p)) {
      "a two sided formula."
    } else {
      "{.obj_type_friendly {(p)}}."
    }
    cli::cli_abort(
      paste("{.arg p} must be a function or a one sided formula, not", not),
      class = c("specifyr_error_api", "specifyr_error")
    )
  }

  blueprint <- check_blueprint(
    p = rlang::as_function(p),
    msg = msg,
    title = title,
    msg_env = msg_env
  )
  rm(p, msg, msg_env)
  structure(
    .Data = purrr::partial(assert_check_blueprint, blueprint = !!blueprint),
    title = title,
    class = "specifyr_obj_check"
  )

}

is_check <- function(x) {
  inherits(x, "specifyr_obj_check")
}

check_must <- function(
    p,
    must,
    title = rlang::caller_arg(p),
    msg_env = rlang::base_env()
) {

  stop_wrong_vec(must, "character", len = 1L, nas = FALSE)
  msg <- paste0("{.arg {arg_name}} must ", must, ".")
  rlang::try_fetch(
    check(
      p = p,
      msg = msg,
      title = title,
      msg_env = msg_env
    ),
    error = function(cnd) cli::cli_abort("", parent = cnd, .inherit = TRUE)
  )
}

check_must_not <- function(
    p,
    must,
    not,
    title = rlang::caller_arg(p),
    msg_env = rlang::base_env()
) {

  stop_wrong_vec(must, "character", len = 1L, nas = FALSE)
  stop_wrong_vec(not, "character", len = 1L, nas = FALSE)
  msg <- paste0("{.arg {arg_name}} must ", must, ", not ", not, ".")
  rlang::try_fetch(
    check(
      p = p,
      msg = msg,
      title = title,
      msg_env = msg_env
    ),
    error = function(cnd) cli::cli_abort("", parent = cnd, .inherit = TRUE)
  )
}

check_blueprint <- function(p, msg, title, msg_env) {
  stopifnot(rlang::is_function(p))
  stopifnot(rlang::is_character(msg))
  stopifnot(rlang::is_scalar_character(title))
  stopifnot(rlang::is_environment(msg_env))
  structure(
    .Data = p,
    message = msg,
    title = title,
    msg_env = msg_env,
    class = "specifyr_obj_check_blueprint"
  )
}

get_check_blueprint <- function(x) {
  x_env <- rlang::fn_env(x)
  x_env$blueprint
}

# formatting -------------------------------------------------------------------

#' @export
format.specifyr_obj_check <- function(x) {
  format(get_check_blueprint(x))
}

#' @export
format.specifyr_obj_check_blueprint <- function(x) {
  format(attr(x, "title"))
}

#' @export
print.specifyr_obj_check <- function(x) {
  cli::cat_line("<obj_check>")
  cli::cat_line(format(x))
  invisible(x)
}

# assert -----------------------------------------------------------------------

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

  locations <- which(is.na(results) | !results)
  cli_env <- rlang::env(
    attr(blueprint, "msg_env"),
    arg = arg,
    arg_name = arg_name,
    loc = locations,
    at_loc = at_locations(locations)
  )

  cli::cli_abort(
    c(attr(blueprint, "msg"), error_spec_call_prompt(error_class)),
    call = error_call,
    class = c(error_class, "specifyr_error"),
    .envir = cli_env
  )

}
