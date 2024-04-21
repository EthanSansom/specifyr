# specification ----------------------------------------------------------------

vec_spec <- function(.cls = NULL, .len = NULL, .nas = TRUE, .opt = FALSE) {
  stop_wrong_vec(.cls, cls = "character", nas = FALSE, opt = TRUE)
  stop_not_integerish(.len, len = 1:2, nas = FALSE, opt = TRUE)
  stop_wrong_vec(.nas, cls = "logical", len = 1, nas = FALSE)
  stop_wrong_vec(.opt, cls = "logical", len = 1, nas = FALSE)

  .len <- as.integer(.len)
  new_spec(vec_blueprint(.cls = .cls, .len = .len, .nas = .nas, .opt = .opt))
}

is_vec_spec <- function(x) {
  inherits(x, "specifyr_vec_spec")
}

vec_blueprint <- function(
    .cls = NULL,
    .len = NULL,
    .nas = TRUE,
    .opt = FALSE,
    .check_blueprints = list()
) {
  structure(
    .Data = list(
      cls = .cls,
      len = .len,
      nas = .nas,
      opt = .opt,
      check_blueprints = .check_blueprints
    ),
    class = c("specifyr_vec_blueprint", "specifyr_obj_blueprint")
  )
}

is_vec_blueprint <- function(x) {
  inherits(x, "specifyr_vec_blueprint")
}

# formatting -------------------------------------------------------------------

#' @export
abbreviation.specifyr_vec_spec <- function(x) {
  abbreviation(spec_blueprint(x))
}

#' @export
abbreviation.specifyr_vec_blueprint <- function(x) {
  paste0(
    "<",
    if (is.null(x$cls)) "vector" else fmt_cls(x$cls),
    fmt_len(x$len),
    fmt_nas(x$nas),
    fmt_opt(x$opt),
    fmt_checks(x$check_blueprints),
    ">"
  )
}

#' @export
format.specifyr_vec_spec <- function(x) {
  abbreviation(x)
}

#' @export
print.specifyr_vec_spec <- function(x) {
  checks <- spec_checks(x)
  cli::cat_line(c(
    "<vec_spec>",
    numbered(format(x))
  ))
  if (!rlang::is_empty(checks)) {
    cat("\n")
    print_checks(checks)
  }
  invisible(x)
}

# assert -----------------------------------------------------------------------

assert_spec_blueprint.specifyr_vec_blueprint <- function(
    blueprint,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  record_blueprint_assertion(blueprint)
  if (blueprint$opt && is.null(arg)) {
    record_blueprint_success()
    return(TRUE)
  }

  cls <- blueprint$cls
  if (is.null(cls)) {
    check_vctr(arg, arg_name, error_call_error_class)
  } else {
    check_cls(arg, cls = cls, arg_name, error_call, error_class)
  }
  check_len(arg, len = blueprint$len, arg_name, error_call, error_class)
  if (!blueprint$nas) check_nas(arg, arg_name, error_call, error_class)
  execute_checks(
    blueprint$check_blueprints,
    arg = arg,
    arg_name = arg_name,
    error_call = error_call,
    error_class = error_class
  )

  record_blueprint_success()
  TRUE
}
