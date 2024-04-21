# the --------------------------------------------------------------------------

the <- rlang::new_environment(
  data = list(
    last_blueprint = NULL,
    error_blueprint = NULL,
    previous_error_blueprint = NULL
  )
)

record_blueprint_assertion <- function(blueprint) {
  the$previous_error_blueprint <- the$error_blueprint
  the$last_blueprint <- blueprint
  the$error_blueprint <- blueprint
}

record_blueprint_success <- function() {
  the$error_blueprint <- the$previous_error_blueprint
}

# specification ----------------------------------------------------------------

obj_spec <- function(.cls = NULL, .opt = FALSE) {
  stop_wrong_vec(.cls, cls = "character", nas = FALSE, opt = TRUE)
  stop_wrong_vec(.opt, cls = "logical", len = 1, nas = FALSE)

  new_spec(obj_blueprint(.cls = .cls, .opt = .opt))
}

is_obj_spec <- function(x) {
  inherits(x, "specifyr_obj_spec")
}

obj_blueprint <- function(
    .cls = NULL,
    .opt = FALSE,
    .check_blueprints = list()
  ) {
  structure(
    .Data = list(cls = .cls, opt = .opt, check_blueprints = .check_blueprints),
    class = "specifyr_obj_blueprint"
  )
}

is_obj_blueprint <- function(x) {
  inherits(x, "specifyr_obj_blueprint")
}

spec_blueprint <- function(x) {
  x_env <- rlang::fn_env(x)
  x_env$blueprint
}

spec_checks <- function(x) {
  blueprint <- spec_blueprint(x)
  blueprint$check_blueprints
}

# formatting -------------------------------------------------------------------

# Since recursive specifications keep only the blueprint of specifications that
# they contain (and not the specifications themselves), we must be able to format
# specifications using only their blueprint.

#' @export
abbreviation <- function(x) {
  UseMethod("abbreviation")
}

#' @export
abbreviation.specifyr_obj_spec <- function(x) {
  abbreviation(spec_blueprint(x))
}

#' @export
abbreviation.specifyr_obj_blueprint <- function(x) {
  paste0(
    "<",
    if (is.null(x$cls)) "object" else fmt_cls(x$cls),
    fmt_opt(x$opt),
    fmt_checks(x$check_blueprints),
    ">"
  )
}

#' @export
format.specifyr_obj_spec <- function(x) {
  abbreviation(x)
}

#' @export
format.specifyr_obj_blueprint <- function(x) {
  abbreviation(x)
}

#' @export
print.specifyr_obj_spec <- function(x) {
  checks <- spec_checks(x)
  cli::cat_line(c(
    "<obj_spec>",
    cli::cat_line(numbered(format(x)))
  ))
  if (!rlang::is_empty(checks)) {
    cat("\n")
    print_checks(checks)
  }
  invisible(x)
}

print_checks <- function(checks) {
  n_checks <- length(checks)
  header <- if (n_checks == 1) {
    "[*] 1 Additional Check:"
  } else {
    paste("[*]", n_checks, "Additional Checks:")
  }
  cli::cat_line(c(
    style_subtle(header),
    lettered(purrr::map_chr(checks, format))
  ))
}

# assert -----------------------------------------------------------------------

assert_spec_blueprint <- function(
    blueprint,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {
  UseMethod("assert_spec_blueprint")
}

assert_spec_blueprint.specifyr_obj_blueprint <- function(
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
  if (is.null(cls)) check_cls(arg, cls = cls, arg_name, error_call, error_class)
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

execute_checks <- function(
    check_blueprints,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  for (check_blueprint in check_blueprints) {
    assert_check_blueprint(
      check_blueprint,
      arg = arg,
      arg_name = arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }

}
