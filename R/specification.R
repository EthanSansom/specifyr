new_spec <- function(blueprint) {
  structure(
    .Data = purrr::partial(assert_spec_blueprint, blueprint = !!blueprint),
    class = gsub("_blueprint$", "_spec", class(blueprint))
  )
}

#' @export
last_spec <- function() {
  last_blueprint <- the$last_blueprint
  if (!is.null(last_blueprint)) {
    new_spec(last_blueprint)
  } else {
    NULL
  }
}

#' @export
error_spec <- function() {
  error_blueprint <- the$error_blueprint
  if (!is.null(error_blueprint)) {
    new_spec(error_blueprint)
  } else {
    NULL
  }
}

# checks -----------------------------------------------------------------------

add_checks <- function(.spec, ...) {

  dots <- rlang::list2(...)
  are_checks <- purrr::map_lgl(dots, is_check)
  if (!all(are_checks)) {
    non_check_at <- which.min(are_checks)
    non_check_arg <- paste0('..', non_check_at)
    non_check <- dots[[non_check_at]]
    cli::cli_abort(
      c(
        "{.arg ...} must contain only {.cls specifyr_obj_check} objects.",
        x = "{.arg {non_check_arg}} is {.obj_type_friendly {non_check}}."
      )
    )
  }
  if (!is_obj_spec(.spec)) {
    cli::cli_abort(
      c(
        "{.arg .spec} must be a {.cls specifyr_obj_spec} object.",
        x = "{.arg .spec} is {.obj_type_friendly .spec}."
      )
    )
  }

  blueprint <- spec_blueprint(.spec)
  blueprint$check_blueprints <- append(
    blueprint$check_blueprints,
    purrr::map(dots, get_check_blueprint)
  )
  new_spec(blueprint)
}

# subsetting -------------------------------------------------------------------

# TODO Ethan:
# - divert subsetting to the inner blueprint$elements
# - add `names` and `length` methods which do the same
# - might want to create an `specifyr_rec_spec` (recursive specificication)
#   to inherit from here, so that we can define a common subsetting strategy?
