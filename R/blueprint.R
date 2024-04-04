# blueprint --------------------------------------------------------------------

obj_blueprint <- function(.cls = "", .opt = FALSE, .checks = list()) {
  structure(
    .Data = list(cls = .cls, opt = .opt, checks = .checks),
    class = "specifyr_obj_blueprint"
  )
}

is_obj_blueprint <- function(x) {
  inherits(x, "specifyr_obj_blueprint")
}

vec_blueprint <- function(
    .cls = "",
    .len = NULL,
    .nas = TRUE,
    .opt = FALSE,
    .checks = list()
  ) {
  structure(
    .Data = list(cls = .cls, len = .len, nas = .nas, opt = .opt, checks = .checks),
    class = c("specifyr_vec_blueprint", "specifyr_obj_blueprint")
  )
}

lst_blueprint <- function(..., .len = NULL, .opt = FALSE, .checks = list()) {
  dots <- rlang::list2(...)
  stopifnot(all(purrr::map_lgl(dots, is_obj_blueprint)))
  structure(
    .Data = list(len = .len, opt = .opt, elements = dots, checks = .checks),
    class = c("specifyr_lst_blueprint", "specifyr_obj_blueprint")
  )
}

# assert -----------------------------------------------------------------------

assert_blueprint <- function(
    blueprint,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {
  UseMethod("assert_blueprint")
}

assert_blueprint.specifyr_obj_blueprint <- function(
    blueprint,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  if (blueprint$opt && is.null(arg)) {
    return(TRUE)
  }

  cls <- blueprint$cls
  if (cls != "") check_cls(arg, cls = cls, arg_name, error_call, error_class)
  check_attatched(blueprint$checks, arg, arg_name, error_call, error_class)
  TRUE

}

assert_blueprint.specifyr_vec_blueprint <- function(
    blueprint,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  if (blueprint$opt && is.null(arg)) {
    return(TRUE)
  }

  # If `cls` is left blank, assert that `arg` is a vector.
  cls <- blueprint$cls
  if (cls == "") {
    check_vctr(arg, arg_name, error_call_error_class)
  } else {
    check_cls(arg, cls = cls, arg_name, error_call, error_class)
  }

  check_len(arg, len = blueprint$len, arg_name, error_call, error_class)
  if (!blueprint$nas) check_nas(arg, arg_name, error_call, error_class)
  check_attatched(blueprint$checks, arg, arg_name, error_call, error_class)
  TRUE

}

assert_blueprint.specifyr_lst_blueprint <- function(
    blueprint,
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_object_mispecified"
) {

  if (blueprint$opt && is.null(arg)) {
    return(TRUE)
  }

  # Check the class, length, and attached predicates of the outer list.
  check_cls(arg, cls = "list", arg_name, error_call, error_class)
  check_len(arg, len = blueprint$len, arg_name, error_call, error_class)
  check_attatched(blueprint$checks, arg, arg_name, error_call, error_class)

  # Prepare accessors for the required names and blueprints of list elements.
  inner_blueprints <- blueprint$elements
  inner_names <- rlang::names2(inner_blueprints)
  n_inner_elems <- length(inner_blueprints)
  blueprint_at <- function(i) inner_blueprints[[min(i, n_inner_elems)]]
  target_name_at <- function(i) inner_names[[min(i, n_inner_elems)]]

  # Recursively check the elements of the list.
  arg_names <- rlang::names2(arg)
  arg_indices <- indices(arg)
  for (i in seq_along(arg)) {
    inner_arg_name <- paste0(arg_name, as_index(arg_indices[[i]]))
    # TODO Ethan: Decide which names we should care about. Currently, I only
    #             bother checking the name of an inner object if the specification
    #             (blueprint$elements) is named AT THAT INDEX.
    target_name <- target_name_at(i)
    if (target_name != "") {
      check_nm1(
        actual_name = arg_names[[i]],
        target_name = target_name_at(i),
        arg_name = inner_arg_name,
        error_call = error_call,
        error_class = error_class
      )
    }
    assert_blueprint(
      blueprint = blueprint_at(i),
      arg = arg[[i]],
      arg_name = inner_arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  TRUE

}

# attatching -------------------------------------------------------------------

attatch <- function(.spec, ...) {

  dots <- rlang::list2(...)
  stopifnot(all(purrr::map_lgl(dots, is_check)))
  stopifnot(is_obj_spec(.spec))

  new_blueprint <- blueprint(.spec)
  new_blueprint$attatched <- append(new_blueprint$attatched, dots)

  new_spec(new_blueprint)
}

check_attatched <- function(
    attatched,
    arg,
    arg_name = caller_arg(arg),
    error_call = caller_env(),
    error_class = "objspec_error_mispecified"
) {

  for (predicate in attatched) {
    predicate(
      arg = arg,
      arg_name = arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }

}
