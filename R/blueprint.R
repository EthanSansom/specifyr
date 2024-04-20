
# Store information about the last blueprint run
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
    .Data = list(
      cls = .cls,
      len = .len,
      nas = .nas,
      opt = .opt,
      checks = .checks
    ),
    class = c("specifyr_vec_blueprint", "specifyr_obj_blueprint")
  )
}

lst_blueprint <- function(..., .len = NULL, .opt = FALSE, .checks = list()) {
  dots <- rlang::list2(...)
  stopifnot(all(purrr::map_lgl(dots, is_obj_blueprint)))
  structure(
    .Data = list(
      len = .len,
      opt = .opt,
      elements = compact_elements(dots),
      checks = .checks
    ),
    class = c(
      "specifyr_lst_blueprint",
      "specifyr_rec_blueprint",
      "specifyr_obj_blueprint"
    )
  )
}

is_lst_blueprint <- function(x) {
  inherits(x, "specifyr_lst_blueprint")
}

compact_elements <- function(elements) {

  # Don't need to compact if < 2 elements or if the last element is named
  i <- length(elements)
  nms <- rlang::names2(elements)
  if (i <= 1 || nms[[i]] != "") {
    return(elements)
  }

  # Remove identical same named elements from the tail of the blueprint
  last_element <- elements[[i]]
  while (i > 1) {
    if (nms[[i - 1]] != "") {
      break
    }
    if (!identical(last_element, elements[[i - 1]])) {
      break
    }
    i <- i - 1
  }
  vctrs::vec_slice(elements, seq(i))
}

# formatting -------------------------------------------------------------------

# TODO: The blueprint itself probably doesn't need a formating method... and
#       doing it this way makes everything more confusing. Why not just include
#       these in the specification formatting methods instead.

#' @export
abbreviation <- function(x) {
  UseMethod("abbreviation")
}

#' @export
format.specifyr_obj_blueprint <- function(x) {
  abbreviation(x)
}

#' @export
abbreviation.specifyr_obj_blueprint <- function(x) {
  paste0(
    "<",
    if (x$cls == "") "object" else fmt_cls(x$cls),
    fmt_opt(x$opt),
    fmt_checks(x$checks),
    ">"
  )
}

#' @export
format.specifyr_vec_blueprint <- function(x) {
  abbreviation(x)
}

#' @export
abbreviation.specifyr_vec_blueprint <- function(x) {
  paste0(
    "<",
    if (x$cls == "") "vector" else fmt_cls(x$cls),
    fmt_len(x$len),
    fmt_nas(x$nas),
    fmt_opt(x$opt),
    fmt_checks(x$checks),
    ">"
  )
}

#' @export
format.specifyr_lst_blueprint <- function(x, width = NULL) {

  width <- width %||% cli::console_width()

  prefix <- abbreviation(x)
  elements <- x$elements
  n_elements <- length(elements)

  if (n_elements == 0) {
    return(prefix)
  } else if (n_elements == 1) {
    element_abbr <- abbreviation(elements[[1]])
    is_within_width <- nchar(prefix) + nchar(element_abbr) + 5 <= width
    if (is_within_width) {
      return(paste0(prefix, "[of ", element_abbr, "]"))
    } else {
      return(paste0(prefix, "[...]"))
    }
  }

  # Indicate whether the last blueprint in a recursive blueprint was recycled
  elements_abbr <- purrr::map_chr(elements, abbreviation)
  if (last_element_recycled(x)) {
    elements_abbr[[n_elements]] <- paste0(elements_abbr[[n_elements]], "..+")
  }

  # Attempt to fit the text within `width` characters
  within_width <- (5 + cumsum(nchar(prefix) + nchar(elements_abbr))) <= width
  if (all(within_width)) {
    paste0(prefix, "[", commas(elements_abbr), "]")
  } else if (!any(within_width)) {
    paste0(prefix, "[...]")
  } else {
    paste0(prefix, "[", commas(elements_abbr[within_width]), ", ...]")
  }
}

#' @export
abbreviation.specifyr_lst_blueprint <- function(x) {
  paste0(
    "<list",
    fmt_len(x$len),
    fmt_opt(x$opt),
    fmt_checks(x$checks),
    ">"
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

  record_blueprint_assertion(blueprint)
  if (blueprint$opt && is.null(arg)) {
    record_blueprint_success()
    return(TRUE)
  }

  cls <- blueprint$cls
  if (is.null(cls)) check_cls(arg, cls = cls, arg_name, error_call, error_class)
  check_attatched(blueprint$checks, arg, arg_name, error_call, error_class)

  record_blueprint_success()
  TRUE
}

assert_blueprint.specifyr_vec_blueprint <- function(
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
  check_attatched(blueprint$checks, arg, arg_name, error_call, error_class)

  record_blueprint_success()
  TRUE
}

assert_blueprint.specifyr_lst_blueprint <- function(
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

  check_cls(arg, cls = "list", arg_name, error_call, error_class)
  check_len(arg, len = blueprint$len, arg_name, error_call, error_class)
  check_attatched(blueprint$checks, arg, arg_name, error_call, error_class)

  # Ensures that the outer list blueprint, and not the blueprint of an element,
  # is recorded as the last blueprint.
  on.exit(the$last_blueprint <- blueprint, add = TRUE)

  inner_blueprints <- blueprint$elements
  inner_names <- rlang::names2(inner_blueprints)
  n_inner_elems <- length(inner_blueprints)
  # Accessors allow the last element of the blueprint to be recycled as many
  # times as required.
  blueprint_at <- function(i) inner_blueprints[[min(i, n_inner_elems)]]
  target_name_at <- function(i) inner_names[[min(i, n_inner_elems)]]

  arg_names <- rlang::names2(arg)
  arg_indices <- indices(arg)
  for (i in seq_along(arg)) {
    # Updating the error blueprint manually, to record the inner blueprint
    # as a the error blueprint in the event of a `check_nm1` failure.
    inner_blueprint <- blueprint_at(i)
    the$error_blueprint <- inner_blueprint

    # Only checking the names at indices which are named in the blueprint. This
    # assumes that the user doesn't care about names that they didn't specify.
    inner_arg_name <- paste0(arg_name, as_index(arg_indices[[i]]))
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
      blueprint = inner_blueprint,
      arg = arg[[i]],
      arg_name = inner_arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }

  record_blueprint_success()
  TRUE
}

# helpers ----------------------------------------------------------------------

last_element_recycled <- function(x) {
  blueprint <- if (is_obj_blueprint(x)) x else blueprint(x)
  if (!inherits(blueprint, "specifyr_rec_blueprint")) {
    FALSE
  } else {
    !identical(blueprint$len, length(blueprint$elements))
  }
}
