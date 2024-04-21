# TODO Ethan:
# - spring cleaning, for ease of organization I think it's easier to have `vec`, `lst`, `obj`
#   each as their own scripts. Declare all of the required methods in `obj_spec`,
#   but have their specific implementations (i.e. print.specifyr_lst_spec) in the
#   correct file

# spec -------------------------------------------------------------------------
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

obj_spec <- function(.cls = NULL, .opt = FALSE) {
  stop_wrong_vec(.cls, cls = "character", nas = FALSE, opt = TRUE)
  stop_wrong_vec(.opt, cls = "logical", len = 1, nas = FALSE)

  blueprint <- obj_blueprint(.cls, .opt)
  new_spec(blueprint)
}

vec_spec <- function(.cls = NULL, .len = NULL, .nas = TRUE, .opt = FALSE) {
  stop_wrong_vec(.cls, cls = "character", nas = FALSE, opt = TRUE)
  stop_wrong_vec(.len, cls = "integer", len = 1:2, nas = FALSE, opt = TRUE)
  stop_wrong_vec(.nas, cls = "logical", len = 1, nas = FALSE)
  stop_wrong_vec(.opt, cls = "logical", len = 1, nas = FALSE)

  blueprint <- vec_blueprint(.cls, .len, .nas, .opt)
  new_spec(blueprint)
}

is_vec_spec <- function(x) {
  inherits(x, "specifyr_vec_spec")
}

lst_spec <- function(..., .len = NULL, .opt = FALSE) {
  dots <- rlang::list2(...)
  for (i in seq_along(dots)) {
    check_cls(
      arg = dots[[i]],
      cls = "specifyr_obj_spec",
      arg_name = paste0("..", i),
      error_class = "specifyr_error_api"
    )
  }
  stop_wrong_vec(.len, "integer", len = 1:2, nas = FALSE, opt = TRUE)
  stop_wrong_vec(.opt, "logical", len = 1, nas = FALSE)
  stop_incompatible_dots_length(dots, .len)

  blueprint <- lst_blueprint(
    !!!purrr::map(dots, blueprint),
    .len = .len,
    .opt = .opt
  )
  new_spec(blueprint)
}

is_lst_spec <- function(x) {
  inherits(x, "specifyr_lst_spec")
}

is_rec_spec <- function(x) {
  inherits(x, "specifyr_rec_spec")
}

stop_incompatible_dots_length <- function(
    dots,
    .len,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {

  if (is.null(.len)) {
    return(invisible())
  }

  n_dots <- length(dots)
  if (!identical(n_dots, .len)) {
    max_dots <- .len[[1]]
    if (n_dots > max_dots) {
      is_length <- switch(
        len_type(.len),
        range = paste0("has length in range [", .len[[1]], "-", .len[[2]], "]"),
        exact = paste0("is of length ", .len[[1]])
      )
      cli::cli_abort(
        c(
          "Can't specify an object which contains {n_dots} elements and {is_length}.",
          i = "{n_dots} elements were supplied to {.arg ...}."
        ),
        call = error_call,
        class = error_class
      )
    }
  }
}

is_obj_spec <- function(x) inherits(x, "specifyr_obj_spec")

blueprint <- function(x) {
  x_env <- rlang::fn_env(x)
  x_env$blueprint
}

add_check <- function(.spec, ...) {

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

  spec_blueprint <- blueprint(.spec)
  check_blueprints <- purrr::map(dots, get_check_blueprint)
  spec_blueprint$checks <- append(spec_blueprint$checks, check_blueprints)
  new_spec(spec_blueprint)
}

attatched_checks <- function(x) {
  blueprint <- blueprint(x)
  if ("checks" %notin% rlang::names2(blueprint)) {
    NULL
  } else {
    blueprint$checks
  }
}

# printing ---------------------------------------------------------------------

#' @export
format.specifyr_obj_spec <- function(x, ...) {
  format(blueprint(x), ...)
}

#' @export
abbreviation.specifyr_obj_spec <- function(x, ...) {
  abbreviation(blueprint(x), ...)
}

#' @export
print.specifyr_obj_spec <- function(x) {
  checks <- attatched_checks(x)
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

#' @export
print.specifyr_vec_spec <- function(x) {
  checks <- attatched_checks(x)
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

#' @export
print.specifyr_lst_spec <- function(x, width = NULL) {
  checks <- attatched_checks(x)
  width <- width %||% cli::console_width()
  cli::cat_line(c(
    "<lst_spec>",
    numbered(format_lst_blueprint_elements(blueprint(x)), from = 0)
  ))
  if (!rlang::is_empty(checks)) {
    cat("\n")
    print_checks(checks)
  }
  invisible(x)
}

format_lst_blueprint_elements <- function(x, prefix = "") {

  header <- if (prefix == "") {
    abbreviation(x)
  } else {
    paste(style_subtle(prefix), abbreviation(x))
  }
  if (!is_lst_blueprint(x)) {
    return(header)
  }

  x_elements <- x$elements
  if (rlang::is_empty(x_elements)) {
    return(header)
  }

  n_elements <- length(x_elements)
  elements_indices <- indices(x_elements)
  last_index <- elements_indices[[n_elements]]
  recycled <- last_element_recycled(x)

  body <- purrr::list_c(purrr::map2(
    x_elements,
    elements_indices,
    function(element, index) {
      index <- as_index(index, .recycled = (recycled && index == last_index))
      prefix <- paste0(prefix, index)
      format_lst_blueprint_elements(element, prefix = prefix)
    }
  ))
  c(header, body)
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

# TODO Ethan:
# - use the blueprint to format
#   - in fact, define formatting for each blueprint, which is then called within
#     the `format.specifyr_*_spec` method
# - add formatting for `check` as well! Need to add these somehow.

# subsetting -------------------------------------------------------------------

# TODO Ethan:
# - divert subsetting to the inner blueprint$elements
# - add `names` and `length` methods which do the same
# - might want to create an `specifyr_rec_spec` (recursive specificication)
#   to inherit from here, so that we can define a common subsetting strategy?
