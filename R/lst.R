# specification ----------------------------------------------------------------

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
  stop_not_integerish(.len, len = 1:2, nas = FALSE, opt = TRUE)
  stop_wrong_vec(.opt, "logical", len = 1, nas = FALSE)
  stop_incompatible_dots_len(dots, .len)

  blueprint <- lst_blueprint(
    !!!purrr::map(dots, spec_blueprint),
    .len = as.integer(.len),
    .opt = .opt
  )
  new_spec(blueprint)
}

is_lst_spec <- function(x) {
  inherits(x, "specifyr_lst_spec")
}

lst_blueprint <- function(
    ...,
    .len = NULL,
    .opt = FALSE,
    .check_blueprints = list()
  ) {
  dots <- rlang::list2(...)
  stopifnot(all(purrr::map_lgl(dots, is_obj_blueprint)))
  structure(
    .Data = list(
      len = .len,
      opt = .opt,
      elements = compact_elements(dots),
      check_blueprints = .check_blueprints
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

stop_incompatible_dots_len <- function(
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

# formatting -------------------------------------------------------------------

#' @export
abbreviation.specifyr_lst_spec <- function(x) {
  abbreviation(spec_blueprint(x))
}

#' @export
abbreviation.specifyr_lst_blueprint <- function(x) {
  paste0(
    "<list",
    fmt_len(x$len),
    fmt_opt(x$opt),
    fmt_checks(x$check_blueprints),
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
print.specifyr_lst_spec <- function(x, width = NULL) {
  checks <- spec_checks(x)
  blueprint <- spec_blueprint(x)
  width <- width %||% cli::console_width()
  cli::cat_line(c(
    "<lst_spec>",
    numbered(format_blueprint_elements(blueprint), from = 0)
  ))
  if (!rlang::is_empty(checks)) {
    cat("\n")
    print_checks(checks)
  }
  invisible(x)
}

format_blueprint_elements <- function(x, prefix = "") {

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
      format_blueprint_elements(element, prefix = prefix)
    }
  ))
  c(header, body)
}

last_element_recycled <- function(x) {
  blueprint <- if (is_obj_blueprint(x)) x else spec_blueprint(x)
  if (!inherits(blueprint, "specifyr_rec_blueprint")) {
    FALSE
  } else {
    !identical(blueprint$len, length(blueprint$elements))
  }
}

# assert -----------------------------------------------------------------------

assert_spec_blueprint.specifyr_lst_blueprint <- function(
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
  execute_checks(
    blueprint$check_blueprint,
    arg = arg,
    arg_name = arg_name,
    error_call = error_call,
    error_class = error_class
  )

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
    assert_spec_blueprint(
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
