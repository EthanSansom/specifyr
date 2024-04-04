# spec -------------------------------------------------------------------------

new_spec <- function(blueprint, ...) {
  structure(
    .Data = purrr::partial(assert_blueprint, blueprint = blueprint),
    blueprint = blueprint,
    class = gsub("_blueprint$", "_spec", class(blueprint))
  )
}

obj_spec <- function(.cls = "", .opt = FALSE) {
  stop_wrong_vec(.cls, cls = "character", len = 1, nas = FALSE)
  stop_wrong_vec(.opt, cls = "logical", len = 1, nas = FALSE)

  blueprint <- obj_blueprint(.cls, .opt)
  new_spec(blueprint)
}

vec_spec <- function(.cls = "", .len = NULL, .nas = TRUE, .opt = FALSE) {
  stop_wrong_vec(.cls, cls = "character", len = 1, nas = FALSE)
  stop_wrong_vec(.len, cls = "integer", len = 1:2, nas = FALSE, opt = TRUE)
  stop_wrong_vec(.nas, cls = "logical", len = 1, nas = FALSE)
  stop_wrong_vec(.opt, cls = "logical", len = 1, nas = FALSE)

  blueprint <- vec_blueprint(.cls, .len, .nas, .opt)
  new_spec(blueprint)
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
  stop_incompatible_dots_length(dots, len = .len)

  blueprint <- lst_blueprint(!!!purrr::map(dots, blueprint), .len = .len, .opt = .opt)
  new_spec(blueprint)
}

stop_incompatible_dots_length <- function(
    dots,
    len,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_api"
) {

  if (is.null(len)) {
    return(invisible())
  }

  n_dots <- length(dots)
  if (!identical(n_dots, len)) {
    max_dots <- len[[1]]
    if (n_dots > max_dots) {
      # TODO: Improve this message. We can't have more inner specifications than
      #       the minimum list length.
      cli_abort(
        "Incompatible dots length.",
        class = error_class
      )
    }
  }

}

is_obj_spec <- function(x) inherits(x, "specifyr_obj_spec")

blueprint <- function(x) attr(x, "blueprint")

# attachment -------------------------------------------------------------------

attatch <- function(.spec, ...) {

  # TODO Ethan: Improve the error messages here
  dots <- rlang::list2(...)
  stopifnot(all(purrr::map_lgl(dots, is_check)))
  stopifnot(is_obj_spec(.spec))

  new_blueprint <- blueprint(.spec)
  new_blueprint$checks <- append(new_blueprint$checks, dots)
  new_spec(new_blueprint)
}
