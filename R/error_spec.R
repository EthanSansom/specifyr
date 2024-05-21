the <- rlang::new_environment(
  list(
    error_spec = NULL
  )
)

register_error_spec <- function(spec) {
  if (!is_obj_spec(spec)) {
    cli::cli_abort("{.arg spec} must be a {.cls specifyr_obj_spec}.", .internal = TRUE)
  }
  class(spec) <- c("error_spec", class(spec))
  the$error_spec <- spec
}

error_spec <- function() {
  the$error_spec
}

# TODO Ethan: Given the naming conventions of everything else, having an `error_spec`
# might be confusing (since we'll also have `condition_spec` / `cnd_spec`). Maybe `fail_spec`?
