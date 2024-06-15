the <- rlang::new_environment(
  list(
    spec_error = NULL
  )
)

register_spec_error <- function(spec) {
  specifyr_internal_error(spec, "is_object_spec")
  class(spec) <- c("specifyr_error_spec", class(spec))
  the$spec_error <- spec
}

# TODO: This should have a `...` argument that allows you to pluck INTO the
# specification at the supplied index.
#
#' @export
spec_error <- function(...) {
  # spec_pluck(the$spec_error, ...)
  the$spec_error
}
