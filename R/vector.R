# constructors -----------------------------------------------------------------

# TODO:
# I think the most useful thing would be to give users the ability to generate
# *just* the specification function, without the blueprint. That way, they can
# opt out of the `last_spec` messaging behaviour and have a lightweight function.

# TODO: Requires a *lot* of error checking for bad inputs.
vector_spec <- function(
    cls,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    missing = FALSE,
    spec_env = rlang::caller_env(),
    spec_checks = list(),
    spec_checks_at = c("after", "before"),
    spec_return = rlang::sym("x"),
    spec_null_return = spec_return,
    spec_missing_return = spec_return,
    spec_ns_base = FALSE
  ) {

}

#' @export
new_spec <- function(blueprint, ...) {
  UseMethod("new_spec")
}

# TODO: Remember, you need a generic way to construct a specification from a blueprint,
#       so that it can be re-constructed when supplied to a recursive specification.
new_spec.vector_blueprint <- function(
    blueprint,
    spec_env = rlang::caller_env(),
    spec_checks_at = c("after", "before"),
    spec_return = rlang::sym("x"),
    spec_null_return = spec_return,
    spec_missing_return = spec_return,
    spec_ns_base = FALSE
  ) {

}

new_vector_blueprint <- function(cls, len, nas, null, missing, check_blueprints) {
  stopifnot(
    is.list(check_blueprints) &&
    all(vapply(check_blueprints, inherits, logical(1L), what = "specifyr_check_blueprint"))
  )
  structure(
    .Data = list(cls = cls, len = len, nas = nas, null = null, missing = missing),
    check_blueprints = check_blueprints,
    class = c("specifyr_vector_blueprint", "specifyr_object_blueprint")
  )
}

# TODO:
# - implement `check`, `check_must`, and friends
# - add the `spec_checks`
vector_check_fn <- function(
    cls,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    missing = FALSE,
    spec_env = rlang::caller_env(),
    spec_checks = list(),
    spec_checks_at = c("after", "before"),
    spec_return = rlang::sym("x"),
    spec_null_return = spec_return,
    spec_missing_return = spec_return,
    spec_ns_base = FALSE
  ) {

  error_index <- list()
  x <- rlang::sym("x")

  null_return <- if (null) null_return_body(x, error_index, spec_null_return)
  missing_return <- if (missing) missing_return_body(x, error_index, spec_missing_return)

  cls_check <- if (is.null(cls)) {
    vctr_check_body(x, error_index)
  } else {
    cls_check_body(x, error_index, cls)
  }
  len_check <- if (!is.null(len)) len_check_body(x, error_index, target_len = len)
  nas_check <- if (!nas) nas_check_body(x, error_index, target_len = len)

  body <- expr_squash(
    missing_return,
    null_return,
    cls_check,
    len_check,
    nas_check,
    spec_return
  )

  new_function(
    args = pairlist2(
      x = ,
      x_name = quote(rlang::caller_arg(x)),
      error_call = quote(rlang::caller_env()),
      error_class = "specifyr_object_mispecified_error"
    ),
    body = body,
    env = spec_env
  )
}
