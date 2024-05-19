# constructors -----------------------------------------------------------------

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

new_vector_spec <- function(fn, blueprint) {
  stopifnot(is.function(fn))
  stopifnot(inherits(blueprint, "vector_blueprint"))
  structure(
    .Data = fn,
    blueprint = blueprint,
    class = c("specifyr_vector_spec", "specifyr_object_spec")
  )
}

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
  missing_return <- if (missing) missing_return_body(x, error_index, spec_null_return)

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
