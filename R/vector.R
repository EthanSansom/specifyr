# constructors -----------------------------------------------------------------

# TODO:
# I think the most useful thing would be to give users the ability to generate
# *just* the specification function, without the blueprint. That way, they can
# opt out of the `last_spec` messaging behavior and have a lightweight function.

# TODO Ethan:
# - make some special cases for certain combinations of vector spec inputs
# Ex. `vector_spec("logical", len = 1, nas = FALSE)` produces checks with a
# special error message regarding a boolean value.
#
# Error:
# ! `x` must be a single `TRUE` or `FALSE` value.
# x `x` is NA.
#
# Same for `is_string` and `is_count`.
#
# TODO ACUALLY! Just make some pre-built specs for bool, count, string...

# TODO: Requires a *lot* of error checking for bad inputs.
vector_spec <- function(
    cls,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    missing = FALSE,
    spec_env = rlang::caller_env(),
    spec_checks = list(),
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

vector_check_fn <- function(
    cls,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    missing = FALSE,
    spec_env = rlang::caller_env(),
    spec_checks = list(),
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

  spec_checks <- lapply(
    spec_checks,
    \(check) {
      bp <- blueprint(check)
      predicate_check_body(
        x = x,
        error_index = list(),
        predicate = bp$predicate,
        message = bp$message
      )
    }
  )

  body <- expr_squash(
    missing_return,
    null_return,
    cls_check,
    len_check,
    nas_check,
    !!!spec_checks,
    spec_return
  )

  rlang::new_function(
    args = rlang::pairlist2(
      x = ,
      x_name = quote(rlang::caller_arg(x)),
      error_call = quote(rlang::caller_env()),
      error_class = "specifyr_object_mispecified_error"
    ),
    body = body,
    env = spec_env
  )
}

# INTERACTIVE TESTS ------------------------------------------------------------

if (FALSE) {
  rm(list = ls())
  load_all()

  # Test inserting checks into vector specifications
  above_10 <- check(~ isTRUE(x > 10), "{.arg {x_name}} must be greater than 10.")
  big_integer <- vector_check_fn(cls = "integer", len = 1, spec_checks = list(above_10))

  big_integer(20L)
  big_integer(1L)
  big_integer(c(100L, 200L))
  big_integer(NA_integer_)

  below_100 <- check(~isTRUE(x < 100), "{.arg {x_name}} must be less than 1000.")
  medium_integer <- vector_check_fn(
    cls = "integer",
    len = 1,
    spec_checks = list(above_10, below_100)
  )

  medium_integer(20L)
  medium_integer(1000L)

  base_medium_integer <- function(x) {
    if (!is.integer(x)) stop("`x` must be an integer.")
    if (!isTRUE(x > 10)) stop("`x` must be greater than 10.")
    if (!isTRUE(x < 1000)) stop("`x` must be less than 1000.")
    x
  }

  bench::mark(
    medium_integer(20L),
    base_medium_integer(20L)
  )

  object.size(medium_integer)
  object.size(base_medium_integer)
}


