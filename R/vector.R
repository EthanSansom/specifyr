# constructors -----------------------------------------------------------------

vector_spec <- function(
    cls,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    missing = FALSE,
    spec_env = rlang::caller_env(),
    spec_checks = list(),
    spec_return = rlang::missing_arg(),
    spec_null_return = rlang::missing_arg(),
    spec_missing_return = rlang::missing_arg(),
    spec_ns_base = FALSE
  ) {

  cls <- if_missing(cls, NULL, check_is_cls(cls))
  len <- check_is_len(len)
  nas <- check_is_bool(nas)
  null <- check_is_bool(null)
  missing <- check_is_bool(missing)

  check_is_list(spec_checks)
  for (i in seq_along(spec_checks)) {
    check_is_check(spec_checks[[i]], paste0("spec_checks[[", i, "]]"))
  }
  check_blueprints <- lapply(spec_checks, blueprint)

  spec_return <- rlang::enexpr(spec_return)
  spec_null_return <- rlang::enexpr(spec_null_return)
  spec_missing_return <- rlang::enexpr(spec_missing_return)

  # TODO: Make a nicer way to do this. I need some sentinel value to use as a
  # default for objects which will be immediately converted to expressions.
  if (identical(spec_return, rlang::expr(rlang::missing_arg()))) spec_return <- rlang::sym("x")
  if (identical(spec_null_return, rlang::expr(rlang::missing_arg()))) spec_null_return <- spec_return
  if (identical(spec_missing_return, rlang::expr(rlang::missing_arg()))) spec_missing_return <- spec_return

  vector_fn <- new_vector_check_fn(
    cls = cls,
    len = len,
    nas = nas,
    null = null,
    missing = missing,
    spec_env = check_is_env(spec_env),
    spec_return = spec_return,
    spec_null_return = spec_null_return,
    spec_missing_return = spec_missing_return,
    spec_ns_base = check_is_bool(spec_ns_base),
    check_blueprints = check_blueprints
  )
  vector_blueprint <- new_vector_blueprint(
    cls = cls,
    len = len,
    nas = nas,
    null = null,
    missing = missing,
    check_blueprints = check_blueprints
  )

  structure(
    .Data = vector_fn,
    blueprint = vector_blueprint,
    class = c("specifyr_vector_spec", "specifyr_object_spec")
  )
}

new_vector_blueprint <- function(cls, len, nas, null, missing, check_blueprints) {
  structure(
    .Data = list(cls = cls, len = len, nas = nas, null = null, missing = missing),
    check_blueprints = check_blueprints,
    class = c("specifyr_vector_blueprint", "specifyr_object_blueprint")
  )
}

new_vector_check_fn <- function(
    cls,
    len,
    nas,
    null,
    missing,
    spec_env,
    spec_return,
    spec_null_return,
    spec_missing_return,
    spec_ns_base,
    check_blueprints
  ) {

  error_index <- list()
  x <- rlang::sym("x")

  null_return <- if (null) null_return_body(x, spec_null_return)
  missing_return <- if (missing) missing_return_body(x, spec_missing_return)

  cls_check <- if (is.null(cls)) vctr_check_body(x, error_index) else cls_check_body(x, error_index, cls)
  len_check <- if (!is.null(len)) len_check_body(x, error_index, target_len = len)
  nas_check <- if (!nas) nas_check_body(x, error_index, target_len = len)
  checks <- lapply(
    check_blueprints,
    \(blueprint) {
      predicate_check_body(
        x = x,
        error_index = list(),
        predicate = blueprint$predicate,
        message = blueprint$message
      )
    }
  )

  rlang::new_function(
    args = rlang::pairlist2(
      x = ,
      x_name = quote(rlang::caller_arg(x)),
      error_call = quote(rlang::caller_env()),
      error_class = "specifyr_object_mispecified_error"
    ),
    body = expr_squash(
      missing_return,
      null_return,
      cls_check,
      len_check,
      nas_check,
      !!!checks,
      spec_return
    ),
    env = spec_env
  )
}

if_missing <- function(x, then, otherwise) {
  if (rlang::is_missing(x)) then else otherwise
}

# INTERACTIVE TESTS ------------------------------------------------------------

if (FALSE) {
  rm(list = ls())
  load_all()

  # Test inserting checks into vector specifications
  above_10 <- check(~ isTRUE(x > 10), "{.arg {x_name}} must be greater than 10.")
  big_integer <- vector_spec(cls = "integer", len = 1, spec_checks = list(above_10))

  # Test `spec_error`
  above_10(1)
  spec_error() # NULL

  big_integer(50L)
  spec_error() # NULL

  big_integer(1)
  spec_error() # returns `big_integer`

  big_integer(20L)
  big_integer(1L)
  big_integer(c(100L, 200L))
  big_integer(NA_integer_)

  below_100 <- check(~isTRUE(x < 100), "{.arg {x_name}} must be less than 1000.")
  medium_integer <- vector_spec(
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

  # Do some of the `checkmate` benchmarks
  spec <- vector_spec("logical", len = 1, nas = FALSE)
  cm   <-   function(x) checkmate::assertFlag(x)
  cmq  <- function(x) checkmate::qassert(x, "B1")

  bench::mark(
    spec(TRUE),
    cm(TRUE),
    cmq(TRUE)
  )

  # TODO: We could very well use `checkmate::` as the back-end where possible...
  # For vector specifications we could do something like:
  # if (checkmateable(cls = cls, len = len, nas = nas, null = null)) {
  #   checkmate_check_body(...)
  # } else {
  #   create the check body manually...
  # }
  # See:
  # - https://mllg.github.io/checkmate/reference/qassert.html  (builtin-types)
  # - https://mllg.github.io/checkmate/reference/qassertr.html (optimized for list or dataframe)
  # Benchmark the cases where checkmate is slower, make those non-checkmateable.
  #
  # Because checkmate's tests are super fast on combined inputs, you can deliver
  # an error message that is more detailed (i.e. covers all the failure cases).
  #
  #
  x = runif(1000)
  spec <- vector_spec(
    cls = "numeric",
    len = 1000,
    nas = FALSE,
    spec_checks = list(
      check_must(~all(0 < x & x < 1), "be in range (0, 1)")
    ),
    spec_return = x
  )
  spec_check <- check_must(
    ~checkmate::qtest(x, "N1000[0,1]"),
    "be a length-1000 {.cls numeric} vector in range (0, 1)"
  )
  r = function(x) { stopifnot(is.numeric(x), length(x) == 1000, all(!is.na(x) & x >= 0 & x <= 1)); x }
  cm = function(x) checkmate::assertNumeric(x, len = 1000, any.missing = FALSE, lower = 0, upper = 1)
  cmq = function(x) checkmate::qassert(x, "N1000[0,1]")

  bench::mark(
    spec(x),
    spec_check(x),
    r(x),
    cm(x),
    cmq(x)
  )

}
