# todos ------------------------------------------------------------------------

# TODO:
# - every specification needs to record a blueprint which allows the specification
#   to be "completely" reconstructed

# int --------------------------------------------------------------------------

# Specifications have a "blueprint". This will allow them to be intelligently
# combined and nested (e.g. `lst_spec(x = int_spec(), y = int_spec())`).
#
# For example, `lst_spec(int_spec(len = 10L, nas = FALSE))` can use `checkmate::qtestr`
# with the correct rules for the inner integer.

int_spec <- function(
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    # TODO: Add the ability to attach checks directly to a specification. Make
    #       sure that they're included in the blueprint.
    checks = list(),
    error_class = "specifyr_error_mistyped",
    spec_is = c("check", "test"),
    spec_env = rlang::caller_env(),
    # TODO: Maybe get rid of this - confusing and not totally neccisary
    x_must = NULL
  ) {

  # TODO: Error message doesn't make sense in this context
  blueprint <- check_vector_type_args(
    cls = "integer",
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    # `check_vector_type_args` doesn't check the type, since lower/upper is class dependent
    lower = num(lower),
    upper = num(upper)
  )
  spec_is <- rlang::arg_match(spec_is)
  spec_env <- vir(spec_env)
  error_class <- chr(error_class, min_len = 1, nas = FALSE)
  x_must <- x_must %!|% string(x_must)

  args <- rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::caller_env()),
    error_class = error_class
  )
  test_body <- make_qtest_test(
    cls = "integer",
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper
  )

  if (spec_is == "test") {
    spec <- rlang::new_function(
      args = args,
      body = test_body,
      env = spec_env
    )
    class(spec) <- c("specifyr_atomic_spec", "specifyr_type_test", "function")
    attr(spec, "blueprint") <- blueprint
    return(spec)
  }

  body <- rlang::expr({
    if (!!test_body) {
      return(x)
    }
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      x_must = !!x_must,
      type_test = is.integer,
      type_desc = c("an", "{.cls integer}"),
      len = !!len,
      nas = !!nas,
      null = !!null,
      min_len = !!min_len,
      max_len = !!max_len,
      lower = !!lower,
      upper = !!upper
    )
  })
  spec <- rlang::new_function(
    args = args,
    body = body,
    env = spec_env
  )
  class(spec) <- c(
    "specifyr_atomic_spec",
    "specifyr_type_check",
    "function"
  )
  class(blueprint) <- c("atomic_blueprint")
  attr(spec, "blueprint") <- blueprint
  spec
}
class(int_spec) <- c("specifyr_type_spec", "function")

# helpers ----------------------------------------------------------------------

is_atomic_spec <- function(x) {
  inherits(x, "specifyr_atomic_spec")
}

is_atomic_blueprint <- function(x) {
  inherits(x, "atomic_blueprint")
}

is_vector_spec <- function(x) {
  inherits(x, "specifyr_vector_spec")
}

is_vector_blueprint <- function(x) {
  inherits(x, "vector_blueprint")
}

# new --------------------------------------------------------------------------

new_vector_spec <- function(
    type_test,
    type_desc,
    type_must,
    ...,
    checks = list(),
    env = rlang::caller_env()
  ) {

}

# factories --------------------------------------------------------------------

as_stop <- function(x, ...) {
  UseMethod("as_stop")
}

as_test <- function(x, ...) {
  UseMethod("as_test")
}

as_stop.specifyr_atomic_spec <- function(
    x,
    x_sym = quote(x),
    x_name = quote(x_name),
    error_call = "specifyr_error_mistyped",
    error_class = quote(error_class),
    x_must = NULL
  ) {

  blueprint <- blueprint(x)
  make_vector_stop(
    type_test = builtin_type_test(blueprint$cls),
    type_desc = builtin_type_desc(blueprint$cls),
    !!!blueprint,
    .x = x_sym,
    .x_name = x_name,
    .error_call = error_call,
    .error_class = error_class,
    .x_must = x_must
  )
}

as_stop.specifyr_vector_spec <- function(
    x,
    x_sym = quote(x),
    x_name = quote(x_name),
    error_call = "specifyr_error_mistyped",
    error_class = quote(error_class),
    x_must = NULL
) {

  blueprint <- blueprint(x)
  make_vector_stop(
    type_test = builtin_type_test(blueprint$cls),
    type_desc = builtin_type_desc(blueprint$cls),
    !!!blueprint,
    .x = x_sym,
    .x_name = x_name,
    .error_call = error_call,
    .error_class = error_class,
    .x_must = x_must
  )
}

as_test.specifyr_atomic_spec <- function(x) {
  make_qtest_test(!!!blueprint(x))
}

as_test.specifyr_vector_spec <- function(x) {
  make_vector_test(!!!blueprint(x))
}

make_vector_stop <- function(
    type_test,
    type_desc,
    ...,
    .x = quote(x),
    .x_name = quote(x_name),
    .error_call = "specifyr_error_mistyped",
    .error_class = quote(error_class),
    .x_must = NULL
  ) {

  type_desc <- chr(type_desc, min_len = 1, max_len = 2, nas = FALSE)
  x_must <- .x_must %!|% string(.x_must)

  # TODO: Catch this and re-throw as internal
  vector_type_args <- check_vector_type_args(...)
  vector_type_arg_nms <- rlang::names2(vector_type_args)

  # `lower` and `upper` need to be quoted, since they could be calls
  if ("lower" %in% vector_type_arg_nms) {
    vector_type_args$lower <- rlang::enexprs(...)$lower
  }
  if ("upper" %in% vector_type_arg_nms) {
    vector_type_args$lower <- rlang::enexprs(...)$upper
  }

  stop_args <- rlang::pairlist2(
    x = .x,
    x_name = .x_name,
    x_must = x_must,
    !!!vector_type_args,
    error_call = .error_call,
    error_class = .error_class
  )

  rlang::new_call(
    car = rlang::sym("stop_vector_mistyped"),
    cdr = as.pairlist(stop_args)
  )
}

make_vector_test <- function(...) {

  dots <- rlang::list2(...)
  dot_nms <- rlang::names2(dots)
  if ("type_test" %notin% dot_nms) {
    cli::cli_abort(
      "{.arg type_test} must be supplied to `...`.",
      .internal = TRUE
    )
  } else {
    type_test <- dots$type_test
  }

  type_arg_nms <- check_vector_type_arg_nms(
    x = dot_nms["type_test" != dot_nms],
    x_is_names = TRUE
  )

  vector_tests <- list(
    nas = quote((nas || !any(is.na(x)))),
    len = quote((is.null(len) || length(x) == len)),
    min_len = quote((is.null(min_len) || length(x) >= min_len)),
    max_len = quote((is.null(min_len) || length(x) >= min_len)),
    lower = quote((all(lower <= x, na.rm = TRUE))),
    upper = quote((all(upper >= x, na.rm = TRUE))),
    bounds = quote((all(lower <= x && x <= upper, na.rm = TRUE)))
  )

  arg_tests <- vector_tests[type_arg_nms]
  if (all(c("lower", "upper") %in% type_arg_nms)) {
    arg_tests$lower <- NULL
    arg_tests$upper <- NULL
    arg_tests$bounds <- vector_tests$bounds
  }
  expr_and(type_test, !!!arg_tests)
}

make_qtest_test <- function(...) {

  type_args <- check_vector_type_args(...)

  cls <- type_args$cls %||% "any"
  rules <- switch(
    tolower(cls),
    logical = "b",     # [bB]	Bool / logical.
    integer = "i",     # [iI]	Integer.
    integerish = "x",  # [xX]	Integerish.
    double = "r",      # [rR]	Real / double.
    complex = "c",     # [cC]	Complex.
    numeric = "n",     # [nN]	Numeric (integer or double).
    character = "s",   # [sS]	String / character.
    factor = "f",      # [fF]	Factor
    atomic = "a",      # [aA]	Atomic.
    vector = "v",      # [vV]	Atomic vector.
    list = "l",        # [lL]	List. Missingness is defined as NULL element.
    matrix = "m",      # [mM]	Matrix.
    data.frame = "d",  # [dD]	Data.frame.
    posixct = "p",     # [pP]	POSIXct date.
    environment = "e", # [e]	Environment.
    null = "0",        # [0]	NULL.
    any = "*",         # [*]	placeholder to allow any type.
    cli::cli_abort("Unknown class {.val {cls}}.", .internal = TRUE)
  )

  if (isFALSE(type_args$nas)) rules <- toupper(rules)

  # TODO: Import the `zealot` pipe for multiple assignment
  len <- type_args$len
  min_len <- type_args$min_len
  max_len <- type_args$max_len
  len_null <- is.null(type_args$len)
  min_len_null <- is.null(min_len) || is.infinite(min_len)
  max_len_null <- is.null(max_len) || is.infinite(max_len)
  if (!len_null && (!min_len_null || !max_len_null)) {
    supplied_args <- c("len", min_len %!|% "min_len", max_len %!|% "max_len")
    cli::cli_abort(
      c(
        "Either {.arg len} or any of {.arg min_len}, {.arg max_len} may be supplied.",
        x = "{.args {supplied_args}} were supplied."
      ),
      .internal = TRUE
    )
  }
  if (!len_null) {
    rules <- paste0(rules, len)
  } else if (!min_len_null & max_len_null) {
    rules <- paste0(rules, ">=", min_len)
  } else if (min_len_null & !max_len_null) {
    rules <- paste0(rules, "<=", max_len)
  }
  if (!min_len_null & !max_len_null) {
    rules <- paste0(rules, "<=", max_len)
    extra_len_test <- rlang::expr(length(x) >= !!min_len)
  } else {
    extra_len_test <- NULL
  }

  lower <- type_args$lower
  upper <- type_args$upper
  lower_finite <- !is.null(lower) && is.finite(lower)
  upper_finite <- !is.null(upper) && is.finite(upper)
  if (lower_finite && upper_finite) {
    rules <- paste0(rules, "[", lower, ",", upper, "]")
  } else if (lower_finite && !upper_finite) {
    rules <- paste0(rules, "[", lower, if (isTRUE(finite)) ",)" else ",]")
  } else if (!lower_finite && upper_finite) {
    rules <- paste0(rules, if (isTRUE(finite)) "(," else "[,", upper, "]")
  } else if (isTRUE(type_args$finite)) {
    rules <- paste0(rules, "[]")
  }

  if (isTRUE(type_args$null)) rules <- c("0", rules)

  expr_and(rlang::expr(checkmate::qtest(x, !!rules)), extra_len_test)
}
