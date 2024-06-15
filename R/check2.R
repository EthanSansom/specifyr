# TODO:
# - for the blueprint system, make a generic "from blueprint" function, which
#   can reconstruct the necessary parts of a test or speck from the blueprint
#   - `blueprint_to_test_body` -> make a "test" expression from a blueprint
#   - `blueprint_to_error_expr` -> make an "error" expression (i.e the call) from
#   a blueprint
#     - these should have options for "single" and "multiple", as well as ways to
#       change what symbols "x" and "x_name" are.

# constructor ------------------------------------------------------------------

check <- function(test, error_args, env = rlang::caller_env()) {
  new_check(
    test = rlang::enexpr(test),
    error_args = rlang::enexpr(error_args),
    env = vir(env)
  )
}

check_must <- function(
    test,
    must,
    bullets = NULL,
    env = rlang::caller_env()
  ) {
  message <- c(
    paste0("{.arg {x_name}} must ", string(must), "."),
    chr(bullets, nas = FALSE, null = TRUE)
  )
  new_check(
    test = rlang::enexpr(test),
    error_args = message,
    env = vir(env)
  )
}

check_must_not <- function(
    test,
    must,
    not = NULL,
    bullets = NULL,
    env = rlang::caller_env()
) {
  must <- string(must)
  not <- string(not %||% "{.obj_type_friendly {x}}")
  message <- c(
    paste0("{.arg {x_name}} must ", must, ", not ", not, "."),
    chr(bullets, nas = FALSE, null = TRUE)
  )
  new_check(
    test = rlang::enexpr(test),
    error_args = message,
    env = vir(env)
  )
}

new_check <- function(test, error_args, env) {

  args <- rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::current_env()),
    error_class = "specifyr_error_mistyped"
  )
  body <- rlang::expr({
    if (!!test) {
      return(x)
    }
    # TODO: `prepare_error_args` will need to be exported, or accessed with `:::`
    args <- prepare_error_args(!!error_args)
    .envir <- if (is.null(args$.envir)) rlang::current_env() else args$.envir
    .frame <- if (is.null(args$.frame)) .envir else args$.frame
    cli::cli_abort(
      message = args$message,
      !!quote(!!!args$dots),
      call = error_call,
      class = c(error_class, "specifyr_error"),
      .envir = .envir,
      .frame = .frame
    )
  })

  check <- rlang::new_function(
    args = args,
    body = body,
    env = env
  )
  class(check) <- c("specifyr_check", "function")
  attr(check, "blueprint") <- structure(
    list(test = test, error_args = error_args),
    class = c("blueprint_check", "blueprint")
  )
  check
}

# helpers ----------------------------------------------------------------------

# TODO: This will need to be exported, or namespaced within the `new_check` generated function
prepare_error_args <- function(error_args) {
  out <- list(
    message = NULL,
    dots = NULL,
    .envir = NULL,
    .frame = NULL
  )
  if (is.character(error_args)) {
    out$message <- error_args
  } else if (is.list(error_args)) {
    out$message <- error_args$message
    out$dots <- error_args[rlang::names2(error_args) %notin% c("message", ".envir", ".frame")]
    out$.envir <- error_args$.envir
    out$.frame <- error_args$.frame
  }
  out
}

is_check <- function(x) {
  inherits(x, "specifyr_check")
}

is_check_blueprint <- function(x) {
  inherits(x, "blueprint_check")
}

# factories --------------------------------------------------------------------

# TODO STYLE GUIDE:
# - `as_*` converts an object or it's blueprint into an expression
# - `make_*` converts some argument into an expression

as_check_test <- function() {
  if (is_check(x)) {
    return(blueprint(x)$test)
  } else if (is_check_blueprint(x)) {
    return(x$test)
  }
  cli::cli_abort(
    "{.arg x} must be a {.cls specifyr_check} or {.cls blueprint_check}.",
    .internal = TRUE
  )
}

as_stop.specifyr_check <- function(x) {
  make_check_stop(blueprint(x)$error_args)
}

as_stop.check_blueprint <- function(x) {
  make_check_stop(x$error_args)
}

as_test.specifyr_check <- function(x) {
  blueprint(x)$test
}

as_test.check_blueprint <- function(x) {
  x$test
}

as_check_stop <- function(x) {
  if (is_check(x)) {
    error_args <- blueprint(x)$error_args
  } else if (is_check_blueprint(x)) {
    error_args <- x$error_args
  } else {
    cli::cli_abort(
      "{.arg x} must be a {.cls specifyr_check} or {.cls blueprint_check}.",
      .internal = TRUE
    )
  }
  make_check_stop(error_args)
}

make_check_stop <- function(error_args) {
  rlang::expr({
    # TODO: `prepare_error_args` will need to be exported, or accessed with `:::`
    args <- prepare_error_args(!!error_args)
    .envir <- if (is.null(args$.envir)) rlang::current_env() else args$.envir
    .frame <- if (is.null(args$.frame)) .envir else args$.frame
    cli::cli_abort(
      message = args$message,
      !!quote(!!!args$dots),
      call = error_call,
      class = c(error_class, "specifyr_error"),
      .envir = .envir,
      .frame = .frame
    )
  })
}

# Test -------------------------------------------------------------------------

if (FALSE) {

  na_errors <- function(x, x_name) {
    n_nas <- sum(is.na(x))
    list(
      message = c(
        "{.arg {x_name}} must not be NA.",
        x = "{.arg {x_name}} is NA {n_nas} time{?s}."
      ),
      .envir = rlang::current_env()
    )
  }

  check_no_nas <- check(!any(is.na(x)), na_errors(x, x_name))
  check_no_nas(10)
  check_no_nas(NA)

  x <- c(1, 2, 3, NA, NA, 4, 5, NA, 10)
  check_no_nas(x)

  check_is_date <- check_must_not(
    test = lubridate::is.Date(x),
    must = "be a {.cls Date}"
  )
  check_is_date(as.Date("2021-02-01"))
  check_is_date(20)
}
