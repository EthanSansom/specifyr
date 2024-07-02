#' Generate a function to check an object
#'
#' @description
#'
#' [new_check()], [new_check_must()], and [new_check_must_not()] generate a
#' function which checks an object using a specified `test`. If the `test`
#' evaluates to `FALSE`, an error is raised. Otherwise, the checked object is
#' returned.
#'
#' @param test `[expression]`
#'
#' An expression in terms of `x` which returns `TRUE` or `FALSE`, used to test
#' some quality of an object `x`.
#'
#' Examples of tests include:
#' - does `x` contain any `NaN` values, `any(is.nan(x))`
#' - is `x` a date before the 21st century, `is.Date(x) && x < as.Date("2000-01-01")`
#' - is `x` an even number, `is.numeric(x) && x %% 2 == 0`
#'
#' @param error_args `[character / list / expression]`
#'
#' Arguments passed to [cli::cli_abort()] by the generated function when `test`
#' evaluates to `FALSE`.
#'
#' @param env `[environment]`
#'
#' The environment of the generated function. By default, `env` is the current
#' environment.
#'
#' @return
#'
#' A function with arguments `x`, `x_name`, `error_call`, and `error_class`,
#' which returns `x` if `x` meets some criteria (specified by `test`) and
#' which raises an error otherwise.
#'
#' @export
#'
#' @examples
new_check <- function(test, error_args, env = rlang::caller_env()) {
  .new_check(
    test = rlang::enexpr(test),
    error_args = rlang::enexpr(error_args),
    env = vir(env)
  )
}

#' @export
new_check_must <- function(
    test,
    must,
    bullets = NULL,
    env = rlang::caller_env()
  ) {

  message <- c(
    paste0("{.arg {x_name}} must ", string(must), "."),
    chr(bullets, nas = FALSE, null = TRUE)
  )
  .new_check(
    test = rlang::enexpr(test),
    error_args = message,
    env = vir(env)
  )
}

#' @export
new_check_must_not <- function(
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

.new_check <- function(test, error_args, env) {

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
    !!make_check_stop(error_args)
  })

  check <- rlang::new_function(
    args = args,
    body = body,
    env = env
  )
  class(check) <- c("specifyr_check", "function")
  attr(check, "blueprint") <- structure(
    list(test = test, error_args = error_args),
    class = c("specifyr_check_blueprint", "blueprint")
  )
  check
}

# helpers ----------------------------------------------------------------------

# The expression run to emit an error after a check has failed
make_check_stop <- function(error_args) {
  rlang::expr({
    args <- specifyr::prepare_error_args(!!error_args)
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

#' @export
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

# helpers ----------------------------------------------------------------------

is_check <- function(x) {
  inherits(x, "specifyr_check")
}

is_check_blueprint <- function(x) {
  inherits(x, "specifyr_check_blueprint")
}

#' @export
as_test.specifyr_check <- function(x) {
  blueprint(x)$test
}

#' @export
as_test.specifyr_check_blueprint <- function(x) {
  x$test
}

#' @export
as_stop.specifyr_check <- function(x) {
  make_check_stop(blueprint(x)$error_args)
}

#' @export
as_stop.specifyr_check_blueprint <- function(x) {
  make_check_stop(x$error_args)
}

# interactive test -------------------------------------------------------------

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
