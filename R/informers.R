# todo -------------------------------------------------------------------------

# TODO:
# - Make easier ways to describe objects, useful for the header
# - Add the `pass_hints` option to `and()`. Otherwise, don't include the `pass`
#   argument in the generated `check()` function (to save space)
#   - Never preserve `pass` in `or()`
# - Allow a top-level `pass` and `fail` to be defined in `and()` and `or()`
# - Allow arguments like `pass`, `fail`, `header` to be provided as expressions
#   - We're looking for either a character vector or a quoted expression
# - Create the `manual_check()` function, where a `test`, `header`, and `bullets`
#   are provided as expressions

# TODO: Make an `alias()` function:
# - An alias is simply a check with no arguments other than `x` (i.e. `check(test(is.numeric(x)))`)
#   - This allows static typing via the alias (i.e. `%int%`)
#   - This allows nesting aliases with `a_lst()`, `a_dat()`, etc.
#     `a_dat(x = int, y = chr)` -> dataframe with columns `x` (integer), `y` (character)
#
# We allow either an alias OR a call, with any necessary non-`x` defaults filled in.
# When we provide `a_dat(x = int)`, the default arguments to `int` are crystallized,
# (i.e. substituted into the function).
#
# Suppose in `int()`, we have default `len = NULL`
# get_test(int)            -> is.integer(x) && (is.null(len) || length(x) == len)
# get_test(a_dat(x = int)) -> is.integer(x) && (is.null(NULL)) || length(x) == NULL)
# - note that `len` has been substituted for it's value `NULL`

# TODO: Create a `vignette` about the restrictions on expressions supplied to
# `test()` and why they exist. Specifically:
# - why the `test() expression `expr` may not contain symbols `.t*.`
# - why the `test()` arguments `.args` (or `...`) may not match `.*_name$`
#
# The reasons are:
# 1. We need the `.t*.` symbols to record the results
# 2. We use the `.*_name` argument with `*_name = rlang::caller_arg(*)` to
#    use in error message.

# TODO: Implement the pretty features of `multi_info()` which will be used in
# `and()` and `or()`. And the features to `info()` and `test()`.
# - allow `and()` and `or()` arguments to be named `test()` functions, where the
#   name is the failure bullet (like in `stopifnot()`)
# - create default `pass`, `fail`, and `header` components in `info()` using the
#   supplied `test`
#   - we'll want to copy someones (probably {dplyr}'s) labeling functions for error
#     messages to make the test look pretty
#   - we also need a function for stripping the test expressions of the `.t*.`
#     assignments

# test -------------------------------------------------------------------------

# TODO: Allow missing arguments to be supplied to a `test()` via `x = ,` - like
# in `rlang::pairlist2()`.

# NOTE: In the docs, mention that you can supply arguments to `...` with names
# `.expr` or `.env` by splicing (i.e. `!!!list(.env = quote(caller_env()))`)
test <- function(.expr, ..., .env = rlang::caller_env()) {

  expr <- rlang::enexpr(.expr)
  args <- rlang::pairlist2(...) %|0% list(x = rlang::missing_arg())

  reserved_symbols <- find_syms(expr, pattern = "^\\.t[0-9]+\\.$")
  if (length(reserved_symbols) > 0) {
    pattern <- "^\\.t[0-9]+\\.$"
    cli::cli_abort(
      c(
        "`.expr` must not contain symbols matching the pattern {.val {pattern}} (e.g. `.t1.`, `.t2.`, ...).",
        x = "Expression `.expr` contains symbols: {.var {reserved_symbols}}.",
        i = "{.pkg specifyr} uses these reserved symbols within generated tests."
      )
    )
  }
  arg_names <- rlang::names2(args)
  reserved_args_at <- grep(x = arg_names, pattern = "_name$")
  if (length(reserved_args_at) > 0) {
    pattern <- "_name$"
    bad_dots <- paste0("..", reserved_args_at)
    cli::cli_abort(
      c(
        "Argument names supplied to `...` must not match the pattern {.val {pattern}} (e.g. `arg_name`).",
        x = "Argument{?s} {.arg {bad_dots}} name{?s} match{?/es} the patten {.val {pattern}}.",
        i = "{.pkg specifyr} uses these reserved symbols within generated checks."
      )
    )
  }
  if (vctrs::vec_duplicate_any(arg_names)) {
    pattern <- "_name$"
    bad_dots <- paste0("..", anyDuplicated(arg_names))
    cli::cli_abort(
      c(
        "Argument names supplied to `...` must be unique.",
        x = "Arguments {.arg {bad_dots}} have duplicate names."
      )
    )
  }

  out <- rlang::new_function(
    args = as.pairlist(args),
    body = rlang::enexpr(expr),
    env = .env
  )
  class(out) <- c("specifyr_test", "function")
  out
}

is_test <- function(x) inherits(x, "specifyr_test")

# info -------------------------------------------------------------------------

default_header <- function(expr, args) {
  paste0("`", test_as_label(expr, args), "` must be `TRUE`.")
}

default_bullet <- function(expr, args, result, nm = "x") {
  rlang::set_names(
    x = paste0("`", test_as_label(expr, args), "` is `", result, "`."),
    nm = nm
  )
}

test_as_label <- function(expr, args) {
  # `args` is a named list of the form `arg = rlang::caller_arg(arg)`. We
  # attempt to replace the symbol `arg` in the expression `expr` with the
  # contents of `rlang::caller_arg(arg)` (a string).
  #
  # 1. Attempt to parse `rlang::caller_arg(arg)`
  # 2. If un-parseable, use `...` as the expression
  # 3. Substitute the parsed expression (or back-up symbol) into `expr`
  # 4. Convert `expr` into a label
  args <- purrr::map(
    .x = args,
    .f = function(arg) {
      rlang::try_fetch(
        # `parse_exprs()` returns a list of "lines" of code, e.g. "x;y" is parsed
        # as two lines of code, which we then squash into a single expression.
        {
          arg_expr <- rlang::parse_exprs(arg)
          if (length(arg_expr) > 1) {
            expr_squash(!!!arg_expr)
          } else {
            arg_expr[[1]]
          }
        },
        error = function(cnd) quote(...)
      )
    }
  )
  rlang::as_label(replace_symbols(expr, !!!args))
}

# TODO: Allow `pass` and `fail` to be expressions?
info <- function(test, pass, fail, header, env = rlang::caller_env()) {
  # TODO: Improve errors
  stopifnot(
    "`test` must be a <specifyr_test> function." = is_test(test),
    "`pass` must be a character vector." = rlang::is_missing(pass) || is.character(pass),
    "`fail` must be a character vector." = rlang::is_missing(fail) || is.character(fail),
    "`header` must be a character vector." = rlang::is_missing(header) || is.character(header)
  )

  # The default pass or fail bullet looks like:
  # `default_bullet(
  #   expr = quote(<test expression>)`,
  #   args = list(arg1 = arg1_name, arg2 = arg2_name, ...),
  #   result = env_get_result(.t0.)
  # )`
  # Where `arg1`, `arg2`, etc. are arguments of the `test()` function (usually `x`).
  if ((ms_pass <- rlang::is_missing(pass)) | (ms_fail <- rlang::is_missing(fail))) {
    arg_names <- rlang::fn_fmls_names(test)
    args <- rlang::set_names(as.list(rlang::syms(paste0(arg_names, "_name"))), arg_names)
    bullet <- rlang::expr(
      default_bullet(
        expr = quote(!!get_test(test)),
        args = list(!!!args),
        result = env_get_result(.t0.)
      )
    )
    if (ms_pass) {
      pass <- rlang::expr(c(v = !!bullet))
    }
    if (ms_fail) {
      fail <- rlang::expr(c(x = !!bullet))
    }
  }

  if (rlang::is_missing(header)) {
    arg_names <- rlang::fn_fmls_names(test)
    args <- rlang::set_names(as.list(rlang::syms(paste0(arg_names, "_name"))), arg_names)
    header <- rlang::expr(
      default_header(
        expr = quote(!!get_test(test)),
        args = list(!!!args)
      )
    )
  }

  # Adds an appropriate bullet if these have no names
  if (!ms_pass && all(!rlang::have_name(pass))) {
    pass <- rlang::set_names(pass, "v")
  }
  if (!ms_fail && all(!rlang::have_name(fail))) {
    fail <- rlang::set_names(fail, "x")
  }

  test_expr <- get_test(test)
  bullets <- rlang::call2(
    .fn = "single_info",
    pass = pass,
    fail = fail,
    header = header,
    result = quote(env_get_result(.t0.))
  )
  body <- rlang::expr({
    # `force()` is just sugar here, it makes it easy to pull the test expression
    # from an `info()` function.
    force(.t0. <- !!test_expr)
    bullets <- !!bullets
    cli::cli_div(theme = specifyr_theme())
    cli::cli_inform(c(bullets$header, bullets$bullets), class = "specifyr_message_info")
  })

  args <- rlang::fn_fmls(test)
  name_args <- make_name_args(rlang::names2(args))

  out <- rlang::new_function(
    args = c(args, name_args),
    body = body,
    env = env
  )
  class(out) <- c("specifyr_info", "function")
  out
}

is_info <- function(x) inherits(x, "specifyr_info")

# Creates a named list of `arg_name = rlang::caller_arg(arg)` for each `arg`
# in `args`. Note that `args` is a character vector of argument names.
make_name_args <- function(args) {
  args |>
    purrr::map(\(arg) rlang::expr(rlang::caller_arg(!!rlang::sym(arg)))) |>
    rlang::set_names(nm = paste0(args, "_name"))
}

# multiple info ----------------------------------------------------------------

or <- function(..., .header, .env = rlang::caller_env()) {

  informers <- purrr::modify_if(
    .x = rlang::list2(...),
    .p = is_test,
    .f = info
  )
  if (inherits(.header, "cli_ansi_string")) {
    .header <- as.character(.header)
  }

  # TODO: Improve internal error suite later
  stopifnot(
    "`...` must be <specifyr_info> functions." = all(purrr::map_lgl(informers, is_info)),
    "`.header` must be a character vector." = is.character(.header),
    "`.env` must be an environment." = is.environment(.env)
  )

  multiple_info(informers = informers, header = .header, op = "||", env = .env)
}

# TODO: Generate a default `.header` for `and()` and `or()`. Something like:
# All of the following must be `TRUE`:
# * `..1` must be `TRUE`.
# * `..2` must be `TRUE`.
and <- function(..., .header, .env = rlang::caller_env()) {

  # Users can supply multiple informers in a `list()` to combine them with `&`
  # rather than `&&`, to prevent short circuiting and allow more error bullets.
  informers <- purrr::modify_if(
    .x = rlang::list2(...),
    .p = is.list,
    .f = \(x) and_single(informers = x, header = "", env = .env)
  )
  informers <- purrr::modify_if(
    .x = informers,
    .p = is_test,
    .f = info
  )

  if (inherits(.header, "cli_ansi_string")) {
    .header <- as.character(.header)
  }

  # TODO: Improve internal error suite later
  stopifnot(
    "`...` must be <specifyr_info> functions." = all(purrr::map_lgl(informers, is_info)),
    "`.header` must be a character vector." = is.character(.header),
    "`.env` must be an environment." = is.environment(.env)
  )

  multiple_info(
    informers = informers,
    header = .header,
    op = "&&",
    env = .env
  )
}

and_single <- function(informers, header, env) {

  informers <- purrr::modify_if(
    .x = informers,
    .p = is_test,
    .f = info
  )
  stopifnot(
    "`...` must be <specifyr_info> functions." = all(purrr::map_lgl(informers, is_info)),
    "`header` must be a character vector." = is.character(header),
    "`env` must be an environment." = is.environment(env)
  )

  # TODO: Add errors. This will always be called from `and()`, so you'd actually
  # want to refer to it's own arguments.
  multiple_info(informers = informers, header = header, op = "&", env = env)
}

multiple_info <- function(informers, header, op, env, error_call = rlang::caller_env()) {

  # `informers` are functions with bodies like:
  # {
  #  force(<test>)
  #  bullets <- <bullets>
  #  cli::cli_inform(bullets$message)
  # }
  # - `informers_tests` are the list of <test>
  # - `informers_bullets` are the list of <bullets>
  informers_tests <- purrr::map(informers, get_test)
  informers_bullets <- purrr::map(informers, get_bullets)

  # Remove the `header` argument from the internal bullets calls,
  # we want only the external header to be emitted.
  informers_bullets <- purrr::map(
    .x = informers_bullets,
    .f = function(bullets) {
      rlang::call_modify(bullets, header = rlang::zap())
    }
  )

  # <test> expressions are of the form `(.t1. <- test1) op ... op (.tn. <- testn)`.
  # This extracts a named list where `.tk. = testk`, which is used to re-map each
  # <test> to a new distinct `.t*.` symbol in the output.
  informers_test_dicts <- purrr::map(informers_tests, test_expr_dict)

  # Using the test dictionary, re-map all `.t*.` expressions which appear in the
  # `informers_bullets` and `informers_tests` expressions to a new, distinct,
  # `.t*.` symbol.
  t_index <- 1
  t_sym_map <- purrr::map(
    informers_test_dicts,
    function(test_dict) {
      # `new_t_syms` are unique `.t*.` symbols for each test in the informer
      n_tests <- length(test_dict)
      new_t_syms <- rlang::syms(paste0(".t", seq(t_index, t_index + n_tests - 1), "."))
      names(new_t_syms) <- names(test_dict)

      # Bump the `t_index` to ensure no conflicting `.t*.` symbols between tests
      t_index <<- t_index + n_tests
      new_t_syms
    }
  )

  # Replace all `.t*.` symbols in the `informers` functions with new `.t*.` symbols
  informers_bullets <- purrr::map2(
    .x = informers_bullets,
    .y = t_sym_map,
    .f = \(bullets, sym_map) replace_symbols(bullets, !!!sym_map)
  )
  informers_tests <- purrr::map2(
    .x = informers_tests,
    .y = t_sym_map,
    .f = \(test, sym_map) replace_symbols(test, !!!sym_map)
  )

  # Expression like `(.t1. <- test1) binary_op ... binary_op (.tn. <- testn)`
  test_expr <- switch(
    op,
    `&` = expr_and(!!!informers_tests, .double = FALSE),
    `&&` = expr_and(!!!informers_tests, .double = TRUE),
    `|` = expr_or(!!!informers_tests, .double = FALSE),
    `||` = expr_or(!!!informers_tests, .double = TRUE)
  )
  bullets <- rlang::call2(
    .fn = switch(op, `&` = , `&&` = "and_info", `|` = , `||` = "or_info"),
    !!!informers_bullets,
    header = header
  )
  body <- rlang::expr({
    # `force()` is just sugar within the output function. This makes it easy
    # to extract `test_expr` from a generated function.
    force(!!test_expr)
    bullets <- !!bullets
    cli::cli_div(theme = specifyr_theme())
    cli::cli_inform(c(bullets$header, bullets$bullets), class = "specifyr_message_info")
  })

  # Informers have arguments:
  # - `arg1`, `arg2`, ..., `argn`: inputs into the `test_expr`
  # - `arg1_name`, `arg2_name`, ..., `argn_name`: inputs into the `bullets`
  # Combine the arguments of all informers, putting the `*_name` arguments last.
  args <- evalidate_informer_arguments(informers, error_call = error_call)

  out <- rlang::new_function(
    args = args,
    body = body,
    env = env
  )
  class(out) <- c(
    "specifyr_info",
    switch(op, `&` = , `&&` = "specifyr_info_and", `|` = , `||` = "specifyr_info_or"),
    "function"
  )
  out
}

evalidate_informer_arguments <- function(informers, error_call = rlang::caller_env()) {
  # Collect the `informers` function arguments into a single list.
  # Using `do.call()` as vctrs::vec_c()` doesn't handle pairlists.
  informers_args <- do.call(`c`, purrr::map(informers, rlang::fn_fmls))

  # Every test argument `arg` has a `arg_name = rlang::caller_arg(arg)` sibling.
  # Users are not allowed to supply arguments ending in `_name`, so we can filter
  # to the test arguments simply by removing `_name` arguments.
  informers_args <- purrr::discard_at(
    x = informers_args,
    at = grep(x = rlang::names2(informers_args), pattern = "_name$")
  )
  informers_arg_nms <- rlang::names2(informers_args)

  # If no arguments in the `informers` share the same name, we're good to go
  duplicated_at <- duplicated(informers_arg_nms)
  if (!any(duplicated_at)) {
    return(c(informers_args, make_name_args(informers_arg_nms)))
  }

  # If the duplicate-name arguments have the same default value (including
  # `rlang::missing_arg()`), collapse the duplicates into a single argument.
  duplicate_nms <- informers_arg_nms[duplicated_at]
  duplicate_args <- split(informers_args, informers_arg_nms)[duplicate_nms]
  conflicting_args_at <- !purrr::map_lgl(duplicate_args, all_identical)
  if (!any(conflicting_args_at)) {
    test_args <- informers_args[!duplicated_at]
    return(c(test_args, make_name_args(rlang::names2(test_args))))
  }

  # To make the error message as informative as possible, we'll just emit
  # bullets for the first conflicting argument.
  conflicting_arg_nm <- duplicate_nms[conflicting_args_at][[1]]
  informers_args <- purrr::map(informers, rlang::fn_fmls)
  bad_informers_at <- purrr::map_lgl(
    .x = informers_args,
    .f = \(args) conflicting_arg_nm %in% rlang::names2(args)
  )
  conflicting_arg_values <- purrr::map(
    .x = informers_args[bad_informers_at],
    .f = \(args) args[[conflicting_arg_nm]]
  )

  bullets <- purrr::map2_chr(
    .x = paste0("..", which(bad_informers_at)),
    .y = conflicting_arg_values,
    \(informer_nm, conflicting_arg_value) {
      value <- if (rlang::is_missing(conflicting_arg_value)) {
        "no default value"
      } else {
        paste0("default value `", rlang::as_label(conflicting_arg_value), "`")
      }
      paste0("{.arg {conflicting_arg_nm}} has ", value, " in `",  informer_nm, "`.")
    }
  )
  bullets <- rlang::set_names(bullets, "*")
  if (n_bullets <- length(bullets) > 5) {
    length(bullets) <- 5
    bullets <- c(bullets, "... and {5 - n_bullets} more function{?s}.")
  }

  cli::cli_abort(
    c(
      "Functions supplied to `...` must have unique argument names, or the same default values.",
      x = paste(
        "Argument {.arg {conflicting_arg_nm}} has a different default value in",
        "functions {.arg {paste0(\"..\", which(bad_informers_at))}}."
      ),
      bullets
    ),
    call = error_call
  )
}

# Check ------------------------------------------------------------------------

check <- function(test, header = NULL, bullets = NULL, env = rlang::caller_env()) {

  # TODO: Improve error messages
  stopifnot(
    "`test` must be a <specifyr_test> or <specifyr_info> function." = is_test(test) || is_info(test),
    "`header` must be `NULL` or a character vector." = is.null(header) || is.character(header),
    "`bullets` must be `NULL` or a character vector." = is.null(bullets) || is.character(bullets),
    "`env` must be an environment" = is.environment(env)
  )

  informer <- if (is_test(test)) info(test) else test

  # Bullets is a call to `and_info()`, `or_info()`, or `single_info()`, all of
  # which store the informer's header in the `header` argument.
  default_bullets <- get_bullets(informer)
  default_header <- rlang::call_args(default_bullets)$header

  test_expr <- get_test(informer)
  informer_args <- rlang::fn_fmls(informer)

  # A `check()` returns the non-default arguments of it's `test`. Typically, a
  # test will contain one non-default value `x` (e.g. `test(is.numeric(x))`). If
  # - no non-default arguments, return `NULL`
  # - 1 non-default arguments (e.g. `x`), return it (e.g. `x`)
  # - > 1 non-default arguments, return a named list of the non-defaults values
  #   (e.g. `check_equal(x = 1, y = 1)` returns `list(x = 1, y = 1)`).
  non_defaults <- informer_args[purrr::map_lgl(as.list(informer_args), rlang::is_missing)]
  if (length(non_defaults) == 0) {
    return_value <- rlang::expr(invisible())
  } else if (length(non_defaults) == 1) {
    return_value <- rlang::sym(rlang::names2(non_defaults))
  } else {
    return_values <- rlang::set_names(
      rlang::sym(rlang::names2(non_defaults)),
      nm = rlang::names2(non_defaults)
    )
    return_value <- rlang::expr(list(!!!return_values))
  }

  args <- rlang::pairlist2(
    !!!informer_args,
    header = header,
    bullets = bullets,
    error_call = quote(rlang::caller_env()),
    error_class = quote(character()),
    error_ps = quote(character())
  )
  body <- rlang::expr({
    if (!!test_expr) {
      return(!!return_value)
    }
    # If the user supplied header is `NULL` in the output check function, fall
    # back to the informer's header.
    header <- header %||% !!default_header

    # We only want to evaluate the informer's bullets (potentially slow) in the
    # case where user supplied bullets are absent.
    if (is.null(bullets)) {
      bullets <- !!default_bullets
      bullets <- bullets$bullets
    }
    cli::cli_div(theme = specifyr_theme())
    cli::cli_abort(
      c(header, bullets, error_ps),
      call = error_call,
      class = c(error_class, "specifyr_check_fail_error")
    )
  })

  out <- rlang::new_function(
    args = args,
    body = body,
    env = env
  )
  class(out) <- c("specifyr_check", "function")
  out
}

is_check <- function(x) inherits(x, "specifyr_check")

# Messaging --------------------------------------------------------------------

# TODO: At some point we can probably make this not a class, since it's just a
# list used internally. Could save a little but of time.
new_info_message <- function(type, header, bullets) {
  structure(
    list(type = type, header = header, bullets = bullets),
    class = "info_message"
  )
}

is_untested <- function(info_message) info_message$type == "untested"
is_pass <- function(info_message) info_message$type == "pass"
is_fail <- function(info_message) info_message$type == "fail"

get_message <- function(info_message) c(info_message$header, info_message$bullets)

get_messages <- function(lst_of_info_message) {
  flatten_lst_of_chrish(purrr::map(lst_of_info_message, get_message))
}

# TODO: These `*_info` functions need to be exported and namespaced when they're
# used - so that external functions know how to access them.

# The atomic unit of information about a test
single_info <- function(pass, fail, result, header = character()) {
  if (is.null(result)) {
    # Over-write the bullets, we don't want to emit anything in un-tested cases
    type <- "untested"
    header <- bullets <- character()
  } else if (result) {
    type <- "pass"
    bullets <- pass
  } else {
    type <- "fail"
    bullets <- fail
  }
  new_info_message(type, header, bullets)
}

# Information about several tests connected via `&&` and/or `&`
and_info <- function(..., header = character(), pass_hints = FALSE) {
  messages <- purrr::discard(rlang::list2(...), is_untested)
  if (length(messages) == 0) {
    type <- "untested"
    header <- bullets <- character()
  } else if (all(purrr::map_lgl(messages, is_pass))) {
    type <- "pass"
    bullets <- get_messages(messages)
  } else {
    type <- "fail"
    # Remove the initial passes if no pass hints
    if (!pass_hints) {
      messages <- purrr::keep(messages, is_fail)
    }
    bullets <- get_messages(messages)
  }
  new_info_message(type, header, bullets)
}

# Information about several tests connected via `||`
or_info <- function(..., header = character()) {
  messages <- purrr::discard(rlang::list2(...), is_untested)
  if (length(info) == 0) {
    type <- "untested"
    header <- bullets <- character()
  } else if (any(purrr::map_lgl(messages, is_pass))) {
    type <- "pass"
    # Don't keep the fail messages prior to an eventual pass. E.g.
    # In `a || b || c`, if `c` is TRUE, we don't care about failed `a`, `b`.
    messages <- purrr::keep(messages, is_pass)
    bullets <- get_messages(messages)
  } else {
    type <- "fail"
    bullets <- get_messages(messages)
  }
  new_info_message(type, header, bullets)
}

#' Flatten a list of character-ish vectors to a single character vector.
#'
#' @param lst_of_chr A list of <character> or <cli_ansi_string> vectors.
#' @param name_spec Argument passed to `vctrs::list_unchop()`.
#'
#' @return A character vector.
#' @noRd
flatten_lst_of_chrish <- function(lst_of_chr, name_spec = "{outer}") {
  lst_of_chr <- purrr::modify_at(
    .x = lst_of_chr,
    .at = \(x) inherits(x, "cli_ansi_string"),
    .f = as.character
  )
  vctrs::list_unchop(lst_of_chr, ptype = character(), name_spec = name_spec)
}

# Helpers ----------------------------------------------------------------------

# Extracts the `test` expression from {specifyr} functions' bodies
get_test <- function(fn, ...) {
  UseMethod("get_test")
}

get_test.specifyr_test <- function(fn) {
  # <specifyr_info> function bodies look like this:
  # { <test to get> }
  # Index `[[2]]` gets expression `<test to get>` (i.e. first arg to `{`)
  rlang::fn_body(fn)[[2]]
}

get_test.specifyr_info <- function(fn) {
  # <specifyr_info> function bodies look like this:
  # {
  #   force(<test to get>)
  #   <other stuff>
  # }
  # Index `[[2]][[2]]` gets expression `<test to get>`
  # I.e. first arg of `{`, then first arg of `force()`
  rlang::fn_body(fn)[[2]][[2]]
}

get_test.specifyr_check <- function(fn) {
  # <specifyr_check> function bodies look like this:
  # {
  #   if (<test to get>) {
  #     return(<return value>)
  #   }
  #   <other stuff>
  # }
  # Index `[[2]][[2]]` gets expression `<test to get>`
  # I.e. first arg of `{`, then the first arg of `if`
  rlang::fn_body(fn)[[2]][[2]]
}

# Extracts the bullets-generating expression from {specifyr} functions' bodies
get_bullets <- function(fn, ...) {
  UseMethod("get_bullets")
}

get_bullets.specifyr_info <- function(fn) {
  # <specifyr_info> function bodies look like this:
  # {
  #   force(test)
  #   bullets <- <bullets to get>
  #   <other stuff>
  # }
  # Index `[[3]][[3]]` gets expression `<bullets to get>`
  rlang::fn_body(fn)[[3]][[3]]
}

get_bullets.specifyr_check <- function(fn) {
  # <specifyr_check> function bodies look like this:
  # {
  #   if (<test>) {
  #     return(<return value>)
  #   }
  #   header <- <header expression>
  #   if (!is.null(bullets)) {
  #     bullets <- <bullets to get> # <<- THIS IS WHAT YOU WANT
  #     bullets <- bullets$bullets
  #   }
  #   <other stuff>
  # }
  # Index `[[4]][[3]][[2]][[3]]` gets expression `<bullets to get>`
  rlang::fn_body(fn)[[4]][[3]][[2]][[3]]
}

# Given a test expression of the form `.t1. <- t1 op ... op .tn. <- tn` where
# `t*` is a user supplied `test()` expression, create a named list of `t*`
# expressions with `.t*.` symbols as the names.
test_expr_dict <- function(expr) {
  stopifnot("`test` must be an expression." = rlang::is_expression(expr))

  t_symbol_pattern <- "\\.t[0-9]+\\."
  dict <- list()

  is_t_assignment <- function(call) {
    if (!identical(call[[1]], quote(`<-`))) {
      return(FALSE)
    }
    grepl(x = rlang::as_string(call[[2]]), pattern = t_symbol_pattern)
  }
  get_t_assignments <- function(expr) {
    if (!rlang::is_call(expr)) {
      # By design, a t-assignment will only ever exist within a call
      return()
    } else if (is_t_assignment(expr)) {
      # The LHS (`x`) argument of the `<-` call is the dictionary key. The
      # RHS (`value`) argument is the entire user-supplied expression.
      t_symbol <- expr[[2]]
      dict[[t_symbol]] <<- expr[[3]]
    } else {
      # The recursive case, we check all of the call arguments for t-assignments
      purrr::map(as.list(expr[-1]), get_t_assignments)
    }
  }

  # Add the t-assignments to the dictionary
  get_t_assignments(expr)
  dict
}

# Grabs the value of `symbol` from the caller environment,
# returns `NULL` if `symbol` doesn't exist.
env_get_result <- function(symbol) {
  env <- rlang::caller_env()
  rlang::env_get(
    env = env,
    nm = rlang::as_string(rlang::enexpr(symbol)),
    default = NULL
  )
}

# Interactive Testing ----------------------------------------------------------

if (FALSE) {

  # Info ----

  ask_is_numeric <- info(
    test = test(is.numeric(x)),
    pass = "{.arg {x_name}} is numeric.",
    fail = "{.arg {x_name}} is {.a {x}}.",
    header = character()
  )
  print(ask_is_numeric)

  ask_is_character <- info(
    test = test(is.character(x)),
    pass = "{.arg {x_name}} is a character.",
    fail = "{.arg {x_name}} is {.a {x}}.",
    header = character()
  )

  ask_is_scalar <- info(
    test = test(length(x) == 1),
    pass = "{.arg {x_name}} is scalar.",
    fail = "{.arg {x_name}} is length {length(x)}.",
    header = character()
  )

  ask_is_above <- info(
    test = test(all(x > above, na.rm = TRUE), x = rlang::missing_arg(), above = 0),
    pass = "{.arg {x_name}} is above {above}.",
    fail = "{.arg {x_name}} has a minimum of {min(x, na.rm = TRUE)}.",
    header = character()
  )

  ask_is_non_missing <- info(
    test = test(!anyNA(x)),
    pass = "{.arg {x_name}} is non-missing.",
    fail = "{.arg {x_name}} is NA or NaN {.at {is.na(x)}}.",
    header = character()
  )

  ask_is_scalar_number_above <- and(
    .header = "{.arg {x_name}} must be a number greater than {above}.",
    ask_is_numeric,
    ask_is_scalar,
    ask_is_above
  )

  ask_is_scalar_non_missing_character <- and(
    .header = "{.arg {x_name}} must be a non-missing string.",
    ask_is_character,
    list(ask_is_scalar, ask_is_non_missing)
  )

  scalar_num_above_or_non_missing_string <- or(
    .header = c(
      "{.arg {x_name}} must be either:",
      `*` = "a number greater than {above}.",
      `*` = "a non-missing string."
    ),
    ask_is_scalar_number_above,
    ask_is_scalar_non_missing_character
  )

  # Pass
  scalar_num_above_or_non_missing_string(10)
  scalar_num_above_or_non_missing_string("A")

  # Fail
  scalar_num_above_or_non_missing_string(c(NA_character_, "A"))
  scalar_num_above_or_non_missing_string(1:2)

  # Check ----

  check_scalar_num_above_or_non_missing_string <- check(
    test = scalar_num_above_or_non_missing_string
  )
  print(check_scalar_num_above_or_non_missing_string)

  # Pass
  check_scalar_num_above_or_non_missing_string(10)
  check_scalar_num_above_or_non_missing_string("A")

  # Fail
  check_scalar_num_above_or_non_missing_string(1:2)

  check_numeric <- check(
    test = info(
      test = test(is.numeric(x)),
      fail = "{.arg {x_name}} is {.a {x}}.",
      pass = "",
      header = "{.arg {x_name}} must be numeric."
    )
  )

  # Making int() ----

  is_integer <- test(is.integer(x))

  is_in_range <- test(
    all(lower < x[!is.na(x)] & x[!is.na(x)] < upper),
    x = ,
    lower = -Inf,
    upper = Inf
  )

  is_length <- test(
    (is.null(len) || length(x) == len) &&
    (is.null(min_len) || length(x) >= min_len) &&
    (is.null(max_len) || length(x) <= max_len),
    x = ,
    len = NULL,
    min_len = NULL,
    max_len = NULL
  )

  is_finite <- test(!finite || all(is.finite(x)), x = , finite = FALSE)

  is_null <- test(null_ok || is.null(x), x = , null_ok = TRUE)

  is_missing <- test(na_ok || !anyNA(x), x = , na_ok = FALSE)

  int <- check(
    test = and(
      is_integer,
      list(
        is_missing,
        is_length,
        is_in_range,
        is_finite,
        is_null
      ),
      .header = "`x` must be an integer."
    )
  )

  i <- and(
    is_integer,
    list(
      is_missing,
      is_length,
      is_in_range,
      is_finite,
      is_null
    ),
    .header = "`x` must be an integer."
  )
  bench::mark(
    try(int(c(1:50, NA), na_ok = FALSE, min_len = 20, max_len = 60)),
    try(checkmate::assert_integer(c(1:50, NA), any.missing = FALSE, min.len = 20, max.len = 60)),
    check = FALSE
  )
}
