# todo -------------------------------------------------------------------------

# IMPORTANT:
# Define what is currently in `type-check-maker` at the top of this script, use
# it to define the remaining functions. That is, use `new_vector_fun`,
# `new_object_type`, and `new_list_of_type`, to define all of the functions here.
#
# While you're at it, move aliases (e.g. `bool`, `string`, `count`) to an alias
# script and generate them there.

# Don't check if the argument is missing:
# Taking a cue from {typed}, we can check if arguments are missing manually within
# a function. This provides more freedom to have some arguments able to be missing
# and others not allowed to be missing.

# TODO:
# Make a shim of the error checkers that you use internally (i.e `int_`), so that
# you don't need to worry about circular dependencies.

# TODO:
# - Add Collate to change the build order of files, so I can safely build the type
#   checks
# - https://stackoverflow.com/questions/18441038/changing-the-load-order-of-files-in-an-r-package
# - ahhh, makes sense that R runs alphabetically, that's why zzz.R is the package
#   load file convention

# Implement other types:
# - POSIXct (psx)
# - Date (dte)
# - factor (fac)

# Major:
# - I want fully custom error headers (must be a string), since I find
#   myself sometimes limited by the fixed header in trying to describe why the
#   error occurred.
#   - might as well provide custom error bullets as well... It's more likely
#     that only the header is supplied, but you might want to suppress the
#     bullets with `error_bullets = NULL`

# constructor ------------------------------------------------------------------

# TODO:
# - you might want to allow the user to add `checks = list()`
# - you could add `"alias"` to `fun_is`, it wouldn't be too difficult to
#   define an alias via this function
#   - `new_vector_aliaser()` could be a function which generates an alias generator...
#   - you'd potentially want an alias generator if you need to use different versions
#     of a type alias (e.g. `a_int(len = 1L)`, `a_int(nas = FALSE)`, etc.)
new_vector_fun <- function(
    # NOTE: Make sure it's clear in the documentation that `type_test` should
    #       return FALSE on `NULL` values, to prevent `NULL` values from having
    #       their length, bounds, etc. tested (possibly throwing an error). This
    #       is usually the case for `is_*()` and `is.*()` functions anyways.
    type_test,
    type_desc,
    ...,
    fun_is = c("check", "test", "alias", "aliaser"), # TODO: Create `alias` option
    checks = list(),
    x_name = NULL,
    error_header = NULL,
    error_bullets = NULL,
    error_class = "specifyr_error_mistyped"
  ) {

  parent_env <- rlang::caller_env()

  type_test <- rlang::enexpr(type_test)
  type_desc <- chr_(type_desc, min_len = 1, max_len = 2, nas = FALSE)
  fun_is <- rlang::arg_match(fun_is)
  checks <- check_is_list_of_check(checks)
  error_class <- chr_(error_class, nas = FALSE)
  x_name <- chr_(x_name, len = 1, nas = FALSE, null = TRUE)
  error_header <- chr_(error_header, len = 1, nas = FALSE, null = TRUE)
  error_bullets <- chr_(error_bullets, nas = FALSE, null = TRUE)

  properties <- check_property_dots(...)
  property_nms <- rlang::names2(properties)

  # The `test` part of supplied checks, each is an expression like `x %% 2 == 0`
  check_tests <- lapply(checks, as_test)

  # Expression like `is.Date(x) && length(x) == len && ...`
  type_and_prop_test <- make_base_variable_properties_test(type_test, property_nms)

  # `is.Date(x) && length(x) == len && ... && check_test1(x) && ...`
  test <- expr_and(type_and_prop_test, !!!check_tests)

  # Return early if we're generating a test function
  if (fun_is == "test") {
    out <- rlang::new_function(
      args = rlang::pairlist2(x = , !!!properties),
      body = test,
      env = parent_env
    )
    class(out) <- c("specifyr_type_test", "function")
    return(out)
  }

  # `stop_vector_mistyped(x, x_name, len = len, nas = nas, ...)`
  stop_mistyped <- make_variable_stop_vector_mistyped(
    type_desc = type_desc,
    type_test = type_test,
    property_nms = property_nms
  )

  if (!is_empty(checks)) {
    # `x` failed either the `type_and_prop_test` or one of `checks`. We retest
    # `x` to determine whether to raise a mistyped error or a check error.
    stop_mistyped <- rlang::expr(if (!(!!type_and_prop_test)) { !!stop_mistyped })

    # ith element is `if (!check_tests[[i]]) { check_stops[[i]](x, x_name, ...) }`
    stop_if_checks_failed <- .mapply(
      FUN = \(stop, test) { rlang::expr(if (!(!!test)) { !!stop }) },
      dots = list(
        stop = lapply(checks, as_stop),
        test = check_tests
      ),
      MoreArgs = list()
    )
  } else {
    stop_if_checks_failed <- list()
  }

  #> # Return `x` if the type, properties, and checks are correct
  #> if (is.numeric(x) && (nas || !anyNA(x)) && check_test1(x)) { return(x) }
  #>
  #> # Raise a mistyped error if the type or properties are incorrect
  #> if (!(is.numeric(x) && (nas || !anyNA(x))) { stop_mistyped(x, ...) }
  #>
  #> # Raise a check error if the corresponding check failed
  #> if (!check_test1(x)) { stop_check1(x, ...) }`
  body <- expr_squash(
    rlang::expr(if (!!test) { return(x) }),
    stop_mistyped,
    !!!stop_if_checks_failed,
    # Type check function should never reach this stage, we raise an error if so
    quote(specifyr::stop_malformed_type_check())
  )
  args <- rlang::pairlist2(
    x = ,
    !!!properties,
    error_call = quote(rlang::caller_env()),
    error_class = error_class,
    x_name = x_name %||% quote(rlang::caller_arg(x)),
    error_header = error_header,
    error_bullets = error_bullets
  )
  out <- rlang::new_function(
    args = args,
    body = body,
    env = parent_env
  )
  class(out) <- c("specifyr_type_check", "function")
  out
}

# TODO: `list_of_type` creates a function like `lst_of_int` by taking another
#       type function and grabbing it's formals to make the list of function.
#
# Produce a function with arguments:
# - inner vector properties (i.e. `nas`, `len`, etc.)
# - list prefixed properties (i.e. `lst_len`)
# - usual args. `error_header`, `error_bullets`
#   - versions for the list, `lst_error_header`, `lst_error_bullets`

new_list_of_type <- function(
    type_fun,
    checks = list(),
    x_name = NULL,
    error_header = NULL,
    error_bullets = NULL,
    lst_error_header = error_header,
    lst_error_bullets = NULL,
    error_class = "specifyr_error_mistyped"
  ) {

  type_fun_defused <- rlang::enexpr(type_fun)
  if (!rlang::is_symbol(type_fun_defused)) {
    cli::cli_abort(
      c(
        "{.arg type_fun} must be a type test or type check function as a symbol.",
        x = "{.arg type_fun} is {.obj_type_friendly {type_fun_defused}}."
      ),
      class = c("specifyr_error_invalid_arg", "specifyr_error")
    )
  } else if (!is_type_test(type_fun) && !is_type_check(type_fun)) {
    fun_(type_fun)
    cli::cli_abort(
      "{.arg type_fun} must be a type test or type check function.",
      class = c("specifyr_error_invalid_arg", "specifyr_error")
    )
  }

  checks <- check_is_list_of_check(checks)
  x_name <- x_name %!|% string_(x_name)
  error_header <- error_header %!|% string_(error_header)
  error_bullets <- chr_(error_bullets, nas = FALSE, null = TRUE)
  lst_error_header = lst_error_header %!|% string_(lst_error_header)
  lst_error_bullets = chr_(lst_error_bullets, nas = FALSE, null = TRUE)
  error_class <- chr_(error_class, nas = FALSE)

  type_fun_fmls <- rlang::fn_fmls(type_fun)
  non_property_fmls <- c(
    "x",
    "error_call",
    "error_class",
    "x_name",
    "x_must",
    "error_header",
    "error_bullets"
  )
  type_property_nms <- setdiff(names(type_fun_fmls), non_property_fmls)
  type_properties <- type_fun_fmls[type_property_nms]

  check_tests <- lapply(checks, as_test)
  list_type_test <- rlang::expr(
    checkmate::test_list(
      x = x,
      len = lst_len,
      null.ok = lst_null,
      min.len = lst_min_len,
      max.len = lst_max_len
    )
  )

  if (is_type_test(type_fun)) {
    # `all(vapply(X = x, FUN = test_int, ..., len = len, nas = nas))`
    element_type_test <- rlang::expr(
      all(vapply(
        X = x,
        FUN = !!type_fun_defused,
        FUN.VALUE = logical(1L),
        !!!rlang::set_names(
          x = rlang::syms(type_property_nms),
          nm = type_property_nms
        )
      ))
    )

    # `checkmate::test_list(...) && all(vapply(x, test_int, ...)) && check1(x) ...`
    body <- expr_and(list_type_test, element_type_test, !!!check_tests)

    args = rlang::pairlist2(
      x = ,
      !!!type_properties,
      lst_len = NULL,
      lst_min_len = NULL,
      lst_max_len = NULL,
      lst_null = FALSE
    )
    out <- rlang::new_function(
      args = args,
      body = body,
      env = rlang::fn_env(type_fun)
    )
    class(out) <- c("specifyr_list_of_test", class(type_fun))
    return(out)
  }

  # Return early if NULL, so we don't run any additional checks or try to test
  # the elements of `x` when `x` is NULL.
  return_if_null <- rlang::expr(if (lst_null && is.null(x)) { return(x) })

  # `if (!list_type_test) stop_vector_mistyped(x, x_name, len = lst_len, ...)`
  stop_if_list_mistyped <- make_fixed_stop_vector_mistyped(
    type_desc = c("a", "list"),
    type_test = rlang::expr(is.list(x)),
    properties = list(
      len = rlang::sym("lst_len"),
      min_len = rlang::sym("lst_min_len"),
      max_len = rlang::sym("lst_max_len"),
      null = rlang::sym("lst_null")
    ),
    error_header = rlang::sym("lst_error_header"),
    error_bullets = rlang::sym("lst_error_bullets")
  )
  stop_if_list_mistyped <- rlang::expr(
    if (!(!!list_type_test)) {
      !!stop_if_list_mistyped
    }
  )

  # ith element is `if (!check_tests[[i]]) { check_stops[[i]](x, x_name, ...) }`
  stop_if_checks_failed <- .mapply(
    FUN = \(stop, test) { rlang::expr(if (!(!!test)) { !!stop }) },
    dots = list(
      stop = lapply(checks, as_stop),
      test = check_tests
    ),
    MoreArgs = list()
  )

  more_args <- c(
    type_property_nms,
    "error_call",
    "error_class",
    "error_header",
    "error_bullets"
  )
  more_args <- rlang::set_names(
    x = rlang::syms(more_args),
    nm = more_args
  )

  # `.mapply(FUN = int, dots = list(x = x, x_name = x_name), MoreArgs = list(...))`
  stop_if_elements_mistyped <- rlang::expr(
    .mapply(
      FUN = !!type_fun_defused,
      dots = list(x = x, sprintf("%s[[%i]]", x_name, seq_along(x))),
      MoreArgs = list(!!!more_args)
    )
  )

  #> # Return `x` if it's NULL and `lst_null == TRUE`
  #> if (lst_null && is.null(x)) { return(x) }
  #>
  #> # Raise a mistyped error if `x` is not a list
  #> if (!(checkmate::test_list(...)) { stop_mistyped(x, ...) }
  #>
  #> # Raise a mistyped error if elements of `x` don't pass check in `type_fun`
  #> .mapply(FUN = type_fun, dots = list(x = x, x_name = x_name), ...)
  #>
  #> # Raise a check error if the corresponding check failed
  #> if (!check_test1(x)) { stop_check1(x, ...) }`
  #>
  #> # Return the checked list `x`
  #> return(x)
  body <- expr_squash(
    return_if_null,
    stop_if_list_mistyped,
    stop_if_elements_mistyped,
    !!!stop_if_checks_failed,
    rlang::sym("x")
  )
  args <- rlang::pairlist2(
    x = ,
    !!!type_properties,
    lst_len = NULL,
    lst_null = FALSE,
    lst_min_len = NULL,
    lst_max_len = NULL,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::caller_env()),
    error_class = "specifyr_error_mistyped",
    error_header = error_header,
    error_bullets = error_bullets,
    lst_error_header = lst_error_header,
    lst_error_bullets = lst_error_bullets
  )
  out <- rlang::new_function(
    args = args,
    body = body,
    env = rlang::fn_env(type_fun)
  )
  class(out) <- c("specifyr_list_of_check", class(type_fun))
  out
}

# TODO: Make this an external function, which matches `new_vector_fun`, but allows
#       you to submit a builtin class name instead of a type check, and uses checkmate
#       on the backend.
#
# Internal function factory, used for building the type check functions
# provided by {specifyr}
new_builtin_type <- function(
    type_class,
    type_desc,
    ...,
    fun_is = c("check", "test"),
    x_must = NULL,
    error_header = NULL,
    error_bullets = NULL
  ) {

  parent_env <- rlang::caller_env()

  type_test <- make_class_to_type_test(type_class)
  properties <- rlang::list2(...)
  property_nms <- rlang::names2(properties)
  fun_is <- rlang::arg_match(fun_is)

  # Expression like `checkmate::test_integer(x, len = len, any.missing = nas)`
  test <- make_checkmate_variable_properties_test(type_class, property_nms)
  if (fun_is == "test") {
    out <- rlang::new_function(
      args = rlang::pairlist2(x = , !!!properties),
      body = test,
      env = parent_env
    )
    class(out) <- c("specifyr_builtin_type_test", "specifyr_type_test", "function")
    return(out)
  }

  # `stop_vector_mistyped(x, x_name, len = len, nas = nas, ...)`
  stop_mistyped <- make_variable_stop_vector_mistyped(
    type_desc = type_desc,
    type_test = type_test,
    property_nms = property_nms
  )

  body <- rlang::expr({
    if (!!test) {
      return(x)
    }
    !!stop_mistyped
  })
  args <- rlang::pairlist2(
    x = ,
    !!!properties,
    error_call = quote(rlang::caller_env()),
    error_class = "specifyr_error_mistyped",
    x_name = quote(rlang::caller_arg(x)),
    x_must = x_must,
    error_header = error_header,
    error_bullets = error_bullets
  )
  out <- rlang::new_function(
    args = args,
    body = body,
    env = parent_env
  )
  class(out) <- c("specifyr_builtin_type_check", "specifyr_type_check", "function")
  out
}

# internal constructor ---------------------------------------------------------

# TODO: Using this function for all of the internal {specifyr} functions (i.e.
# `int`, `test_int`, `intish`, etc.)
new_specifyr_builtin_fun <- function(
  type_class,
  type_desc,
  ...,
  stop_with = quote(specifyr::stop_mistyped_vector),
  fun_is = c("check", "test", "alias", "aliaser"), # TODO: Create `alias` option
  checks = list(),
  x_name = NULL,
  error_header = NULL,
  error_bullets = NULL,
  error_class = "specifyr_error_mistyped"
  ) {

}

new_specifyr_vector_fun <- function(
    type_test,
    type_desc,
    ...,
    stop_with = quote(specifyr::stop_mistyped_vector),
    fun_is = c("check", "test", "alias", "aliaser"), # TODO: Create `alias` option
    checks = list(),
    x_name = NULL,
    error_header = NULL,
    error_bullets = NULL,
    error_class = "specifyr_error_mistyped"
) {

}

# TODO:
# - create custom `stop_mistyped_vector` functions for `intish`, `chr`, `fac`,
#   and others that require a custom check

# helpers ----------------------------------------------------------------------

is_type_check <- function(x) {
  inherits(x, "specifyr_type_check")
}

is_type_test <- function(x) {
  inherits(x, "specifyr_type_test")
}

# repro of lower not found error -----------------------------------------------

# SOLVED: Look through all of the examples until the end, where the simplest repo
#         is made.
if (FALSE) {

  library(rlang)

  # Paste a prefix and suffix onto `x`
  fun <- function(x, prefix = "<", suffix = ">") {
    paste0(prefix, x, suffix)
  }

  # Make a version of `fun()` which is vectorized over a list `x`, with
  # different `prefix` and `suffix` defaults.
  make_fun <- function(prefix, suffix, happy_path = TRUE) {

    # Create symbols for prefix and suffix
    more_args <- c("prefix", "suffix")
    more_args <- set_names(syms(more_args), more_args)

    if (happy_path) {
      # This works
      body <- expr(
        mapply(
          FUN = fun,
          x = x,
          MoreArgs = list(!!!more_args),
          SIMPLIFY = TRUE
        )
      )
    } else {
      # This does not work, but the output looks identical
      body <- expr(
        mapply(
          FUN = fun,
          x = x,
          MoreArgs = !!more_args,
          SIMPLIFY = TRUE
        )
      )
    }

    # Create and return the new function
    env <- rlang::caller_env()
    new_function(
      args = pairlist2(x = , prefix = prefix, suffix = suffix),
      body = body,
      env = env
    )
  }

  # Create the function with specified prefix and suffix
  fun_happy <- make_fun("[", "]", happy_path = TRUE)
  fun_sad   <- make_fun("[", "]", happy_path = FALSE)

  # They're not identical
  identical(fun_happy, fun_sad)

  # They look identical
  print(fun_happy)
  print(fun_sad)

  # When you convert the call to a character, `MoreArgs` drops it's names
  as.character(fn_body(fun_happy))
  as.character(fn_body(fun_sad))

  # They look identical
  print(fun_happy)
  print(fun_sad)

  fun_happy(1:5)
  fun_sad(1:5)

  # Here's a super minimal example of what's happening

  more_args <- set_names(syms(c("a", "b")), c("a", "b"))
  f1 <- new_function(
    args = pairlist2(a = , b = ),
    body = rlang::expr(!!more_args)
  )

  # This returns the list(a = a, b = b), where `a` and `b` are still symbols.
  # So, I've injected THIS list, which was evaluated when `a`, `b` were symbols...
  #
  # The problem, I think, is that I've actually injected a `list()` object and NOT
  # a call to `list()` the function. These look the same, but are not.
  f1(1, 2)

  # The first is a list, the second is a call to list. When I inject the first,
  # I am injecting an already created list of symbols. When I inject the second,
  # I am injecting an unevaluated call to list.
  class(more_args)
  class(rlang::expr(list(!!!more_args)))

  # The problem lies in the fact that, when I put a list of named symbols `a`, `b`
  # in the body of a function, it looks identical to a call to `list(a = a, b = b)`.
  new_function(args = pairlist2(x = ), body = more_args)
  new_function(args = pairlist2(x = ), body = rlang::expr(list(!!!more_args)))

  # Here, I think `a = a, b = b` are injected as arguments to `list()`? So they
  # are evaluated in the correct context. The question is, if I wanted to make
  # a `list(a = a, b = b)` OUTSIDE of the body expression, but have it evaluated
  # correctly in the body, how would I do that?
  f2 <- new_function(
    args = pairlist2(a = , b = ),
    body = rlang::expr(list(!!!more_args))
  )
  f2(1, 2)

  # What do I change here the have the list injected?
  more_args <- set_names(syms(c("a", "b")), c("a", "b"))

  # Both of these work!
  more_args1 <- rlang::call2(.fn = "list", a = sym("a"), b = sym("b"))
  more_args2 <- rlang::call2(.fn = "list", !!!more_args)
  identical(more_args1, more_args2)

  f3 <- new_function(
    args = pairlist2(a = , b = ),
    body = more_args1
  )
  f3(1, 2)

  # Base version of the riddle
  f1 <- f2 <- function(x) {}
  methods::functionBody(f1) <- quote(list(x))
  methods::functionBody(f2) <- list(quote(x))
  f1(10)
  f2(10)

  # Riddle
  f1 <- rlang::new_function(args = pairlist2(x = ), body = expr(list(!!sym("x"))))
  f2 <- rlang::new_function(args = pairlist2(x = ), body = list(sym("x")))

  # Two functions, `f1` and `f2`
  print(f1)
  print(f2)

  # The function body looks identical
  identical(as.character(fn_body(f1)), as.character(fn_body(f2)))
  as.character(fn_body(f1))

  # Both defined in the global environment
  identical(fn_env(f1), fn_env(f2))
  fn_env(f1)

  # Both evaluated in the global environment
  identical(f1(10), f2(10))
  f1(10)
  f2(10)
}

# testing ----------------------------------------------------------------------

if (FALSE) {

  stop_if_elements_mistyped <- rlang::call2(
    .fn = ".mapply",
    FUN = type_fun_defused,
    dots = list(
      x = rlang::sym("x"),
      x_name = rlang::expr(sprintf("%s[[%i]]", x_name, seq_along(x)))
    ),
    MoreArgs = more_args
  )

  FUN <- rlang::expr(int)

  more_args <- c(
    "len"
  )
  more_args <- rlang::set_names(
    x = as.list(rlang::syms(more_args)),
    nm = more_args
  )

  call_test <- rlang::new_call(
    car = rlang::sym("mapply"),
    rlang::pairlist2(
      FUN = FUN,
      x = quote(x),
      x_name = rlang::expr(sprintf("%s[[%i]]", x_name, seq_along(x))),
      MoreArgs = more_args
  ))

  FUN = FUN
  dots = list(
    x = rlang::expr(x),
    x_name = rlang::expr(sprintf("%s[[%i]]", x_name, seq_along(x)))
  )
  MoreArgs = more_args

  call_test <- rlang::expr(
    mapply(FUN = !!FUN, !!dots, MoreArgs = !!more_args)
  )

  x <- list(1L, "A")
  x_name <- "x"
  len <- 1L

  eval(call_test)
}

