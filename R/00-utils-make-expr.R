
# tests ------------------------------------------------------------------------

# TODO: Document the `make_base_fixed/variable_properties_test()`. This is a
#       complete version of `make_checkmate_fixed/variable_properties_test()`
#       using base R functions instead of {checkmate}.

# TODO: Document
make_base_fixed_properties_test <- function(
    type_test,
    properties,
    x_expr = rlang::sym("x")
  ) {

  is_scalar <- isTRUE(properties$len == 1) ||
    (isTRUE(properties$min_len == 1) && isTRUE(properties$max_len == 1))

  property_tests <- list(
    len = if (!is.null(properties$len)) rlang::expr(length(!!x_expr) == !!properties$len),
    min_len = if (!is.null(properties$min_len)) rlang::expr(length(!!x_expr) >= !!properties$min_len),
    max_len = if (!is.null(properties$max_len)) rlang::expr(length(!!x_expr) <= !!properties$max_len),
    # If `x_expr` is scalar, we can use `is.na()` instead of `anyNA()`
    nas = if (isFALSE(properties$nas)) {
      if (is_scalar) {
        rlang::expr(!is.na(!!x_expr))
      } else {
        rlang::expr(!anyNA(!!x_expr))
      }
    },
    # If `x_expr` is scalar and non-NA, don't need `all()` in `lower` or `upper`
    lower = if (!is.null(properties$lower)) {
      if (is_scalar && isFALSE(properties$nas)) {
        rlang::expr(!!x_expr >= !!properties$lower)
      } else {
        rlang::expr(all((!!x_expr)[!is.na(!!x_expr)] >= !!properties$lower))
      }
    },
    upper = if (!is.null(properties$upper)) {
      if (is_scalar && isFALSE(properties$nas)) {
        rlang::expr(!!x_expr <= !!properties$upper)
      } else {
        rlang::expr(all((!!x_expr)[!is.na(!!x_expr)] <= !!properties$upper))
      }
    },
    finite = if (isTRUE(properties$finite)) rlang::expr(is.finite(!!x_expr))
  )

  # The order of the tests is important, `intersect()` maintains that order
  property_nms <- rlang::names2(properties)
  tests <- property_tests[intersect(names(property_tests), property_nms)]

  # If both `lower` and `upper` are supplied, testing both bounds within the
  # same `all()` is faster
  if ("lower" %in% property_nms && "upper" %in% property_nms) {
    tests$lower <- tests$upper <- NULL
    if (is_scalar && isFALSE(properties$nas)) {
      tests$bounds <- rlang::expr(
        !!properties$lower <= !!x_expr && !!x_expr <= !!properties$upper
      )
    } else {
      tests$bounds <- rlang::expr(
        all(
          !!properties$lower <= (!!x_expr)[!is.na(!!x_expr)] &
            (!!x_expr)[!is.na(!!x_expr)] <= !!properties$upper
        )
      )
    }
  }

  # Expression like `is.numeric(x) && !anyNA(x) && length(x) == 1 && ...`
  # If `null` is allowed, like (is.null(x)) || (is.numeric(x) && ...)
  if ("null" %in% property_nms) {
    expr_or(rlang::expr(is.null(!!x_expr)), expr_and(type_test, !!!tests))
  } else {
    expr_and(rlang::expr(!is.null(!!x_expr)), type_test, !!!tests)
  }
}

# TODO: Document
make_base_variable_properties_test <- function(
    type_test,
    property_nms,
    x_expr = rlang::sym("x")
  ) {

  property_tests <- list(
    len = rlang::expr(is.null(len) || length(!!x_expr) == len),
    min_len = rlang::expr(is.null(min_len) || length(!!x_expr) >= min_len),
    max_len = rlang::expr(is.null(max_len) || length(!!x_expr) <= max_len),
    nas = rlang::expr(nas || !anyNA(!!x_expr)),
    lower = rlang::expr(is.null(lower) || all((!!x_expr)[!is.na(!!x_expr)] >= lower)),
    upper = rlang::expr(is.null(upper) || all((!!x_expr)[!is.na(!!x_expr)] <= upper)),
    finite = rlang::expr(!finite || is.finite(!!x_expr))
  )

  # The order of the tests is important, `intersect()` maintains that order
  tests <- property_tests[intersect(names(property_tests), property_nms)]

  # Expression like `is.numeric(x) && (is.null(len) || length(x) == len) && ...`
  # If `null` is allowed, like (null && is.null(x)) || (is.numeric(x) && ...)
  if ("null" %in% property_nms) {
    expr_or(rlang::expr(null && is.null(!!x_expr)), expr_and(type_test, !!!tests))
  } else {
    expr_and(rlang::expr(!is.null(!!x_expr)), type_test, !!!tests)
  }
}

#' Make an expression to test an object `x` using a fixed `checkmate::qtest`
#'
#' @details
#'
#' This function generates a fixed test expression, such as that used in an
#' alias, e.g. `checkmate::qtest(x, "S1")` (tests whether `x` is a string). See
#' [make_checkmate_variable_properties_test()] to generate a variable test
#' expression, such as that used in type checks.
#'
#' @param type_class `[character(1)]`
#'
#' A built-in object class, as a string, e.g. `"logical"`, `"environment"`. The
#' returned call tests whether an object `x` is this class.
#'
#' @param properties `[list]`
#'
#' A list of property, expected value pairs, e.g. `list(nas = FALSE, len = 1L)`
#' (says to test that `x` contains no `NA` values and is length 1). The returned
#' call tests whether an object `x` has these properties.
#'
#' @return
#'
#' An unevaluated [checkmate::qtest()] call to test an object `x`,
#' e.g. `checkmate::qtest(x, "B1")` (tests whether `x` is a boolean).
#' @noRd
make_checkmate_fixed_properties_test <- function(
    type_class,
    properties,
    x_expr = rlang::sym("x")
  ) {

  rules <- switch(
    type_class,
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
    cli::cli_abort("Unknown class {.val {type_class}}.", .internal = TRUE)
  )

  # {checkmate} sets the class rule to uppercase if NA values are prohibited
  if (isFALSE(properties$nas)) rules <- toupper(rules)

  # {checkmate} adds the length after the class rule, can use >= or <=, but
  # can't define a range - so we'll add an extra min length test if necessary.
  len <- properties$len
  min_len <- properties$min_len
  max_len <- properties$max_len
  if (!is.null(len)) {
    rules <- paste0(rules, len)
  } else if (!is.null(min_len) && is.null(max_len)) {
    rules <- paste0(rules, ">=", min_len)
  } else if (is.null(min_len) && !is.null(max_len)) {
    rules <- paste0(rules, "<=", max_len)
  }
  if (!is.null(min_len) && !is.null(max_len)) {
    rules <- paste0(rules, "<=", max_len)
    extra_len_test <- rlang::expr(length(x) >= !!min_len)
  } else {
    extra_len_test <- NULL
  }

  finite <- properties$finite
  lower <- properties$lower
  upper <- properties$upper
  lower_finite <- !is.null(lower) && is.finite(lower)
  upper_finite <- !is.null(upper) && is.finite(upper)
  if (lower_finite && upper_finite) {
    rules <- paste0(rules, "[", lower, ",", upper, "]")
  } else if (lower_finite && !upper_finite) {
    rules <- paste0(rules, "[", lower, if (isTRUE(finite)) ",)" else ",]")
  } else if (!lower_finite && upper_finite) {
    rules <- paste0(rules, if (isTRUE(finite)) "(," else "[,", upper, "]")
  } else if (isTRUE(finite)) {
    rules <- paste0(rules, "[]")
  }

  # If `rules` is a vector, the object must meet any one of the rules
  if (isTRUE(properties$null)) rules <- c("0", rules)

  expr_and(rlang::expr(checkmate::qtest(!!x_expr, !!rules)), extra_len_test)
}

#' Make an expression to test an object `x` using a variable `checkmate::test_*`
#'
#' @details
#'
#' This function generates a variable test expression, such as that used in an
#' type check, e.g. `checkmate::test_integer(x, len = len, any.missing = nas)`.
#'  See [make_checkmate_fixed_properties_test()] to generate a fixed test
#' expression, such as that used in aliases.
#'
#' @param type_class `[character(1)]`
#'
#' A built-in object class, as a string, e.g. `"logical"`, `"environment"`. The
#' returned call tests whether an object `x` is this class.
#'
#' @param property_nms `[character]`
#'
#' A vector of {specifyr} property names to test, e.g. `c("len", "nas")` (says
#' to test the length of `x` and whether `x` contains `NA` values).
#'
#' @return
#'
#' An unevaluated `checkmate::test_*()` call to test an object `x`, given some
#' properties, e.g. `checkmate::test_integer(x, len = len, any.missing = nas)`.
#' @noRd
make_checkmate_variable_properties_test <- function(
    type_class,
    property_nms,
    x_expr = rlang::sym("x")
  ) {

  rlang::call2(
    .fn = glue::glue("test_{type_class}"),
    x = x_expr,
    !!!rlang::set_names(
      x = rlang::syms(property_nms),
      nm = property_names_to_checkmate_args(property_nms)
    ),
    .ns = "checkmate"
  )
}

#' Translate {specifyr} properties to {checkmate} test arguments
#'
#' @param property_nms `[character]`
#'
#' A character vector of {specifyr} properties, e.g. `c("len", "nas", "null")`.
#'
#' @return
#'
#' A character vector of {checkmate} test argument names. For example, the
#' properties `c("len", "nas", "null")` are translated to the argument names
#' `c("len", "any.missing", "null.ok")`.
#' @noRd
property_names_to_checkmate_args <- function(property_nms) {
  c(
    len = "len",
    lst_len = "len",
    nas = "any.missing",
    null = "null.ok",
    lst_null = "null.ok",
    min_len = "min.len",
    lst_min_len = "min.len",
    max_len = "max.len",
    lst_max_len = "max.len",
    lower = "lower",
    upper = "upper",
    finite = "finite"
  )[property_nms]
}


# stops ------------------------------------------------------------------------

#' Make a call to `stop_vector_mistyped()` with variable arguments
#'
#' @details
#'
#' This function generates an unevaluated call to `stop_vector_mistyped()` with
#' variable arguments, such as that which would appear within a type check. For
#' example, the correct stop for the `lgl()` type check is:
#'
#' \preformatted {
#'  specifyr::stop_vector_mistyped(
#'    x = x,
#'    x_name = x_name,
#'    type_test = is.logical(x),
#'    type_desc = c("a", "logical"),
#'    len = len,
#'    min_len = min_len,
#'    max_len = max_len,
#'    nas = nas,
#'    null = null,
#'    error_header = error_header,
#'    error_bullets = error_bullets,
#'    error_call = error_call,
#'    error_class = error_class
#'  )
#' }
#'
#' The above call could be generated via:
#'
#' \preformatted {
#'  make_variable_stop_vector_mistyped(
#'    type_desc = c("a", "logical"),
#'    type_test = quote(is.logical(x)),
#'    property_nms = c("len", "min_len", "max_len", "nas", "null")
#'  )
#' }
#'
#' @param type_desc `[character]`
#'
#' A description of the mistyped object `x`, e.g. `c("an", "integer")`, `"raw"`.
#'
#' @param type_test `[call]`
#'
#' An unevaluated call to test the type of `x`, e.g. `quote(is.numeric(x))`.
#'
#' @param property_nms `[character]`
#'
#' A vector of {specifyr} property names, e.g. `c("len", "nas")`.
#'
#' @return
#'
#' An unevaluated call to `specifyr::stop_vector_mistyped()` with variable
#' arguments.
#' @noRd
make_variable_stop_vector_mistyped <- function(
    type_desc,
    type_test,
    property_nms,
    x_expr = rlang::sym("x"),
    x_name = rlang::sym("x_name"),
    x_must = rlang::sym("x_must"),
    error_header = rlang::sym("error_header"),
    error_bullets = rlang::sym("error_bullets"),
    error_call = rlang::sym("error_call"),
    error_class = rlang::sym("error_class")
  ) {

  rlang::call2(
    .fn = "stop_vector_mistyped",
    x = x_expr,
    type_desc = type_desc,
    type_test = type_test,
    !!!rlang::set_names(rlang::syms(property_nms), property_nms),
    x_name = x_name,
    error_header = error_header,
    error_bullets = error_bullets,
    error_call = error_call,
    error_class = error_class,
    .ns = "specifyr"
  )
}

#' Make a call to `stop_vector_mistyped()` with fixed arguments
#'
#' @details
#'
#' This function generates an unevaluated call to `stop_vector_mistyped()` with
#' fixed arguments, such as that which would appear within an alias. For example,
#' the correct stop for the `bool` alias is:
#'
#' \preformatted {
#'  specifyr::stop_vector_mistyped(
#'    x = x,
#'    x_name = x_name,
#'    x_must = "be the value TRUE or FALSE",
#'    type_test = is.logical(x),
#'    type_desc = c("a", "boolean"),
#'    len = 1L,
#'    nas = FALSE,
#'    null = FALSE,
#'    error_header = error_header,
#'    error_bullets = error_bullets,
#'    error_call = error_call,
#'    error_class = error_class
#'  )
#' }
#'
#' The above call could be generated via:
#'
#' \preformatted {
#'  make_fixed_stop_vector_mistyped(
#'    type_desc = c("a", "boolean"),
#'    type_test = quote(is.logical(x)),
#'    properties = list(len = 1L, nas = FALSE, null = FALSE),
#'    x_must = "be the value TRUE or FALSE"
#'  )
#' }
#'
#' @param type_desc `[character]`
#'
#' A description of the mistyped object `x`, e.g. `c("an", "integer")`, `"raw"`.
#'
#' @param type_test `[call]`
#'
#' An unevaluated call to test the type of `x`, e.g. `quote(is.numeric(x))`.
#'
#' @param properties `[list]`
#'
#' A list of property, expected value pairs, e.g. `list(nas = FALSE, len = 1L)`.
#'
#' @return
#'
#' An unevaluated call to `specifyr::stop_vector_mistyped()` with fixed
#' arguments.
#' @noRd
make_fixed_stop_vector_mistyped <- function(
    type_desc,
    type_test,
    properties,
    x_expr = rlang::sym("x"),
    x_name = rlang::sym("x_name"),
    x_must = rlang::sym("x_must"),
    error_header = rlang::sym("error_header"),
    error_bullets = rlang::sym("error_bullets"),
    error_call = rlang::sym("error_call"),
    error_class = rlang::sym("error_class")
  ) {

  rlang::call2(
    .fn = "stop_vector_mistyped",
    x = x_expr,
    type_desc = type_desc,
    type_test = type_test,
    !!!properties,
    x_name = x_name,
    error_header = error_header,
    error_bullets = error_bullets,
    error_call = error_call,
    error_class = error_class,
    .ns = "specifyr"
  )
}

# misc -------------------------------------------------------------------------

make_class_to_type_test <- function(type_class) {
  switch(
    type_class,
    integer = quote(is.integer(x)),
    numeric = quote(is.numeric(x)),
    double = quote(is.double(x)),
    complex = quote(is.complex(x)),
    logical = quote(is.logical(x)),
    character = quote(is.character(x)),
    raw = quote(is.raw(x)),
    factor = quote(is.factor(x)),
    list = quote(is.list(x)),
    posixct = quote(inherits(x, "POSIXct")),
    date = quote(inherits(x, "Date")),
    environment = quote(is.environment(x)),
    data.frame = quote(is.data.frame(x)),
    `function` = quote(is.function(x)),
    rlang::expr(inherits(x, !!type_class))
  )
}

# convert ----------------------------------------------------------------------

#' @export
as_test <- function(x, ...) {
  UseMethod("as_test")
}

#' @export
as_stop <- function(x, ...) {
  UseMethod("as_stop")
}

# helpers ----------------------------------------------------------------------

expr_squash <- function(..., .compact = TRUE) {
  dots <- if (.compact) purrr::compact(rlang::list2(...)) else rlang::list2(...)
  bquote({ ..(dots) }, splice = TRUE)
}

expr_or <- function(..., .double = TRUE, .compact = TRUE) {
  dots <- if (.compact) purrr::compact(rlang::list2(...)) else rlang::list2(...)
  fn <- if (.double) \(x, y) rlang::expr(!!x || !!y) else \(x, y) rlang::expr(!!x | !!y)
  Reduce(fn, dots)
}

expr_and <- function(..., .double = TRUE, .compact = TRUE) {
  dots <- if (.compact) purrr::compact(rlang::list2(...)) else rlang::list2(...)
  fn <- if (.double) \(x, y) rlang::expr(!!x && !!y) else \(x, y) rlang::expr(!!x & !!y)
  Reduce(fn, dots)
}

# This function receives an expression in terms of a symbol (e.g. `mean(old_sym)`)
# and returns an expression in terms of a new symbol (e.g. `mean(new_sym)`).
# Don't change function arguments or names. I.e. `old_sym()` and `mean(old_sym = 10)`
# shouldn't be effected.

# There are 2 different ways to do this:
# https://stackoverflow.com/questions/26638746/how-can-i-do-partial-substitution-in-r?rq=3
# - taking example from MrFlick
# - you could also use `substitute(substitute(` from Josh O'Brien
#
# Read on the R Language Definition for more details:
# - https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Substitutions
replace_symbols <- function(expr, ...) {
  replacements <- rlang::list2(...)
  if (!rlang::is_named2(replacements)) {
    cli::cli_abort("`...` must be named", .internal = TRUE)
  } else if (!all(vapply(replacements, rlang::is_symbol, logical(1L)))) {
    cli::cli_abort("`...` must be symbols", .internal = TRUE)
  }
  do.call("substitute", list(expr, replacements))
}

replace_symbols2 <- function(expr, ...) {
  replacements <- rlang::list2(...)
  if (!rlang::is_named2(replacements)) {
    cli::cli_abort("`...` must be named", .internal = TRUE)
  }
  # See the R Language Definition for more details on why this works:
  # - https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Substitutions
  #
  # TLDR:
  # If I provide the argument `expr = quote(x > 10)` then `substitute(expr)`
  # will give me back `quote(x > 10)`, because substitute is looking for the expression
  # component of the promise `expr` (recall that function arguments are promises).
  # If I wanted to get back `x > 10` I'd have to actually provide `expr = x > 10`,
  # which I can't do if `expr` was defined earlier.
  #
  # So! Instead I substitute `expr` INTO my inner substitute call. That way, the
  # expression `quote(x > 10)` is *evaluated* by the outer `substitute()` and put
  # into the inner `substitute()` in place of `inner_expr`. This way, my call
  # looks like `eval(substitute(x > 10, replacements))`, which is exactly what I
  # want.
  eval(substitute(substitute(inner_expr, replacements), list(inner_expr = expr)))
}

# AH! Here in lies the problem {dplyr} is solving with `node_walk_replace()`.
# `substitute()` doesn't know about functions, so does weird shit like this.
replace_symbols(quote(function(x) x > 10), x = quote(y))
replace_symbols2(quote(function(x) x > 10), x = quote(y))

# Solution to the problem above ------------------------------------------------

replace_symbol <- function(expr, old, new) {
  stopifnot(
    rlang::is_symbol(old),
    rlang::is_expression(new)
  )
  switch_expr(
    expr,

    # Base Cases:
    constant = expr,
    symbol = if (identical(expr, old)) new else expr,

    # TODO: If we wanted `replace_symbols`, we'd just need to change the symbol
    # path to find WHICH symbol `expr` was identical to, then replace with the
    # corresponding `new`.

    # Recursive Cases:
    pairlist = as.pairlist(lapply(expr, replace_symbol, old = old, new = new)),
    call = {
      if (!rlang::is_call(expr, "function")) {
        # Replace any symbols within the call arguments
        expr[-1] <- replace_symbol(as.pairlist(as.list(expr[-1])), old, new)
        expr
      } else {
        # A call to `function` has 3 components:
        # - [[1]] the symbol of the function itself (i.e. `function`)
        # - [[2]] a pairlist of the formals, `function(< these >)` (NULL if no formals!)
        # - [[3]] the function body as an expression, `function() { <this> }`
        #
        # We don't want to replace anything in the body, but we do want to
        # replace symbols in the formals (e.g. `\(arg = x) { arg }`, we want
        # to replace the default of `x` in `arg = x` if `old = quote(x)`).
        fmls <- expr[[2]]
        if (!is.null(fmls)) {
          expr[[2]] <- replace_symbol(expr[[2]], old, new)
        }
        expr
      }
    }
  )
}

# Copied from Advanced R: https://adv-r.hadley.nz/expressions.html#ast-funs
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

# Copied from Advanced R: https://adv-r.hadley.nz/expressions.html#ast-funs
switch_expr <- function(x, ...) {
  switch(
    expr_type(x),
    ...,
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

# Interactive Testing
if (FALSE) {
  # Normal usage works as expected
  replace_symbol(quote(x + 10), quote(x), quote(y))
  replace_symbol(quote(my_list$x), quote(x), quote(y))
  replace_symbol(quote(my_list[["x"]]), quote(x), quote(y))
  replace_symbol(quote(x + 10 + z), quote(z), quote(x[[1]]))
  replace_symbol(quote(sum(x, x, x) + x), quote(x), quote(y))

  # Function formal defaults are replaced, functions arguments are not
  expr <- quote(function(x = x) x + 10)
  replace_symbol(expr, quote(x), quote((y * 100)))

  # Case with no function formals
  replace_symbol(quote(x + function() 10), quote(x), quote(y))

  # Call name is not replaced, but other symbols are
  expr <- quote(f(10) - f)
  replace_symbol(expr, quote(f), quote(x))
}
