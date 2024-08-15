# Splice a list of expressions into a `{}` block (as though they were lines of code)
expr_squash <- function(..., .compact = TRUE) {
  dots <- if (.compact) purrr::compact(rlang::list2(...)) else rlang::list2(...)
  bquote({ ..(dots) }, splice = TRUE)
}

# Combine expressions in `...` with `||` or `|`
expr_or <- function(..., .double = TRUE, .compact = TRUE) {
  dots <- if (.compact) purrr::compact(rlang::list2(...)) else rlang::list2(...)
  fn <- if (.double) \(x, y) rlang::expr(!!x || !!y) else \(x, y) rlang::expr(!!x | !!y)
  Reduce(fn, dots)
}

# Combine expressions in `...` with `&&` or `&`
expr_and <- function(..., .double = TRUE, .compact = TRUE) {
  dots <- if (.compact) purrr::compact(rlang::list2(...)) else rlang::list2(...)
  fn <- if (.double) \(x, y) rlang::expr(!!x && !!y) else \(x, y) rlang::expr(!!x & !!y)
  Reduce(fn, dots)
}

# Return symbols in an expression which match a `pattern` or are in `syms`
find_syms <- function(expr, pattern, syms) {

  # TODO: Add some nicer error messages
  stopifnot(rlang::is_expression(expr))
  rlang::check_exclusive(pattern, syms, .require = FALSE)
  if (!missing(pattern)) stopifnot(rlang::is_string(pattern))
  if (!missing(syms)) stopifnot(is.character(syms), !anyNA(syms))

  # TODO: We may want to create an exception for in-line function calls
  # like `function(x) { x }`, because the symbol `x` doesn't actually appear
  # there.
  # - maybe we could alter `expr_type` to have an `inline_function` type,
  #   since it's a common exception. Perhaps an `assignment` type as well.

  # Add symbols from `expr` to `symbols`
  symbols <- list()
  find_syms0 <- function(expr) {
    switch(
      expr_type(expr),
      # Base cases
      constant = NULL,
      symbol = { symbols <<- append(symbols, expr)},
      # Recursive cases
      pairlist = { purrr::map(as.list(expr), find_syms0) },
      call = { purrr::map(as.list(expr[-1]), find_syms0) }
    )
  }
  find_syms0(expr)

  # Prune the bad symbols (if required)
  if (rlang::is_empty(symbols)) {
    list()
  } else if (!missing(pattern)) {
    symbol_names <- purrr::map_chr(symbols, rlang::as_string)
    symbols[grepl(x = symbol_names, pattern = pattern)]
  } else if (!missing(syms)) {
    symbol_names <- purrr::map_chr(symbols, rlang::as_string)
    symbols[symbol_names %in% syms]
  } else {
    symbols
  }
}

# Test whether `expr` contains any symbols which match a `pattern` or in `syms`.
contains_any_syms <- function(expr, pattern, syms) {
  length(find_syms(expr, pattern, syms)) > 0
}

# Return the type of an `expr` as a string
expr_type <- function(expr) {
  if (rlang::is_syntactic_literal(expr)) {
    "constant"
  } else if (rlang::is_symbol(expr)) {
    "symbol"
  } else if (rlang::is_pairlist(expr)) {
    "pairlist"
  } else if (rlang::is_call(expr)) {
    "call"
  } else {
    typeof(expr)
  }
}

# Replace symbols in `expr` with an expression. `...` are named expressions,
# the names are the symbols to switch.
replace_symbols <- function(expr, ...) {

  if (!rlang::is_expression(expr) && !rlang::is_call(expr)) {
    cli::cli_abort(
      "`expr` must be a call, expression, or symbol, not {.obj_type_friendly {expr}}."
    )
  }
  symbols_dict <- rlang::list2(...)
  stopifnot(
    "`...` must be named calls, expressions, or symbols." = all(purrr::map_lgl(symbols_dict, ~rlang::is_expression(.x) || rlang::is_call(.x))),
    "`...` must have unique names." = !vctrs::vec_duplicate_any(rlang::names2(symbols_dict))
  )

  replace_symbols0 <- function(expr) {
    switch(
      expr_type(expr),
      # Base cases
      constant = expr,
      symbol = if (rlang::as_string(expr) %in% names(symbols_dict)) {
        symbols_dict[[rlang::as_string(expr)]]
      } else {
        expr
      },

      # Recursive cases
      pairlist = as.pairlist(purrr::map(as.list(expr), replace_symbols0)),
      call = {
        # Don't replace symbols within an in-line function.
        # E.g. Given `function(x = a) { x*2 }` we might replace `a`, but we'd
        # ignore the `x` formal and the `x` in the body.
        if (identical(expr[[1]], quote(`function`))) {
          # `expr[[2]]` is a pairlist of the formals, we replace the defaults.
          expr[[2]] <- replace_symbols0(expr[[2]])
          expr
        } else {
          # Replace symbols in the call argument
          as.call(append(expr[[1]], purrr::map(as.list(expr[-1]), replace_symbols0)))
        }
      }
    )
  }
  replace_symbols0(expr)
}
