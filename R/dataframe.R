# TODO:
# - create an `alias()`, which is a partially applied version of a `check()`,
#   think `purrr::partial`, where only the only un-supplied argument is the
#   object to check (i.e. `x`)
#   - `alias()` is a bad name for this, think of something better
#
# - make a function like `check_dataframe(...)` which takes name of columns
#   and their associated checks: `check_dataframe(x = int, y = num)`.

# IDEAS:
#
# [1] `info()` objects preserve all of their arguments - `pass`, `fail`, `header`,
# etc. Once the `info()` object is inserted into a `check()`, then we can strip
# away all of the information which isn't required via `compress_info()`, which
# reduces an `info()` function body to it's required components only.
#
# [2] A new pattern for accepting expressions.
#
# A lot of arguments "could" accept an expression of some {specifyr} object. In
# these cases we do the following.
maybe_test <- function(expr) {
  quoted <- rlang::quo(expr)
  # If we've quoted a test, all is well and we can return the test
  test <- try(rlang::eval_tidy(expr))
  if (is_test(test)) {
    return(test)
  }
  # Otherwise, we assume the user want's the expression to *be* a test
  test(expr)
}
#
# [3] How `and()` and friends should work. Ideally, we want to allow the user
# to do as little boiler plate as possible, literally "what I test" and "what I
# say on error" should be the only things that are typed. The implementation will
# use [2].
#
# So, we allow for varying levels of flexibility. The below should all do the same:
# and(
#   info(test(is.numeric(x)), fail = "`x` must be numeric."),
#   info(test(length(x) == 1), fail = "`x` must be scalar.")
# )
# and(
#   info(is.numeric(x), fail = "`x` must be numeric."),
#   info(length(x) == 1, fail = "`x` must be scalar.")
# )
# and(
#   "`x` must be numeric." = test(is.numeric(x)),
#   "`x` must must be scalar." = test(length(x) == 1)
# )
# FLAG: I think this vvv example is the least permisible. But maybe...
# and(
#   "`x` must be numeric." = is.numeric(x),
#   "`x` must must be scalar." = length(x) == 1
# )
#
# [4] You could add a `returns` argument to `info()`, (and `.returns` to `and()`
# and `or()`), to specify what is returned (using an expression). This only really
# matters in `or()`, as you could do something like.
#
# or(
#   info(is.function(x), returns = x),
#   info(is_lhs_formula(x), returns = rlang::as_function(x))
# )
# if (t1 <- is.function(x) || t2 <- is_lhs_formula(x)) {
#   # Check which was the last tested `t*` (last defined), as this will be the
#   # success.
#   switch(last_or(t1, t2), t1 = x, t2 = rlang::as_function(x))
# }
#
# [5] In a call to `%typed% function(x = x_type(), y = y_type())` allow `||`
# and `&&` to be used to specify multiple type checks.
#
# mean2 <- %typed% function(x = int || num, na.rm = bool) {
#
# }
# function(x, na.rm) {
#   check_any(
#     int(x),
#     num(x)
#   )
#   bool(x)
# }
#
# [6] In the initial package release, create a bunch of pre-built `ask_is_*` functions
# using `info()`, `and()`, and `or()`, so that they can be combined together to make
# new user functions.
#
# [7] `test()` should be able to accept functions, we replace `x` as the first
# argument.
#
# [8] `as_test()` forwards the arguments of a function within a `test()`. Dots
# (`...`) are ignored (can't be used in a test), but users can provide `dots_args`
# (i.e. `arg1 = , arg2 = NULL`) that are forwarded appropriately.
#
# [9] {cli} theme `.is_len`, `.is_dim`, `.has_dim`, `.has_len`, shorthands for
# describing dimensions.
#
# In general, it would be nice for this package to define a family of `*_friendly`
# functions and {cli} shortcuts for these functions. Make your own `obj_type_friendly`,
# as well as `is_length_friendly`, `is_dim_friendly`, etc.

# Study purrr ------------------------------------------------------------------

# {purrr} adverbs: https://purrr.tidyverse.org/reference/index.html#adverbs
# These are the source of inspiration for function composition in {specifyr}.
# I don't want to formally depend on {purrr} for this, but I do want to use
# their approach.

# [1] The only thing that doesn't make sense here is `.Call(purrr_eval`, but I
# think that this is just calling the C-level `eval()` for speed.

purrr::compose

base::Vectorize

# Negate uses `compose()` under the hood
purrr::negate
base::Negate

purrr::insistently

match_fn <- function(fn) {
  match.fun(fn)
}

match_fn(mean)
match_fn("mean")
# match_fn(mean(x = 12))
# match_fn(quote(mean(x = 12)))


# `purrr::compose()` study:

purrr::compose

# This is the output function
function (x)
{
  # Overwrite `_fn` in the caller's environment, with the first function
  # that was composed.
  env <- env(caller_env(), `_fn` = first_fn)

  # Get the call that the user just made (i.e. the composed function).
  # Ex. `f <- purrr::compose(is.numeric, isTRUE); f("A")`. `first_call` is the
  # AST (language objects) with call `f` and arguments (pairlist node) x = "A".
  first_call <- sys.call()

  # Replace the function of the first call with `_fn`, the first composed function.
  # That here is `is.numeric`. This basically inserts all the arguments to `first_call`
  # into `_fn`.
  first_call[[1]] <- quote(`_fn`)

  # Evaluate the `first_call` (really `_fn(<args>)`) and save the output `_out`
  # to `env`.
  env$`_out` <- .Call(purrr_eval, first_call, env)

  # We supply the output `_out` to each subsequent composed function in `fns`. Here,
  # that would just be `isTRUE`. We quote a single call to `_fn(_out)` and then
  # change the binding to `_fn` and `_out` on each iteration.
  call <- quote(`_fn`(`_out`))
  for (fn in fns) {
    env$`_fn` <- fn
    env$`_out` <- .Call(purrr_eval, call, env)
  }

  # Return the last binding to `_out`
  env$`_out`
}

# NOTE: `.` prefixed objects aren't captured in function environments.
g <- function() {
  .hello <- 10
  goodbye <- 11
  function(x) {
    x
  }
}
as.list(environment(g()))

unattr <- function(x) {
  attributes(x) <- NULL
  x
}

purrr::negate(dplyr::across) |> unattr()


specify_named <- function(...) {

}

check(test(is.numeric(x)))

check_names <- function() {

}



