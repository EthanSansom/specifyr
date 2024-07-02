
#> function(x, x_name, ...)
# The top level will proceed like normal. We always check the outer list.
#> if (!qtest(x, "l")) {
#>   stop_vector_mistyped(x, x_name, "list")
#> }

# Nested recursive functions will always use a LOOP for the nested checks
# (except maybe in some cases that we can optimize). We declare a new inner
# `x_{n}`, where `n` is the depth, number of levels we've recursed.

# The body of the nested function looks basically the same. We just have
# to adjust which `x` and `x_name` are used in the error message to reflect
# the current index.
#> for (i_1 in seq_along(x)) {
#>    x_1 <- x[[i_1]]
#>    if (!qtest(x_1, "l")) {
#>      x <- x_1
#>      x_name <- paste0(x_name, "[[", i_1, "]]")
#>      stop_vector_mistyped(x, x_name, "list")
#>    }
#>    if (!all(result <- vapply(X = x_1, FUN = qtest, FUN.VALUE = logical(1L), rules = "I2")) {
#>       first_error <- if (all(is.na(result)) 1 else which.min(result)
#>       x <- x_1[[first_error]]
#>       x_name <- paste0(x_name, "[[", i_1, "]]", "[[", first_error, "]]")
#>       stop_vector_mistyped(x, x_name, "integer", len = 2L, nas = FALSE)
#>    }
#> }
# We finally can return the result after all of the checks are complete.
#> return(x)

a_lst <- function(
    ...,
    .len = NULL,
    .null = FALSE,
    .min_len = NULL,
    .max_len = NULL,
    .strict_names = FALSE,
    .checks = list(),
    .error_class = "specifyr_error_mistyped",
    .alias_is = c("check", "test"),
    .alias_env = rlang::caller_env()
) {

  len <- .len %!|% count(.len)
  null <- bool(.null)
  min_len <- .min_len %!|% count(.min_len)
  max_len <- .max_len %!|% count(.max_len)
  strict_names <- bool(.strict_names)
  checks <- check_is_list_of_check(.checks)
  error_class <- chr(.error_class, nas = FALSE)
  alias_is <- rlang::arg_match(.alias_is)
  alias_env <- vir(.alias_env)

  n_aliases <- rlang::dots_n(...)
  if (n_aliases > 0) {
    aliases <- check_is_list_of_alias(rlang::enquos(...))
    is_recycled <- is_recycled_alias(aliases)
  } else {
    # TODO: Here, you'd want to launch into a different construction of just a list type
    aliases <- list()
    is_recycled <- FALSE
  }

  # TODO: Add a checkmate test to check names, a make_name_test function
  element_nms <- rlang::names2(aliases)

  # `if (is_missing(x)) { stop }`
  stop_if_list_missing <- make_stop_if_missing()

  # A test `checkmate::qtest(x, ...)`
  test_list_type <- make_qtest_test(
    cls = "list",
    len = len,
    null = null,
    min_len = min_len,
    max_len = max_len
  )

  # An error `stop(x, x_name, ...)`
  stop_list_mistyped <- rlang::call2(
    .fn = "stop_vector_mistyped",
    x = quote(x),
    x_name = quote(x_name),
    len = len,
    null = null,
    min_len = min_len,
    max_len = max_len,
    type_test = make_class_to_type_test("list"),
    type_desc = c("a", "list"),
    error_call = quote(error_call),
    error_class = quote(error_class)
  )

  # TODO: Make this a function.
  check_tests <- lapply(checks, as_test)
  check_stops <- lapply(checks, as_stop)

  # A list of `if (!check_test) { stop }`
  stop_if_checks_fail <- .mapply(
    \(check_test, check_stop) rlang::expr(if (!(!!test)) { !!stop }),
    dots = list(
      test = check_tests,
      stop = check_stops
    ),
    MoreArgs = list()
  )
  # If last `stop_if_check_fail` expression is reached, the last check must have failed
  stop_if_checks_fail[[length(checks)]] <- check_stops[[length(checks)]]

  # If no list elements were supplied to `...`, we need only check the list
  if (n_aliases == 0) {
    test <- expr_and(test_list_type, !!!check_tests)
    body <- expr_squash(
      # if (test_list_type && check_tests) { return(x) }
      make_test_return(test),
      # if (!test_list_type) { stop(x, x_name, ...) }
      make_not_test_stop(test_list_type, stop_list_mistyped),
      # if (!check_test_n) { stop(x, x_name, ...) }
      !!!stop_if_checks_fail
    )

    type_alias <- rlang::new_function(
      args = rlang::pairlist2(
        x = ,
        x_name = quote(rlang::caller_arg(x)),
        error_call = quote(rlang::caller_env()),
        error_class = .error_class
      ),
      body = body,
      env = alias_env
    )
    class(type_alias) <- c("specifyr_type_alias", "specifyr_type_check", "function")
    return(type_alias)
  }

  stop_if_aliases_fail <- vector("list", n_aliases)
  for (i in seq_along(aliases)) {

    # NON-RECURSIVE CASE
    #> if (!test(x[[i]])) {
    #>   # This means we refer to the correct index of a named element in the error message
    #>   x_indices <- vector_indices(x)
    #>   stop(x = x[[i]], x_name = indexed_name(x_name, x_indices[[i]]))
    #> }
    #
    # TODO: As with the recursive case, we'll want to abstract this out to a function
    #       which handles any non-recursive element.
    #
    x_expr <- rlang::expr(x[[!!i]])
    stop_if_aliases_fail[[i]] <- make_not_test_stop(
      # TODO: Test, if possible, will need to include the check tests as well,
      #       so it'll look like `test_type && check_test1 && ... && check_testn`
      test = as_test(aliases[[i]]),
      # TODO: Stops will need to include stops from attached `checks` as well,
      #       we'll need to do `if (!type_test) stop(); if (!check_test_i) stop()`
      stop = as_stop(aliases[[i]]),
      x_expr = rlang::expr(x[[!!i]]),
      x_name_expr = rlang::expr(index_name(x_name, vector_indices(x)[[!!i]]))
    )

    ## RECURSIVE CASE
    #
    # TODO: We'll actually need to abstract this whole function away from lists
    #       and into recursive specifications more generally, so that we can call
    #       it here.
  }

  # Remaining Structure
  # 1. Make initial list checks
  # - stop_if_missing (check for missing argument)
  # - stop_if_list_misptyped (check the qualties of the list)
  # - stop_if_wrong_names (check the lists names)
  #
  # 2. Make element checks
  # - non-recursive elements get a single `if (!test) stop` statement
  # - recursive elements will have checks generated via `for (x_1 in x[[n]]) if (!test) stop` statement
  #   - we want to be able to call some function such that nested recursive elements deal with themselves
  # - if `is_recycled`, we want to deal with the last check differently
  #   - if last check is non-recusive, we use
  #     `if (all(result <- vapply(x[...], test, logical(1L)))) { return(x) }; x <- x[which.min(result)]; stop`
  #   - if last check is recursive, we use
  #     # Loop over the remaining (recycled elements of `x`)
  #     `for (i in seq_along(x[...])) {
  #       # Assing each element to `x_1` (recall, these elements are themselves recursive)
  #       x_1 <- x[[i]];
  #       # Do all of the inner recursive tests
  #       for (i_1 in seq_along(x_1) { ... }`
  #     }
  #     # If we pass this point, then `x` is good
  #     return(x)
  #
  # 3. Make check checks for the list, this is the same as in `a_int` in the `vector-alias`
  #   - abstract the addition of checks to a function
  #   - NOTE: if there are any checks, we can't return `x` until they're complete
  #     - I.e. in the previous type checks, DON'T return `x` early on a pass

  # Functions to help:
  # - make_not_test_stop(test, stop, x_expr = quote(x), x_name_expr = quote(x_name))
  #   - makes expression `if (!test) stop` in terms of a `x` (`x_expr`) and `x_name` (`x_name_expr`)
  #
  # - make_test_return(test, returns, x_expr = quote(x))
  #   - makes expression `if (test) return(returns)` in terms of a `x` (`x_expr`)
  #
  # - special versions of the above for a final recycled element of `x`
  #   - make_recycled_test_stop
  #   - make_recycled_test_return
  #
  # - make_check_stop(test, error_args, x_expr = quote(x), x_name_expr = quote(x_name))
  #   - make a `if (!test) stop` expression in terms of `x`, `x_name`, for a `specifyr_check`
  #
  # - indexed_name(x_name, x_index)
  #   - x_index is a list of count (interish) and strings
  #   - `indexed_name("x", list("employees", 3)` -> "x[["employees"]][[3]]"
  #   - use `encodeString` for the character version
  #   - make sure the `x_name` emitted on error keeps track of the index properly

}
class(a_lst) <- c("specifyr_type_aliaser", "function")


is_builtin_blueprint <- function(x) {
  inherits(x, "specifyr_builtin_alias_blueprint")
}
