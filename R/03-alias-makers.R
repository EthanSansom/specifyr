# non-recursive element --------------------------------------------------------

# TODO Implement:
# - make_custom_alias_test
# - make_custom_type_stop
# - make_builtin_alias_test

make_non_rec_element_check <- function(blueprint, index, level = 0L) {

  if (is_builtin_blueprint(blueprint)) {
    # Expression like `is.Date(x) && length(x) == 1L && ...`
    type_test <- make_class_to_type_test(blueprint$type_class)
    type_alias_test <- make_builtin_alias_test(blueprint$type_class, blueprint$type_args)

    # Expression like `stop(x, x_name, ....)`
    stop_mistyped <- make_builtin_type_stop(
      type_test = type_test,
      type_desc = blueprint$type_desc,
      type_args = blueprint$type_args
    )
  } else {
    type_test <- blueprint$type_test
    type_alias_test <- make_custom_alias_test(type_test, blueprint$type_args)
    stop_mistyped <- make_custom_type_stop(
      type_test = type_test,
      type_desc = blueprint$type_desc,
      type_args = blueprint$type_args
    )
  }

  # Something like `is.Date(x[[1]]) && length(x[[1]]) == 1L && ...`
  type_alias_test <- replace_symbols(
    expr = type_alias_test,
    x = make_x_symbol(index, level)
  )

  # `if (!type_alias_test) { stop(x, x_name, ...) }`
  stop_if_mistyped <- make_not_test_stop(
    test = type_alias_test,
    stop = mistyped_stop
  )

  # `stop_if_checks_failed[[i]]` is `if (!check_tests[[i]]) { stop(x, x_name, ...) }`
  check_tests <- lapply(blueprint$checks, make_check_test)
  check_stops <- lapply(blueprint$checks, make_check_stop)
  stop_if_checks_failed <- .mapply(
    FUN = make_stop_not_test,
    dots = list(
      stop = check_stops,
      test = check_tests,
    ),
    MoreArgs = list()
  )

  # `is.Date(x[[1]]) && length(x[[1]]) == 1L && ... && test_past_epoch(x) && ...`
  test <- expr_and(type_alias_test, !!!check_tests)

  # `if (!type_alias_test) { stop(x, x_name) }; if (!check_test_1) { stop(x, x_name) }; ...`
  stops <- expr_squash(stop_mistyped, !!!check_stops)

  rlang::expr(
    if (!(!!test)) {
      !!make_test_failed_preamble(index = index, level = level)
      !!stops
      specifyr::stop_malformed_alias()
    }
  )
}

# TODO: This is the case where the non-recursive element is recycled (i.e. in a
# list of case). It will look something like:
#
# if (!all(result <- vapply(x[indices], test, logical(1L)))) {
#  first_error <- which.min(result)
#  x <- x[[first_error]]
#  x_name <- paste0(x_name, first_error)
#  stop(x, x_name)
# }
make_non_rec_recycled_check <- function(blueprint, indicies, level = 0L) {

}

# recursive element ------------------------------------------------------------

# TODO: This is the case where we're checking a recursive element WITHIN a recursive
# element. Like when `x = list(list(list(...), ...), ...)`.

make_rec_element_check <- function(blueprint, index, level = 0L) {

}

make_rec_recycled_check <- function(blueprint, indicies, level = 0L) {

}

# factories --------------------------------------------------------------------

make_x_symbol <- function(index = NULL, level = 0L) {
  if (level > 0L) {
    x_sym <- rlang::sym(paste0("x_", level))
    if (is.null(index)) x_sym else rlang::call2("[[", x_sym, index)
  } else {
    if (is.null(index)) rlang::sym("x") else rlang::expr(x[[!!index]])
  }
}

make_test_failed_preamble <- function(index, level) {
  # List of symbols `x, x_1, x_2, ...`, always contains at least `x`
  nested_x_symbols <- lapply(
    X = seq(from = 0L, to = level, by = 1L),
    \(level) make_x_symbol(index = NULL, level = level)
  )
  obj_inds_call <- rlang::call2("obj_inds", !!!nested_x_symbols) # TODO: Namespace `specifyr`

  # This is the object that is currently being tested. If an index is provided,
  # we're testing one if it's elements (e.g. `x[[1]]`), otherwise we're testing
  # the entire object.
  current_x_sym <- nested_x_symbols[[level + 1]]
  if (!is.null(index)) current_x_sym <- rlang::call2("[[", current_x_sym, index)

  # List of expression `inds[[1]][[i_1]], ..., inds[[level - 1]][[i_{level - 1}]],
  # empty if we're at level 0.
  index_calls <- lapply(
    X = sequence(level),
    \(level) str2lang(sprintf("inds[[%1$i]][[i_%1$i]]", level))
  )
  # There's one extra index if we're currently testing an element of the object
  if (!is.null(index)) {
    last_index_call <- str2lang(sprintf("inds[[%i]][[%i]]", level + 1, index))
    index_calls <- append(index_calls, last_index_call)
  }
  idx_name_call <- rlang::call2("idx_name", rlang::sym("x_name"), !!!index_calls) # TODO: Namespace `specifyr`

  # TODO: If you remove the `{}`, R get's angry, but it makes its way into the
  #       final expression. Is there a way to avoid this?
  rlang::expr({
    inds <- !!obj_inds_call
    x <- !!current_x_sym
    x_name <- !!idx_name_call
  })
}

obj_inds <- function(...) {
  lapply(rlang::list2(...), obj_inds1)
}

obj_inds1 <- function(x) {
  x_nms <- rlang::names2(x)
  out <- as.list(x_nms)
  out[x_nms == ""] <- which(x_nms == "")
  out
}

idx_name <- function(x_name, ...) {
  paste0(x_name, index_str(...))
}

index_str <- function(...) {
  dots <- rlang::list2(...)
  dots <- purrr::map_if(dots, is.character, \(x) encodeString(x, quote = '"'))
  paste0("[[", dots, "]]", collapse = "")
}

# helpers ----------------------------------------------------------------------

is_recycled_alias <- function(
    x,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_input"
) {

  x_len <- length(x)
  # `check_is_list_of_alias` returns TRUE wherever the `..etc` symbol is used
  is_etc <- vapply(x, isTRUE, logical(1L))
  if (sum(is_etc[x_len]) > 0) {
    cli::cli_abort(
      c(
        "The `..etc` symbol may only be supplied as the last `...`.",
        x = "`..etc` was supplied {at_loc_friendly(is_etc)}.",
        i = "{x_len} arguments were supplied to `...`."
      ),
      call = error_call,
      error_class = c(error_class, "specifyr_error")
    )
  }
  isTRUE(is_etc[[x_len]])
}


is_alias <- function(x) {
  inherits(x, "specifyr_alias")
}

# Interactive Testing ----------------------------------------------------------

if (FALSE) {

  # Assign intermediate results of the check tests. I think, because of lazy eval,
  # this won't really incur any kind of penalty.
  f_assign <- function(x) {

    if (
      (type_res <- (is.numeric(x))) &&
      (length_res <- (length(x) == 1000L)) &&
      # Whoops, I forgot that if `x` fails either of the previous checks, we'd
      # actually never reach this pre-assignment making the `bullets <-` step invalid
      (nas_res <- (!anyNA(x)))
    ) {
      return(x)
    }

    bullets <- c(
      if (!isTRUE(type_res)) "`x` is wrong type",
      if (!isTRUE(length_res)) "`x` is bad length",
      if (!isTRUE(nas_res)) "`x` is bad NAs"
    )
    cli::cli_abort(bullets)

  }

  # Don't assign intermediate values
  f_no_assign <- function(x) {

    if (
      is.numeric(x) &&
      (length(x) == 1000L) &&
      !anyNA(x)
    ) {
      return(x)
    }

    bullets <- c(
      if (!isTRUE(is.numeric(x))) "`x` is wrong type",
      if (!isTRUE(length(x) == 1000L)) "`x` is bad length",
      if (!isTRUE(!anyNA(x))) "`x` is bad NAs"
    )
    cli::cli_abort(bullets)

  }

  # Assigning is slightly slower, but only by ~100 nanoseconds. I think if we
  # expect every assignment to only be a length-1 logical anyways, then we should
  # be fine
  x <- 1:1000
  bench::mark(
    f_assign(x),
    f_no_assign(x)
  )

  x <- 1:999
  bench::mark(
    try(f_assign(x)),
    try(f_no_assign(x)),
    check = FALSE
  )
}

