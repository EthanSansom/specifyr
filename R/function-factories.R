# cls --------------------------------------------------------------------------

# TODO Ethan: Think more about how you want to check classes of multiple objects.
# It's a little trickier than length!!

## `target_cls` can be what?
# - a string              -> `inherits(x, target_cls)` or `is.target_cls(x)`
# - a character           -> `rlang::inherits_all(x, target_cls)`
#
# TODO Ethan: A way to do `rlang::inherits_only` would be nice.
#
# TODO Ethan: For now, I've nixed this one. I'm not sure if it's in the spirit of
#             the package. Other than `integerish`, I'm not sure what functions
#             would need to accept an argument of multiple classes.
#
# - a list of characters  -> is target_cls[[1]] || ... || is target_cls[[n]]

cls_check_body <- function(x, error_index, target_cls) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  rlang::expr(
    if (!!cls_test_body(x, target_cls, negate = TRUE)) {
      specifyr::emit_cls_error(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class,
        cls = !!target_cls
      )
    }
  )
}

cls_check_body_multi <- function(x, x_indices, error_index, target_cls) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(x_indices, "is.integer", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  # If indices are provided, we want to check the object `x` only at the
  # specified `x_indices`.
  if (!rlang::is_empty(x_indices)) {
    x <- rlang::call2("[", x, x_indices)
  }

  rlang::expr(
    if (!!cls_test_body_multi(x, target_cls, negate = TRUE)) {
      specifyr::emit_cls_error_multi(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class,
        cls = !!target_cls
      )
    }
  )
}

cls_test_body <- function(x, target_cls, negate = FALSE) {
  call <- switch(
    target_cls_type_single(target_cls),
    # Ex. `is.numeric(c(1, 2, 3))`
    recognized = rlang::call2(cls_test_fn_name(target_cls), x),
    # Ex. `inherits(tibble(), "tbl")`
    string = rlang::expr(inherits(!!x, !!target_cls)),
    # Ex. `inherits_all(tibble(), c("tbl", "data.frame"))`
    character = rlang::expr(rlang::inherits_all(!!x, !!target_cls)),
    # Ex. `is.numeric(x) || is.integer(x) || inherits(x, "my_class")`
    list = {
      cls_tests <- lapply(target_cls, cls_test_body, x = x, negate = FALSE)
      expr_or(!!!cls_tests)
    }
  )
  if (negate) rlang::call2("!", call) else call
}

cls_test_body_multi <- function(x, target_cls, negate = FALSE) {
  call <- switch(
    target_cls_type_multi(target_cls),
    # Ex. `vapply(list(1, 2, 3), is.numeric, logical(1L)`
    recognized = {
      fn <- rlang::sym(cls_test_fn_name(target_cls))
      rlang::expr(all(vapply(!!x, !!fn, logical(1L))))
    },

    # Ex. `vapply(list(tibble(), tibble()), inherits, logical(1L), what = "tbl")`
    string = rlang::expr(all(vapply(!!x, inherits, logical(1L), what = !!target_cls))),

    # Ex. `mapply(inherits, list(1, "A", 3L), c("numeric", "character", "integer"))`
    character = rlang::expr(all(mapply(inherits, !!x, !!target_cls))),

    # Ex. `mapply(inherits_all, list(tibble(), TRUE), list(c("tbl", "data.frame"), "logical"))`
    list_of_character = rlang::expr(all(mapply(rlang::inherits_all, !!x, !!target_cls))),

    # If the target class is a list of lists, it contains only one inner list
    # (ex. `list(list("integer", "numeric"))`). If more options arise, might have
    # to make an `expected_cls` class to manage the options.
    #
    # Ex. `vapply(list(1, 10L), \(x) is.numeric(x) || is.integer(x), logical(1L))`
    list_of_list = {
      body <- cls_test_body(x, target_cls[[1]], negate = FALSE)
      rlang::expr(all(vapply(!!x, \(x) { !!body }, logical(1L))))
    }
  )
  if (negate) rlang::call2("!", call) else call
}

target_cls_type_single <- function(target_cls) {
  if (is_recognized_cls(target_cls)) {
    "recognized"
  } else if (rlang::is_string(target_cls)) {
    "string"
  } else if (is.character(target_cls)) {
    "character"
  } else if (is.list(target_cls)) {
    "list"
  } else {
    cli::cli_abort("Can't assign type to `target_cls`", .internal = TRUE)
  }
}

target_cls_type_multi <- function(target_cls) {
  if (is_recognized_cls(target_cls)) {
    "recognized"
  } else if (rlang::is_string(target_cls)) {
    "string"
  } else if (is.character(target_cls)) {
    "character"
  } else if (is.list(target_cls) && !is_empty(target_cls)) {
    if (is.list(target_cls[[1]])) {
      "list_of_list"
    } else {
      "list_of_character"
    }
  } else {
    cli::cli_abort("Can't assign type to `target_cls`", .internal = TRUE)
  }
}

cls_test_fn_name <- function(target_cls, use_ns = FALSE) {
  if (!rlang::is_string(target_cls)) {
    return(NULL)
  }
  switch(
    target_cls,
     logical = "is.logical",
     integer = "is.integer",
     numeric = "is.numeric",
     complex = "is.complex",
     character = "is.character",
     raw = "is.raw",
     factor = "is.factor",
     list = "is.list",
     environment = "is.environment",
     data.frame = "is.data.frame",
     `function` = "is.function",
     NULL
  )
}

is_recognized_cls <- function(target_cls) {
  !is.null(cls_test_fn_name(target_cls))
}

#' @export
emit_cls_error <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class,
    cls,
    caller = NULL
  ) {

  # TODO: Implement this in all of the `emit_*_error` functions. Make sure that
  # the `emit_*_error_multi` functions also pass their caller in. Probably change
  # this into a function, since it will be used everywhere. Maybe this is actually
  # the definition of `register_error_spec`.
  caller <- caller %||% rlang::caller_fn()
  if (is_object_spec(caller)) {
    register_spec_error(caller)
    error_spec_msg <- cli::col_grey("Run `{.run specifyr::spec_error()}` for details on {.arg {x_name}}.")
  } else {
    error_spec_msg <- NULL
  }

  indexed_name <- paste0(x_name, format_index(x_index))
  expected_class <- if (target_cls_type_single(cls) == "list") {
    oxford(paste0("{.cls {cls[[", seq_along(cls), "]]}}"), sep2 = " or ")
  } else {
    "{.cls {cls}}"
  }

  cli::cli_abort(
    c(
      paste0("{.arg {indexed_name}} must be class ", expected_class, "."),
      x = "{.arg {indexed_name}} is class {.cls {class(x)}}.",
      error_spec_msg
    ),
    call = error_call,
    class = error_class,
    x_name = x_name,
    x_index = x_index
  )
}

#' @export
emit_cls_error_multi <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class,
    cls
  ) {

  # We need to re-test the classes of `x`'s elements. Re-generating the class
  # check functions using `cls_test_body` ensures that we get the same results
  # which caused the original class error.
  cls_tests <- purrr::map(
    cls,
    \(cls) {
      rlang::new_function(
        args = pairlist2(x = ),
        body = cls_test_body(sym("x"), cls)
      )
    }
  )
  error_at <- which.min(purrr::map2_lgl(cls_tests, x, \(f, x) isTRUE(f(x))))
  # `as.numeric(error_at)` prevents integer index formatting (i.e. [[1]] not [[1L]])
  error_index <- append(x_index, as.numeric(error_at))
  emit_cls_error(
    x = x[[error_at]],
    x_name = x_name,
    x_index = error_index,
    error_call = error_call,
    error_class = error_class,
    cls = cls[[error_at]],
    caller = rlang::caller_fn()
  )
}

# Test `cls_check_body_multi`
if (FALSE) {
  # Test with a simple case
  target_cls <- list("integer", "character")
  f <- new_function(
    args = pairlist2(x = ),
    body = cls_check_body_multi(sym("x"), integer(), list(), target_cls)
  )

  # No Error
  x <- list(tibble::tibble(), 10L)
  f(x)

  # Error
  y <- list(12, 10L)
  f(y)

  # Test with a fancy case
  target_cls <- OlsonNames()
  x <- lapply(target_cls, \(cls) { x <- 0; class(x) <- cls; x })
  f <- new_function(
    args = pairlist2(x = ),
    body = cls_check_body_multi(sym("x"), integer(), list(), target_cls)
  )

  f(x)

  x[[100]] <- 10
  f(x)
}

## len -------------------------------------------------------------------------

# TODO Ethan: Make sure that `len` is controlled. Ex. Users must supply a lenght
# 1 or 2 integerish number. If length 2, then `len[[1]] < len[[2]]`. And, can't
# both be infinite. AND min length is 0 or more.

len_check_body <- function(x, error_index, target_len) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  rlang::expr(
    if (!!len_test_body(x, target_len, negate = TRUE)) {
      specifyr::emit_len_error(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class,
        len = !!target_len
      )
    }
  )
}

len_check_body_multi <- function(x, x_indices, error_index, target_len) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(x_indices, "is.integer", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  # If indices are provided, we want to check the object `x` only at the specified `x_indices`.
  if (!rlang::is_empty(x_indices)) {
    x <- rlang::call2("[", x, x_indices)
  }

  rlang::expr(
    if (!!len_test_body_multi(x, target_len, negate = TRUE)) {
      specifyr::emit_len_error_multi(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class,
        len = !!target_len
      )
    }
  )
}

len_test_body <- function(x, target_len, negate = FALSE) {
  switch(
    target_len_type_single(target_len),

    # Ex. `length("A") == 1`
    exact = if (negate) {
      rlang::expr(length(!!x) != !!target_len)
    } else {
      rlang::expr(length(!!x) == !!target_len)
    },

    # Ex. `3 <= length(1:5) && length(1:5) <= 10`
    range = {
      min_len <- target_len[[1]]
      max_len <- target_len[[2]]
      if (negate) {
        if (min_len == 0) {
          # If `min_len` is 0 you need only check the upper bound
          rlang::expr(length(!!x) > !!max_len)
        } else if (is.infinite(max_len)) {
          # If `max_len` is Inf, only check lower bound
          rlang::expr(length(!!x) < !!min_len)
        } else {
          rlang::expr(specifyr::not_between1(!!length(x), !!min_len, !!max_len))
        }
      } else {
        if (min_len == 0) {
          # If `min_len` is 0 you need only check the upper bound
          rlang::expr(length(!!x) <= !!max_len)
        } else if (is.infinite(max_len)) {
          # If `max_len` is Inf, only check lower bound
          rlang::expr(length(!!x) >= !!min_len)
        } else {
          rlang::expr(specifyr::between1(length(!!x), !!min_len, !!max_len))
        }
      }
    },

    # We don't care about the length
    any = if (negate) rlang::expr(FALSE) else rlang::expr(TRUE)
  )
}

len_test_body_multi <- function(x, target_lens, negate = FALSE) {
  x_lens <- rlang::expr(vapply(!!x, length, integer(1L)))
  switch(
    target_len_type_multi(target_lens),

    # Ex. `vapply(list(1, 1:2), length, integer(1L)) == c(1, 2)`
    exact = if (negate) {
      rlang::expr(any(!!x_lens != !!target_lens))
    } else {
      rlang::expr(all(!!x_lens == !!target_lens))
    },

    # Ex. `all(mapply(specifyr::between, x, left, right))`
    range = {
      # These could contain `Inf` or `-Inf`, so result is potentially numeric
      min_lens <- vapply(target_lens, \(x) x[[1]], numeric(1L))
      max_lens <- vapply(target_lens, \(x) x[[2]], numeric(1L))
      if (negate) {
        if (all(min_lens == 0)) {
          # If all `min_len` is 0 you need only check the upper bound
          rlang::expr(any(!!x_lens > !!max_lens))
        } else if (all(is.infinite(max_lens))) {
          # If all `max_len` is Inf, only check lower bound
          rlang::expr(any(!!x_lens < !!min_lens))
        } else {
          rlang::expr(any(specifyr::not_between(!!x_lens, !!min_lens, !!max_lens)))
        }
      } else {
        if (all(min_lens == 0)) {
          # If all `min_len` is 0 you need only check the upper bound
          rlang::expr(all(!!x_lens <= !!max_lens))
        } else if (all(is.infinite(max_lens))) {
          # If all `max_len` is Inf, only check lower bound
          rlang::expr(all(!!x_lens >= !!min_lens))
        } else {
          rlang::expr(all(specifyr::between(!!x_lens, !!min_lens, !!max_lens)))
        }
      }
    }
  )
}

target_len_type_single <- function(target_len) {
  target_len_n <- length(target_len)
  if (is.null(target_len)) {
    "any"
  } else if (target_len_n == 1) {
    "exact"
  } else if (target_len_n == 2) {
    "range"
  } else {
    cli::cli_abort("Can't assign type to `target_len`", .internal = TRUE)
  }
}

target_len_type_multi <- function(target_lens) {
  if (rlang::is_integerish(target_lens)) {
    "exact"
  } else if (is.list(target_lens)) {
    "range"
  } else {
    cli::cli_abort("Can't assign type to `target_len`", .internal = TRUE)
  }
}

#' @export
emit_len_error <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class,
    len
) {
  indexed_name <- paste0(x_name, format_index(x_index))
  expected_len <- switch(
    target_len_type_single(len),
    exact = "be length {len}",
    range = "have length in range [{len[[1]]}, {len[[2]]}]"
  )
  message <- paste0("{.arg {indexed_name}} must ", expected_len, ".")
  cli::cli_abort(
    c(
      message,
      x = "{.arg {indexed_name}} is length {length(x)}."
    ),
    call = error_call,
    class = error_class,
    x_name = x_name,
    x_index = x_index
  )
}

#' @export
emit_len_error_multi <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class,
    len
) {
  len_tests <- purrr::map(
    len,
    \(len) {
      rlang::new_function(
        args = pairlist2(x = ),
        body = len_test_body(sym("x"), len)
      )
    }
  )
  error_at <- which.min(purrr::map2_lgl(len_tests, x, \(f, x) isTRUE(f(x))))
  error_index <- append(x_index, as.numeric(error_at))
  emit_len_error(
    x = x[[error_at]],
    x_name = x_name,
    x_index = error_index,
    error_call = error_call,
    error_class = error_class,
    len = len[[error_at]]
  )
}

# Test `emit_len_error_multi`
if (FALSE) {
  emit_len_error_multi(
    x = list(1, 1:5, 2, numeric(0L)),
    x_name = "x",
    x_index = list(),
    error_call = caller_env(),
    error_class = "eerrr",
    len = list(c(0, 10), NULL, 1:2, c(1, 20))
  )
}

# Test `len_check_body_multi`
if (FALSE) {

  ## Exact Length

  # Test with a simple case
  target_len <- c(1L, 10L, 12L)
  f <- new_function(
    args = pairlist2(x = ),
    body = len_check_body_multi(sym("x"), integer(), list(), target_len)
  )

  # No Error
  x <- list(1, 1:10, 1:12)
  f(x)

  # Error
  y <- list(1, 1:5, "A")
  f(y)

  # Test with a fancy case for exact length
  target_lens <- 1:1000
  x <- lapply(target_lens, \(len) rep(0, len))
  f <- new_function(
    args = pairlist2(x = ),
    body = len_check_body_multi(sym("x"), integer(), list(), target_lens)
  )

  # Passes
  f(x)

  # Error
  x[[100]] <- 10
  f(x)

  ## Range Length

  # Test with a simple case
  target_len <- list(c(0, 10L), c(1L, 5L), c(1, Inf))
  f <- new_function(
    args = pairlist2(x = ),
    body = len_check_body_multi(sym("x"), integer(), list(), target_len)
  )

  # No Error
  x <- list(numeric(0L), 1:3, 1)
  f(x)

  # Error
  y <- list(1:20, 1:5, numeric(0L))
  f(y)

  # Test with a fancy case
  target_len <- mapply(c, 1:1000, 1:1000 + 10, SIMPLIFY = FALSE)
  x <- lapply(target_lens, \(len) rep(0, len[[1]] + 5))
  f <- new_function(
    args = pairlist2(x = ),
    body = len_check_body_multi(sym("x"), integer(), list(), target_len)
  )

  # No error
  f(x)

  # Error
  x[[10]] <- "A"
  f(x)
}

# Test scoping
if (FALSE) {

  # Check for length 10 object
  f <- new_function(
    args = pairlist2(x = ),
    body = len_check_body(sym("x"), list(), 10L)
  )

  # Works as expected
  f(1:10)
  f(1)

  # Someone overwrites a base function
  length <- function(x) 0

  # No longer works
  f(1:10)
  f(1)

  # Re-set `length`
  length <- base::length

  # What is we scoped it to the package environment?
  f <- new_function(
    args = pairlist2(x = ),
    body = len_check_body(sym("x"), list(), 10L),
    # `rlang` env, making sure `emit_len_error` is also available
    env = rlang::new_environment(
      list(emit_len_error = emit_len_error),
      parent = rlang::pkg_env("rlang")
    )
  )

  # Works
  f(1:10)
  f(1)

  # Still works!
  length <- function(x) 0
  f(1:10)
  f(1)

  length <- base::length
}

# nas --------------------------------------------------------------------------

# TODO Ethan:
# Don't allow users to specify an `NA` check if the expected length is exactly 0.

# We incorporate `target_len` so we can use `is.na` instead of `all(is.na())`
# on length 1 inputs and handle 0 length cases.
nas_check_body <- function(x, error_index, target_len = NULL) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  rlang::expr(
    if (!!nas_test_body(x, target_len, negate = FALSE)) {
      specifyr::emit_nas_error(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class
      )
    }
  )
}

nas_check_body_multi <- function(x, x_indices, error_index, target_lens = NULL) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(x_indices, "is.integer", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  # If indices are provided, we want to check the object `x` only at the specified `x_indices`.
  if (!rlang::is_empty(x_indices)) {
    x <- rlang::call2("[", x, x_indices)
  }

  rlang::expr(
    if (!!nas_test_body_multi(x, target_lens, negate = FALSE)) {
      specifyr::emit_nas_error_multi(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class
      )
    }
  )
}

# Test whether any element of `x` is NA
nas_test_body <- function(x, target_len = NULL, negate = FALSE) {
  min_len <- target_len_min_single(target_len)
  max_len <- target_len_max_single(target_len)

  # `is.na(numeric(0L))` returns `logical(0L)` (bad) and `any(is.na(numeric(0L)))`
  # returns FALSE (good) - so be careful in cases where length is potentially 0.
  if (min_len == 0) {
    ## Length is in range [0, 1]
    if (max_len == 1) {
      # If the max length is 1, we can use `is.na` instead of `is.na`
      if (negate) {
        # 0-length -> TRUE, non-NA -> TRUE, NA -> FALSE
        rlang::expr(length(!!x) == 0 || !is.na(!!x))
      } else {
        # 0-length -> FALSE, non-NA -> FALSE, NA -> TRUE
        rlang::expr(length(!!x) != 0 && is.na(!!x))
      }
    } else {
      ## Length is in range [0, 2+)
      if (negate) {
        # 0-length -> TRUE, non-NA -> TRUE, NA -> FALSE
        rlang::expr(!any(is.na(!!x)))
      } else {
        # 0-length -> FALSE, no NA's -> FALSE, any NA's -> TRUE
        rlang::expr(any(is.na(!!x)))
      }
    }
  } else {
    # Minimum length is > 0, don't need to check for 0-length inputs
    if (max_len == 1) {
      # Length is exactly 1
      if (negate) rlang::expr(!is.na(!!x)) else rlang::expr(is.na(!!x))
    } else {
      # Length is in range [1, 2+)
      if (negate) rlang::expr(!any(is.na(!!x))) else rlang::expr(any(is.na(!!x)))
    }
  }
}

# Test whether any element of `x[[i]]` is NA, for all `x[[i]]` in `x`
nas_test_body_multi <- function(x, target_lens = NULL, negate = FALSE) {
  # Two cases:
  # 1. All elements are scalar  -> use `is.na(x)`
  # 2. All other cases          -> use `any(is.na(x))`
  # `any(is.na(x))` is FALSE iff `x` is 0-length or contains no NA's
  all_scalar <- !is.null(target_lens) && identical(unique(target_lens), 1)
  na_fn <- if (all_scalar) rlang::expr(is.na) else rlang::expr(\(x) any(is.na(x)))
  if (negate) {
    # Ex. `!any(vapply(list(1:5, 1:2), \(x) any(is.na(x)), logical(1L))) -> TRUE`
    rlang::expr(any(vapply(!!x, !!na_fn, logical(1L))))
  } else {
    # Ex. `any(vapply(list(NA, 1), is.na, logical(1L))) -> TRUE`
    rlang::expr(any(vapply(!!x, !!na_fn, logical(1L))))
  }
}

target_len_min_single <- function(target_len) {
  target_len_n <- length(target_len)
  if (is.null(target_len)) {
    0
  } else if (target_len_n == 1) {
    target_len
  } else if (target_len_n == 2) {
    target_len[[1]]
  } else {
    cli::cli_abort("Can't get minimum of `target_len`", .internal = TRUE)
  }
}

target_len_max_single <- function(target_len) {
  target_len_n <- length(target_len)
  if (is.null(target_len)) {
    Inf
  } else if (target_len_n == 1) {
    target_len
  } else if (target_len_n == 2) {
    target_len[[2]]
  } else {
    cli::cli_abort("Can't get maximum of `target_len`", .internal = TRUE)
  }
}

#' @export
emit_nas_error <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class
) {
  indexed_name <- paste0(x_name, format_index(x_index))
  cli::cli_abort(
    c(
      "{.arg {indexed_name}} must contain no NA or NaN values.",
      x = "{.arg {indexed_name}} is NA or NaN {at_locations(is.na(x))}."
    ),
    call = error_call,
    class = error_class,
    x_name = x_name,
    x_index = x_index
  )
}

#' @export
emit_nas_error_multi <- function(
    x,
    x_name,
    x_index,
    error_call,
    error_class
) {
  error_at <- which.max(vapply(x, \(x) any(is.na(x)), logical(1L)))
  error_index <- append(x_index, as.numeric(error_at))
  emit_nas_error(
    x = x[[error_at]],
    x_name = x_name,
    x_index = error_index,
    error_call = error_call,
    error_class = error_class
  )
}

# Test `emit_nas_error_multi`
if (FALSE) {
  emit_nas_error_multi(
    x = list(1, 2, 3, numeric(0L), c(1, 2, 3, NA, NA, NA, 5), NA, 10),
    x_name = "x",
    x_index = list(),
    error_call = caller_env(),
    error_class = "errr"
  )
}

# Test `nas_test_body_multi` and `nas_test_body`
if (FALSE) {

  ## `nas_test_body`

  # Length 1
  target_len <- 1
  f <- new_function(
    args = pairlist2(x = ),
    body = nas_check_body(sym("x"), list(), target_len)
  )

  # No Error, Error
  f(1L)
  f(NA)

  # Length 0-1
  target_len <- c(0, 1)
  f <- new_function(
    args = pairlist2(x = ),
    body = nas_check_body(sym("x"), list(), target_len)
  )

  # No Error
  f(numeric(0L))
  f(1L)

  # Error
  f(NA)

  # Length [0, Inf)
  target_len <- c(0, Inf)
  f <- new_function(
    args = pairlist2(x = ),
    body = nas_check_body(sym("x"), list(), target_len)
  )

  # No error
  f(numeric(0L))
  f(1:2)
  f(1)

  # Error
  f(NA)
  f(c(1, NA))

  ## `nas_test_body_multi`

  # TODO Ethan: Look at the `len` argument of the `emit_nas_error_multi` in `f`.
  # We'll want to make this more compact automatically via `rep(1, 10)`.
  #
  ## All length-1
  target_lens <- rep(1, 10)
  x <- as.list(rep(0, 10))
  f <- new_function(
    args = pairlist2(x = ),
    body = nas_check_body_multi(sym("x"), integer(), list(), target_lens)
  )

  # No error
  f(x)

  # Error
  x[[5]] <- NA
  f(x)

  # TODO Ethan: I actually think that the `emit_nas_error_multi` DOESN'T need to
  # know about the length, since it can always just use `any(is.na(x))` to check
  # safely.
  #
  ## All varying lengths
  target_lens <- list(c(1, 10), 11, NULL, 100, c(0, 1), 0)
  x <- list(1:5, rep(0, 11), 0:1000, rep(0, 100), 1L, logical(0L))
  f <- new_function(
    args = pairlist2(x = ),
    body = nas_check_body_multi(sym("x"), integer(), list(), target_lens)
  )

  # No error
  f(x)

  # Error
  x[[3]] <- NA
  f(x)

}

# null -------------------------------------------------------------------------

null_return_body <- function(x, spec_null_return) {
  specifyr_internal_error(x, "is.symbol", "base")
  rlang::expr(
    if (!!null_test_body(x, negate = FALSE)) {
      return(!!spec_null_return)
    }
  )
}

null_test_body <- function(x, negate = FALSE) {
  if (negate) rlang::expr(!is.null(!!x)) else rlang::expr(is.null(!!x))
}

# Test `null_return_body`
if (FALSE) {
  f <- rlang::new_function(
    args = pairlist2(x = ),
    body = null_return_body(sym("x"), TRUE)
  )

  # Returns nothing
  f(10)

  # Returns TRUE
  f(NULL)
}

# missing ----------------------------------------------------------------------

missing_return_body <- function(x, spec_missing_return) {
  specifyr_internal_error(x, "is.symbol", "base")
  rlang::expr(
    if (!!missing_test_body(x, negate = FALSE)) {
      return(!!spec_missing_return)
    }
  )
}

missing_test_body <- function(x, negate = FALSE) {
  if (negate) {
    rlang::expr(!rlang::is_missing(!!x))
  } else {
    rlang::expr(rlang::is_missing(!!x))
  }
}

# Test `missing_return_body`
if (FALSE) {
  f <- rlang::new_function(
    args = pairlist2(x = ),
    body = missing_return_body(sym("x"), TRUE)
  )

  # Returns nothing
  f(10)

  # Returns TRUE
  f()
}

# vctr -------------------------------------------------------------------------

# If no class is provided to `vector_spec`, we still want to check that the object
# is vector-like.

vctr_check_body <- function(x, error_index) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  rlang::expr(
    if (!!vctr_test_body(x, negate = TRUE)) {
      specifyr::emit_vctr_error(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class
      )
    }
  )
}

vctr_check_body_multi <- function(x, x_indices, error_index) {

  specifyr_internal_error(x, "is.symbol", "base")
  specifyr_internal_error(x_indices, "is.integer", "base")
  specifyr_internal_error(error_index, "is.list", "base")

  if (!is_empty(x_indices)) x <- rlang::call2("[", x, x_indices)
  rlang::expr(
    if (!!vctr_test_body_multi(x, negate = TRUE)) {
      specifyr::emit_vctr_error_multi(
        x = !!x,
        x_name = x_name,
        x_index = !!error_index,
        error_call = error_call,
        error_class = error_class
      )
    }
  )
}

vctr_test_body <- function(x, negate = FALSE) {
  if (negate) {
    rlang::expr(specifyr::is_not_vctr(!!x))
  } else {
    rlang::expr(specifyr::is_vctr(!!x))
  }
}

# TODO: Implement
vctr_test_body_multi <- function(x, negate = FALSE) {

}

# TODO: Implement
#' @export
emit_vctr_error <- function(...) {
  cli::cat_line("You caught a vector error!")
}

# TODO: Implement
#' @export
emit_vctr_error_multi <- function(...) {
  cli::cat_line("You caught a multi-vector error!")
}

# utils ------------------------------------------------------------------------

# These will need to be exported, so that the end user of the `*_spec` functions
# can access them.

#' @export
between1 <- function(x, left, right) { left <= x && x <= right }

#' @export
between <- function(x, left, right) { left <= x & x <= right }

#' @export
not_between1 <- function(x, left, right) { x < left || right < x }

#' @export
not_between <- function(x, left, right) { x < left | right < x }

#' @export
is_not_vctr <- function(x) {
  is.data.frame(x) || is.list(x) || !vctrs::obj_is_vector(x)
}

#' @export
is_vctr <- function(x) {
  !is.data.frame(x) && !is.list(x) && vctrs::obj_is_vector(x)
}
