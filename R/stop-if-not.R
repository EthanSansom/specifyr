
# TODO: Revise to make more like `stop_if_not()` with some added goodies
new_check2 <- function(test, header, bullets, env = rlang::caller_env()) {

  test_defused <- rlang::enexpr(test)
  header <- chr_(header, nas = FALSE)
  if (is.list(bullets)) {
    bullets <- check_bullet_names(lst_of_chr_(bullets, nas = FALSE))
  } else {
    bullets <- check_bullet_names(chr_(bullets, nas = FALSE))
  }
  env <- vir_(env)

  # Split a call `cnd1 && cnd2 && ... && cndn` into a list of `cnd1`, ..., `cndn`
  conditions <- list()
  call <- test_defused
  while (identical(call[[1]], quote(`&&`))) {
    conditions <- append(conditions, call[[length(call)]])
    call <- call[[2]]
  }
  # Add the one LHS condition left over and reverse to match the order of evaluation.
  # This also takes care of the case where there is no `&&` (i.e. only one condition).
  conditions <- rev(append(conditions, call))

  # If only one condition was provided, we don't need to do anything fancy, as
  # every bullet is supplied unconditionally in the error message
  if (length(conditions) == 1L) {

    if (is.list(bullets)) {
      bullets <- vctrs::list_unchop(bullets, name_spec = "{outer}")
    }
    # TODO: Maybe emit an error or a warning if bullets have blockers with only one condition
    names(bullets) <- gsub("_blk$", "", names(bullets))

    return_if_passed <- rlang::expr(if (!!test_defused) { return(x) })

    emit_error <- rlang::expr(
      cli::cli_abort(
        message = c(!!header, !!bullets),
        call = error_call,
        class = error_class
      )
    )

    check <- rlang::new_function(
      body = expr_squash(return_if_passed, emit_error),
      args = rlang::pairlist2(
        x = ,
        x_name = quote(rlang::caller_arg(x)),
        error_call = quote(rlang::caller_env()),
        error_class = quote(character(0L))
      ),
      env = env
    )
    return(check)
  }

  # Check that the conditions and bullets are recyclable (we've already check the length 1 condition)
  if (!is_empty(bullets) && length(conditions) != length(bullets)) {
    conditions_str <- paste0("{.code ", vapply(conditions, rlang::as_label, character(1L)), "}")
    cli::cli_abort(
      c(
        "Can't recycle conditions (size {length(conditions)}) to match {.arg bullets} (size {length(bullets)}).",
        i = "The following conditions were supplied to {.arg test}:",
        rlang::set_names(conditions_str, "*")
      )
    )
  }

  # Negate the conditions, so that bullets are only emitted if the condition is not met.
  not_conditions <- lapply(conditions, \(condition) rlang::expr(isFALSE(!!condition)))

  # Blockers are conditions which need to pass in order for subsequent bullets and
  # conditions from being evaluated while generating the message. This prevents
  # unsafe conditions from being evaluated.
  #
  # For instance, if we expect `x` to be a length 5 vector where the third element
  # is not NA, we'd test like so:
  #
  # `test = length(x) == 5 && !is.na(x[[3]])`
  # `bullets = c(
  #   x = if (length(x) != 5) "`x` is length {length(x)}",
  #   x = if (is.na(x[[3]])) `x` is NA at location `3`"
  # )`
  #
  # If `x` fails the length condition (say is length 1), then evaluating
  # `is.na(x[[3]])` to emit the second message in `bullets` will cause an
  # error. So, we make the length condition into a blocker like so:
  #
  # `bullets = c(
  #   x = if (length(x) != 5) "`x` is length {length(x)}",
  #   x = if (isTRUE(length(x) == 5) && is.na(x[[3]])) `x` is NA at location `3`"
  # )`
  #
  # Now, the `is.na(x[[3]])` condition is only evaluated when the blocker
  # (`length(x) == 5`) is TRUE, avoiding the error.
  blockers_at <- grepl("_blk$", names(bullets))
  if (any(blockers_at)) {
    new_blocker <- FALSE
    blocker_conditions <- list()
    for (i in seq(which.max(blockers_at), length(conditions))) {
      # `&&` the current blockers prior to the condition. This will look like:
      #
      # `blocker_k && ... && blocker_i_less_1 <- isTRUE(conditions[[i - 1]]) && isFALSE(conditions[[i]])`
      #
      # if a blocker was added in the last iteration (i - 1). And will look like:
      #
      # `blocker_k && ... && blocker_i_less_m && isFALSE(conditions[[i]])`
      #
      # if a blocker was added `m` iterations before the current one. Note that
      # if the first blocker, `blocker_k`, is `FALSE`, then none of these need
      # ever be evaluated - saving some time.
      not_conditions[[i]] <- expr_and(!!!blocker_conditions, not_conditions[[i]])

      # If we added a new blocker in the previous iteration, use the blocker's
      # symbol (i.e. `blocker_i_less_1`) in place of the blocker itself for
      # subsequent conditions to prevent repeated evaluation.
      if (new_blocker) {
        blocker_conditions[[length(blocker_conditions)]] <- blocker_sym
        new_blocker <- FALSE
      }

      # Add a blocker condition to the list of blockers
      if (blockers_at[[i]]) {
        blocker_sym <- rlang::sym(paste0("blocker_", i))
        blocker_is_okay <- rlang::expr(!!blocker_sym <- isTRUE(!!(conditions[[i]])))
        blocker_conditions <- append(blocker_conditions, blocker_is_okay)

        # Let the next iteration know that a new blocker assignment was made
        new_blocker <- TRUE
      }
    }
  }

  # Create a new bullets call, which selectively chooses bullets corresponding
  # to the conditions met.
  new_bullets <- mapply(
    condition = not_conditions,
    bullet = bullets,
    \(condition, bullet) rlang::expr(if (!!condition) { !!bullet })
  )
  new_bullets <- rlang::set_names(new_bullets, gsub("_blk$", "", names(bullets)))

  # Each bullet can itself be a vector of bullets, so we first put them in a list.
  # - `purrr::compact` removes `NULL` bullets (i.e. those where `isFALSE(...)`is `FALSE`)
  # - `vctrs::list_unchop()` flattens the list to a character vector of bullets,
  #   while keeping the names. A named vector (e.g. `list(i = c("Info1", "Info2")`))
  #   gets the same name for all elements (e.g. `c(i = "Info1", i = "Info2"`)`) using
  #   name_spec = "{outer}"
  new_bullets <- rlang::expr(vctrs::list_unchop(purrr::compact(list(!!!new_bullets)), name_spec = "{outer}"))

  # Call to test and return valid objects
  return_if_passed <- rlang::expr(if (!!test_defused) { return(x) })

  # Call to emit the error
  emit_error <- rlang::expr(
    cli::cli_abort(
      message = c(
        !!header,
        !!new_bullets
      ),
      call = error_call,
      class = error_class
    )
  )

  check <- rlang::new_function(
    body = expr_squash(return_if_passed, emit_error),
    args = rlang::pairlist2(
      x = ,
      x_name = quote(rlang::caller_arg(x)),
      error_call = quote(rlang::caller_env()),
      error_class = quote(character(0L))
    ),
    env = env
  )
  check
}

# New Idea for `stopifnot()` blog ----------------------------------------------

# TODO: For the blog, I want to make a drop-in replacement for `stopifnot` with
# tidyverse-compliant error messages - called `stopifnot2`. We'd replicate the
# entire `stopifnot()` process, even the annoying `exprs` and `exprObject` arguments...
# And try to maintain speed while also crafting a pretty message.
#
# Do something like: https://github.com/hadley/assertthat/tree/master
# - revive for the `stopifnot()` context
# - "tell" `stopifnot2()` about some custom error handling functions
#
# I want this to *literally* be a drop-in replacement to `stopifnot()`, with the
# same arguments and error states, so that you can do `stopifnot <- stopifnot2`
# and go on with your life.
#
# You can provide other functions, `stop_if_not` and `stop_if` which provide a
# different interface and allow the user to do more.

# Register a list of functions that `stopifnot2` knows about, like `all`, `any`,
# `!all`, `!any`, `>`, `==`, `identical` (have identical use `waldo::compare`),
# etc. There should be a registered function for each that does the following:
#
# function(call, env) {
#   result <- eval(call, env)
#   # - get information from the result object
#   # - get information from the call object
#   # - return the appropriate error bullets using this information
# }
#
# This ^ is the pattern from {assertthat} package
#
# Make a function which registers a recognized function. I'll make the `base` versions.
# `register_stop_fun` generates a function which grabs the arguments of a call (using rlang::call_match first)
# and their names. You can refer to them in the error bullets as `value` (for the value)
# and `value_name` (for the quoted name). If a `result` is provided, we evaluate
# `result <- eval(call, env)` and you can refer to it in your error message by the
# provided name.
#
# register_stop_fun <- function(fun, bullets, result = NULL) {
#
# }
#
# register_stop_fun(
#   dplyr::between,
#   c(
#     "{.arg {x_name}} must be between {left} and {right} inclusive.",
#     x = "{.arg {x_name}} is not in range [{left}, {right}] {.at {is.na(x) | x < left | x > right}}."
#   )
# )
#
# Or alternatively:
# register_stop_fun(
#   dplyr::between,
#   c(
#     "{.arg {x_name}} must be between {left} and {right} inclusive.",
#     x = "{.arg {x_name}} is not in range [{left}, {right}] {.at {map_lgl(result, ~!isTRUE(.x))}}."
#   ),
#   result = "result"
# )
#
# y <- 1:6
# stopifnot2(between(y, 2, 4))
# # Error:
# # ! `y` must be between 2 and 4 inclusive.
# # x `y` is not in range [2, 4] at locations `c(1, 5, 6)`.
#
# You can insert an expression to do this as well.
#
# register_stop_fun(
#   dplyr::between,
#   {
#     br <- if (isTRUE(inclusive)) c("[", "]") else c("(", ")")
#     must_be <- glue::glue("in range {br[[1]]}{left},{right}{br[[2]]}")
#     if (isTRUE(na.rm)) must_be <- paste("`NA` or", must_be)
#     c(
#       "{.arg {x_name}} {must_be}.",
#       x = "{.arg {x_name}} is not {must_be} {.at {!map_lgl(result, isTRUE)}}."
#     )
#   },
#   result = "result"
# )

# End of New Idea --------------------------------------------------------------
# - Although I said "End of..." there are some other good ideas floating around
#   this script that you should incorporate.

# TODO: Allow some of the bullets to be two bullets (an info `i` and an `x`) when
# required. See the notes below for details on the case of `all`, `any`, `!all`,
# `!any`.

# All: must always be `TRUE` (or `NA`)
# Any:
# - If no NA's:  "must be `TRUE` at least once, but is never `TRUE`".
# - If NA's:     "must never be `NA`", "is NA at locations..."
#
# Not All: "must always be `FALSE` (or `NA`)
# Not Any:
# - If no NA's:  "must never be `TRUE`", "is TRUE at locations..."
# - If NA's:     "must never be `NA`", "is NA at locations..."
#
# Single Condition: "must be `TRUE`, not {.obj_type_friendly {result}}."

# Examples:
# x <- c(1, 2, 3, NA)
# any(x == 3)
# i `x == 3` must never be `NA`.
# x `x == 3` is `NA` at location `4`.
#
# all(x == 3)
# i `x == 3` must always be `TRUE`.
# x `x == 3` is not `TRUE` at locations `c(1, 2, 4)`.
#
# all(x == 3, na.rm = TRUE)
# i `x == 3` must always be `TRUE` or `NA`.
# x `x == 3` is not `TRUE` or `NA` at locations `c(1, 2)`.
#
# any(x > 4, na.rm = TRUE)
# x `x > 4` must be `TRUE` at least once, but is never `TRUE`.
#
# length(x) == 5
# x `length(x) == 5` must be `TRUE`, not `FALSE`.

if (FALSE) {

  fn <- function(x = "default") fn
  rlang::call_match(quote(fn()), fn)
  rlang::call_match(quote(fn()), fn, defaults = TRUE)

  test[[1]]
  test[[2]]

  identical(test[[1]], quote(all))

  # We don't want the `default_bullet` to actually evaluate the bullets beforehand,
  # so, we'll just have it wrap the `test` in a call to `not_all_bullet(test, env)`,
  # `not_any_bullet(test, env)`, ... and so on, which is only evaluated if we need
  # the bullet.

  all_bullet <- function(test, env) {
    test_call <- rlang::call_match(call = test, fn = all, defaults = TRUE)
    test_args <- rlang::call_args(test_call)
  }

  x <- 1:50
  test <- rlang::expr(all(x > 10, x < 49, na.rm = FALSE))
  test_fn <- test[[1]]

  test_call <- rlang::call_match(call = test, fn = all)
  test_args <- rlang::call_args(test_call)

  na_rm <- isTRUE(test_args$na.rm)
  test_args <- test_args[rlang::names2(test_args) != "na.rm"]

  must <- switch(
    test_type,
    "all" = "always be `TRUE`, but is not `TRUE`",
    "any" = "never be `FALSE`, but is `FALSE`",
    "not_all" = "always be `FALSE`"
  )

  bullets <- lapply(
    test_args,
    \(test_arg) {
      result <- eval(test_arg)
      result[is.na(result)] <- if (na_rm) TRUE else FALSE
      if (!all(result)) {
        c(
          "{.code %s} must always be `TRUE`.",
          "{.code %s} is not `TRUE` %s."
        )
        sprintf(
          "{.code %s} must always be `TRUE`, but is not `TRUE` %s.",
          rlang::as_label(test_arg),
          at_loc_friendly(!result)
        )
      }
    }
  )

}

default_bullet <- function(test, env) {

  test_fn <- test[[1]]
  if (identical(test_fn, quote(`!`))) {
    test <- test[-1]
    test_fn <- test[[1]]
    negate <- TRUE
  } else {
    negate <- FALSE
  }

  if (identical(test_fn, quote(all))) {
    test_call <- rlang::call_match(call = test, fn = all, defaults = TRUE)
    test_args <- rlang::call_args(test_call)


  } else if (identical(test_call, quote(any))) {

  }
}

abort_with_default_bullets <- function(
    parent_env,
    test_defused,
    header,
    blockers,
    error_call,
    error_class
  ) {

}

# TODO: Teach `stop_if_not()` about `!`, `all`, and `any`, so it can improve
#       default error messages.
#
# Maybe add a `minimal_error` option, which
stop_if_not <- function(
    test,
    header,
    bullets,
    blockers, # Indicate blockers when using default bullets. If blockers is empty
              # (NULL) or `numeric(0L)`, then create the message for every bullet.
    error_call = rlang::caller_env(),
    error_class = character(0L)
  ) {

  # NOTE: Moving this before for good practice, see if the slowdown is noticable
  #
  # Only doing validation on a failed test to keep the "happy path" fast. This
  # will throw an error with call `stop_if_not()`, which will reveal problems
  # during testing.
  if (!rlang::is_missing(header)) chr_(header, nas = FALSE)
  if (!rlang::is_missing(bullets)) {
    if (is.list(bullets)) {
      lst_of_chr_(bullets, nas = FALSE)
    } else {
      chr_(bullets, nas = FALSE, null = TRUE)
    }
    check_bullet_names(bullets)
  }
  intish_(blockers, null = TRUE, nas = FALSE)
  vir_(error_call)
  chr_(error_class, nas = FALSE)

  test_defused <- substitute(test)
  if (isTRUE(test)) {
    return(invisible())
  }

  parent_env <- rlang::caller_env()

  # Split a call `cnd1 && cnd2 && ... && cndn` into a list of `cnd1`, ..., `cndn`
  conditions <- list()
  call <- test_defused
  while (identical(call[[1]], quote(`&&`))) {
    conditions <- append(conditions, call[[length(call)]])
    call <- call[[2]]
  }
  # Add the one LHS condition left over and reverse to match the order of evaluation.
  # This also takes care of the case where there is no `&&` (i.e. only one condition).
  conditions <- rev(append(conditions, call))

  # Create a default header if missing
  if (rlang::is_missing(header)) {
    header <- paste0("Condition {.code ", rlang::as_label(test_defused), "} is not `TRUE`.")
  }

  # The default bullets option is quite different...
  if (rlang::is_missing(bullets)) {
    abort_with_default_bullets(
      parent_env = parent_env,
      conditions = conditions,
      header = header,
      blockers = blocker,
      error_call = error_call,
      error_class = error_class
    )
  }

  # Create a default `header` and `bullets` if missing
  if (rlang::is_missing(bullets)) {
    # The default header is self-descriptive if there's only one condition
    if (length(conditions) == 1L && rlang::is_missing(header)) {
      bullets <- character(0L)
    } else {
      bullets <- vapply(conditions, rlang::as_label, character(1L))
      bullets <- sprintf(
        "{.code %s} must be `TRUE`, not {.obj_type_friendly {eval(conditions[[%i]], parent_env)}}.",
        bullets, seq_along(conditions)
      )
      names(bullets) <- rep("x", length(bullets))
    }
  }

  # If only one condition was provided, we don't need to do anything fancy, as
  # every bullet is supplied unconditionally in the error message
  if (length(conditions) == 1L) {
    if (is.list(bullets)) {
      bullets <- vctrs::list_unchop(bullets, name_spec = "{outer}")
    }
    # TODO: Maybe emit an error or a warning if bullets have blockers with only one condition
    names(bullets) <- gsub("_blk$", "", names(bullets))

    cli::cli_abort(
      message = c(header, bullets),
      call = error_call,
      class = error_class
    )
  }

  # Check that the conditions and bullets are recyclable
  if (!is_empty(bullets) && length(conditions) != length(bullets)) {
    conditions_str <- paste0("{.code ", vapply(conditions, rlang::as_label, character(1L)), "}")
    cli::cli_abort(
      c(
        "Can't recycle conditions (size {length(conditions)}) to match {.arg bullets} (size {length(bullets)}).",
        i = "The following conditions were supplied to {.arg test}:",
        rlang::set_names(conditions_str, "*")
      )
    )
  }

  if (!is_empty(bullets)) {
    # Accumulate the indices of bullets to emit, stopping if we hit a blocker
    blockers_at <- grepl("_blk$", names(bullets))
    bullets_to_emit <- c()
    for (i in seq_along(conditions)) {
      result <- isTRUE(eval(conditions[[i]], parent_env))
      # If the condition failed, add the corresponding bullet to the message
      if (!result) {
        bullets_to_emit <- c(bullets_to_emit, i)
        # If the condition did not pass and it's a blocker, stop adding bullets
        if (blockers_at[[i]]) {
          break
        }
      }
    }

    bullets <- bullets[bullets_to_emit]
    if (is.list(bullets)) {
      bullets <- vctrs::list_unchop(bullets, name_spec = "{outer}")
    }
    names(bullets) <- gsub("_blk$", "", names(bullets))
  }

  cli::cli_abort(
    message = c(header, bullets),
    call = error_call,
    class = error_class
  )
}

check_bullet_names <- function(
    bullets,
    bullets_name = rlang::caller_arg(bullets),
    error_call = rlang::caller_env(),
    error_class = character(0L)
  ) {

  target_nms <- c("!", "*", "x", "v", "i", " ")
  if (all(rlang::names2(bullets) %in% c(target_nms, paste0(target_nms, "_blk")))) {
    return(bullets)
  }

  bullet_nms <- rlang::names2(bullets)
  if (any(bullet_nms == "")) {
    cli::cli_abort(
      c(
        "{.arg {bullets_name}} must be named.",
        x = "{.arg {bullets_name}} is unnamed {at_loc_friendly(bullet_nms == '')}."
      ),
      call = error_call,
      class = error_class
    )
  }

  invalid_names <- setdiff(bullet_nms, c(target_nms, paste0(target_nms, "_blk")))
  cli::cli_abort(
    c(
      "{.arg {bullets_name}} must have names in {.val {target_nms}}, possibly suffixed with {.val _blk}.",
      x = "{.arg {bullets_name}} {?has an/has} invalid name{?s}: {.val {invalid_names}}."
    ),
    call = error_call,
    class = error_class
  )
}

# Testing
if (FALSE) {

  f <- new_check2(
    is.integer(x) &&
      (all(x >= 0 & x <= 100, na.rm = TRUE)) &&
      (all(x %% 2 == 0 | x %% 3 == 0, na.rm = TRUE)),

    header = "{.arg {x_name}} must be an integer, between 0 and 100, divisible by 2 or 3.",
    bullets = c(
      # TODO: Implement the `{.a {x}}` shorthand for type friendly
      x_blk = "{.arg {x_name}} is {.obj_type_friendly {x}}.",
      # TODO: Implement the `{.at {expr}}` shorthand to `at_loc_friendly(expr)`
      x = "{.arg {x_name}} is not between 0 and 100 {at_loc_friendly(x < 0 | x > 100)}.",
      x = "{.arg {x_name}} is not a multiple of 2 or 3 {at_loc_friendly(x %% 2 != 0 & x %% 3 != 0)}."
    )
  )

  # Passes
  f(10L)

  # Fails
  f(1:1000)
  f(13L)
  f("A")
  f(mean)

  base_stop1 <- function(x) {
    stopifnot(
      "`x` must be an integer, between 0 and 100, divisible by 2 or 3." =
        is.integer(x) &&
        all(x[!is.na(x)] >= 0 & x[!is.na(x)] <= 100) &&
        all(x[!is.na(x)] %% 2 == 0 | x[!is.na(x)] %% 3 == 0)
    )
    x
  }

  base_stop2 <- function(x) {
    stopifnot(
      "`x` must be an integer" = is.integer(x),
      "`x` must be between 0 and 100" = x[!is.na(x)] >= 0 & x[!is.na(x)] <= 100,
      "`x` must be divisible by 2 or 3" = x[!is.na(x)] %% 2 == 0 | x[!is.na(x)] %% 3 == 0
    )
    x
  }

  # TODO:
  # - Add a `postscript` argument, for bullets which are always dispatched
  # - Allow bullets to be length 0, in which case we don't use add any
  specifyr_stop <- function(x) {
    stop_if_not(
      is.integer(x) &&
        all(x[!is.na(x)] >= 0 & x[!is.na(x)] <= 100) &&
        all(x[!is.na(x)] %% 2 == 0 | x[!is.na(x)] %% 3 == 0),
      "`x` must be an integer, in range [0, 100], divisible by 2 or 3.",
      c(
        x = "`x` is {.obj_type_friendly {x}}.",
        x = "`x` is not in range [0, 100] {at_loc_friendly(is.na(x) | x < 0 | x > 100)}.",
        x = "`x` is not divisible by 2 or 3 {at_loc_friendly(is.na(x) | x %% 2 != 0 | x %% 3 != 0)}."
      )
    )
    x
  }

  x <- seq(2L, 50L, 2L)
  bench::mark(
    base_stop1(x),
    base_stop2(x),
    specifyr_stop(x)
  )

  x <- seq(2L, 102L, 2L)
  bench::mark(
    try(base_stop1(x)),
    try(base_stop2(x)),
    try(specifyr_stop(x)),
    check = FALSE
  )

}
