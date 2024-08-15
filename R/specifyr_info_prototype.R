if (FALSE) {
  library(rlang)
  library(purrr)

  # test -------------------------------------------------------------------------

  test <- function(.expr, ..., .env = rlang::caller_env(), .args = pairlist()) {

    if ((args_supplied <- length(.args)) && rlang::dots_n(...)) {
      stop("One of `.args` or `...` must be empty.")
    }
    args <- if (args_supplied) .args else rlang::pairlist2(...)
    if ("x" %in% names(args)) {
      stop(paste0(
        "\"x\" must not be in `names(",
        if (args_supplied) ".args" else "...", ")`."
      ))
    }

    expr <- rlang::enexpr(.expr)
    out <- new_function(
      args = c(rlang::pairlist2(x = ,), args),
      body = rlang::expr(isTRUE(!!expr)),
      env = .env
    )
    class(out) <- c("specifyr_test", "function")
    out
  }

  is_test <- function(x) inherits(x, "specifyr_test")

  # info -------------------------------------------------------------------------

  # TODO: Make it so that `pass`, `fail`, `must`, `header` have to be characters
  #       OR an explicitly quoted expression.
  info <- function(test, fail, pass, must, header, arg_names = FALSE) {

    # `test()` must be a test
    stopifnot("`test` must be a <specifyr_test> object." = is_test(test))

    # The test expr is the first argument supplied to `isTRUE()` in the test body
    test_expr <- rlang::fn_body(test)[[2]][[2]]
    args <- rlang::fn_fmls(test)
    env <- rlang::fn_env(test)

    if (rlang::is_missing(fail)) {
      # Default bullet: "`isTRUE(test)` is `FALSE.`"
      fail <- rlang::expr(
        paste0("`", rlang::as_label(quote(isTRUE(!!test_expr))), "` is `FALSE`.")
      )
    } else {
      # Otherwise, take whatever expression was supplied
      fail <- rlang::enexpr(fail)
    }

    if (rlang::is_missing(pass)) {
      pass <- rlang::expr(
        paste0("`", rlang::as_label(quote(!!test_expr)), "` is `TRUE`.")
      )
    } else {
      pass <- rlang::enexpr(pass)
    }

    if (rlang::is_missing(must) && rlang::is_missing(header)) {
      # Default header: "`test` must be `TRUE`.
      header <- rlang::expr(
        paste0("`", rlang::as_label(quote(!!test_expr)), "` must be `TRUE`.")
      )
    } else if (rlang::is_missing(header)) {
      stopifnot("`must` must be a string." = is.character(must) && length(must) == 1 && !is.na(must))
      header <- rlang::expr(paste("{.arg {x_name}} must", !!must))
    } else {
      header <- rlang::enexpr(header)
    }

    # Arguments: `x_name = caller_arg(x)`, `arg1_name = caller_arg(arg1)`, etc.
    if (arg_names) {
      x_name_args <- lapply(
        names(args),
        \(x) rlang::expr(rlang::caller_arg(!!rlang::sym(x)))
      )
      names(x_name_args) <- paste0(names(args), "_name")
      x_name_args <- as.pairlist(x_name_args)
    } else {
      x_name_args <- pairlist2(x_name = quote(rlang::caller_arg(x)))
    }

    body <- rlang::expr({
      header <- !!header
      if (isTRUE(!!test_expr)) {
        pass <- !!pass
        cli::cli_inform(
          c(i = header, v = pass),
          class = "specifyr_message_info_pass"
        )
      } else {
        fail <- !!fail
        cli::cli_inform(
          c(i = header, x = fail),
          class = "specifyr_message_info_fail"
        )
      }
    })

    out <- new_function(
      body = body,
      args = c(args, x_name_args),
      env = env
    )
    class(out) <- c("specifyr_info", "function")
    out
  }

  is_info <- function(x) inherits(x, "specifyr_info")

  f <- info(
    test = test(x > above, above = -Inf),
    must = "be greater than {above}.",
    fail = "{.arg {x_name}} has a minimum of {min(x)}.",
    pass = "{.arg {x_name}} is greater than {above}.",
    arg_names = TRUE
  )

  f(10, above = 11)

  f_body <- rlang::fn_body(f)

  # TODO: One function we're going to want is an assignment (`<-`) grabber, so
  #       we can scoop up the `header`, `fail`, and `pass` expressions.
  lobstr::ast(!!f_body)

  # and --------------------------------------------------------------------------

  # TODO: `and()` is a work in progress!

  # Note that `and()` and `or()` produce functions with class:
  # c("specifyr_info_and", "specifyr_info", "function")

  # - `...` are `test()` or information functions (`info()`, `or()`, `and()`)
  #   - if an argument to `...` is a `list()`, all conditions are checked together
  #     and then emitted
  # - `.fail`, `.pass`, ... etc. are the same as in `info()`
  and <- function(..., .fail, .pass, .must, .header, .arg_names = FALSE) {

    informants <- purrr::modify_at(rlang::list2(...), is_test, info)

    non_informants <- !purrr::map_lgl(informants, is_info)
    if (any(non_informants)) {
      stop("`...` must be <specifyr_test> or <specifyr_info> objects.")
    }

    # `and_test_expr` looks like: `test_1 && test_2 && ... && test_n`
    info_test_exprs <- map(informants, get_test_expr)
    and_test_expr <- expr_and(!!!info_test_exprs)

    # By default, make the `fail`/`pass` bullets using those of the `informants`
    if (rlang::is_missing(.fail)) {
      info_fail <- purrr::map(informants, get_fail)
      fail <- ""
    } else {
      fail <- rlang::enexpr(.fail)
    }
    if (rlang::is_missing(.pass)) {
      info_pass <- purrr::map(informants, get_pass)
      pass <- ""
    } else {
      pass <- rlang::enexpr(.pass)
    }

    if (rlang::is_missing(.header)) {
      # Default header: "`test_1 && test_2 && ... && test_n` must be `TRUE`."
      header <- rlang::expr(
        paste0("`", as_label(!!and_test_expr), "` must be `TRUE`.")
      )
    } else {
      header <- rlang::enexpr(.header)
    }

  }

  # Test how `and()` bullets would actually work

  # 1. Emit a Single Bullet ----
  rm(list = c(".t1.", ".t2.", ".t3."))
  x <- NA
  x_name <- "x"

  # Bool test
  if (isTRUE(
    (.t1. <- is.logical(x)) &&
    (.t2. <- length(x) == 1) &&
    (.t3. <- !is.na(x))
  )) {
    cli::cli_alert_success("{.arg {x_name}} is a bool.")
  }

  # Bool bullets, emit the first. Set the default to `TRUE`, so we don't emit a
  # bullet for an argument which was never checked.
  results <- rlang::env_get_list(nms = c(".t1.", ".t2.", ".t3."), default = TRUE)
  fail_at <- which(!map_lgl(results, isTRUE))

  fail <- c(
    "{.arg {x_name}} is {.obj_type_friendly {x}}.",
    "{.arg {x_name}} is length {length(x)}.",
    "{.arg {x_name}} is `NA`."
  )[fail_at]

  # cli::cli_abort(fail)

  # 2. Bullets are emitted together ----

  # BOOL EXAMPLE
  rm(list = c(".t1.", ".t2.", ".t3."))
  x <- c(NA, FALSE)
  x_name <- "x"

  # Bool test
  if (isTRUE(
    (.t1. <- is.logical(x)) &&
    # NOTE: Here's the key. If the user wants multiple bullets to be emitted
    #       together then we just DON'T short circuit, so their will be potentially
    #       more than one failure
    ((.t2. <- length(x) == 1) & (.t3. <- !anyNA(x)))
  )) {
    cli::cli_alert_success("{.arg {x_name}} is a bool.")
  }

  # Bool bullets, emit the first
  results <- rlang::env_get_list(nms = c(".t1.", ".t2.", ".t3."), default = TRUE)
  fail_at <- which(!map_lgl(results, isTRUE))

  fail <- c(
    x = "{.arg {x_name}} is {.obj_type_friendly {x}}.",
    x = "{.arg {x_name}} is length {length(x)}.",
    x = "{.arg {x_name}} is `NA`."
  )[fail_at]

  # cli::cli_abort(fail)

  ## NUMERIC EXAMPLE

  rm(list = c(".t1.", ".t2.", ".t3.", ".t4."))
  x <- c(NA, 10, 11, 50, 11)
  x_name <- "x"

  # Numeric test
  if (isTRUE(
    # This is a guard, we'll emit after immediately after it fails
    (.t1. <- is.numeric(x)) &&
    # These can both be checked
    ((.t2. <- length(x) == 4) & (.t3. <- !anyNA(x))) &&
    # This is checked alone
    (.t4. <- all(x > 5))
  )) {
    cli::cli_alert_success("{.arg {x_name}} is a non-missing length 4 numeric above 5.")
  }

  # Bullets
  results <- rlang::env_get_list(nms = c(".t1.", ".t2.", ".t3.", ".t4."), default = TRUE)
  fail_at <- which(!map_lgl(results, isTRUE))

  fail <- c(
    x = "{.arg {x_name}} is {.obj_type_friendly {x}}.",
    x = "{.arg {x_name}} is length {length(x)}.",
    x = "{.arg {x_name}} is NA at locations `c({which(is.na(x))})`.",
    x = "{.arg {x_name}} is less than or equal to 5 at locations `c({which(x > 5)})."
  )[fail_at]

  # cli::cli_abort(fail)


  # or ---------------------------------------------------------------------------

  or <- function(..., .fail, .pass, .must, .header, .arg_names = FALSE) {

  }

  # Test how `or()` bullets would actually work

  # 1. No Extra information passed to the `and()` or `or()` bullet (worst case)

  # `x` is a length 2 logical or a length 5, non-missing, numeric

  rm(list = c(".t1.", ".t2.", ".t3.", ".t4.", ".t5."))
  x <- c(NA, 10, 11, 50, 11)
  x_name <- "x"

  # We test using two separate blocks, as normal. Each of these blocks was already
  # generated for us via `and()`.
  if (isTRUE(
    (
      (
        (.t1. <- is.logical(x)) &&
        (.t2. <- length(x) == 2)
      ) ||
      (
        (.t3. <- is.numeric(x)) &&
        (.t4. <- length(x) == 5) &&
        (.t5. <- !anyNA(x))
      )
    )
  )) {
    cli::cli_alert_success("{.arg {x_name}} is a length 2 logical or a length 5, non-missing, numeric.")
  }

  # Each `||` block needs it's own results
  results_1 <- rlang::env_get_list(nms = c(".t1.", ".t2."), default = TRUE)
  results_2 <- rlang::env_get_list(nms = c(".t3.", ".t4.", ".t5."), default = TRUE)
  fail_at_1 <- which(!map_lgl(results_1, isTRUE))
  fail_at_2 <- which(!map_lgl(results_2, isTRUE))

  # We only emit failure bullets for the `||` block(s) which had the most completed
  # checks. Note that > 1 `||` block(s) failure messages will be emitted in the case
  # of a tie. The number of passes one less than the index of the first failure.
  n_passes <- c(fail_at_1[[1]] - 1L, fail_at_2[[1]] - 1L)

  fail <- list(
    # Failure bullets for the first block (logical, length-2)
    c(
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}.",
      x = "{.arg {x_name}} is length {length(x)}."
    )[fail_at_1],

    # Failure bullets for the second block (numeric, length-5, non-missing)
    c(
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}.",
      x = "{.arg {x_name}} is length {length(x)}.",
      x = "{.arg {x_name}} is NA at locations `c({which(is.na(x))})`."
    )[fail_at_2]
  )[n_passes == max(n_passes)]
  # Notice ^ that we only grab failure bullets from the option with most passes

  # The `pass` bullets are similar to the `fail` bullets
  pass <- list(
    # Pass bullets for the first block (logical, length-2)
    c(
      i = "{.arg {x_name}} is logical.",
      i = "{.arg {x_name}} is length 2."
    )[sequence(n_passes[[1]])],

    # Pass bullets for the second block (numeric, length-5, non-missing)
    c(
      i = "{.arg {x_name}} is numeric.",
      i = "{.arg {x_name}} is length 5.",
      i = "{.arg {x_name}} contains no `NA` values."
    )[sequence(n_passes[[2]])]
  )[n_passes == max(n_passes)]

  # For the requirements section, we have a slight pickle. Here are the cases:
  # 1. Requirements (i.e. a `must` or `header`) are given for the entire `and()`
  # - easy, we just emit that requirement
  # 2. Requirements are given for each test in the `and()` block, NOT via `must`
  # - need to conditionally add a header which says "All conditions must be TRUE"
  # - then, report each header individually
  # 3. Requirements are given for each test in the `and()` block using `must`
  # - we can say "`x` must {must_1}, {must_2}, ..., and {must_3}.
  #
  # The example below we'll say was case 2.
  requirements <- list(
    ## Requirements for the first block (logical, length 2)

    # In the case where requirements are given, if there is more than 1 bullet
    # we need to group them together.
    if (fail_at_1[[1]] > 1) c(`*` = "All conditions must be satified:"),
    # We want to emit requirement bullets for anything that passed OR failed
    c(
      `*` = "{.arg {x_name}} must be logical.",
      `*` = "{.arg {x_name}} must be length 2."
    )[union(sequence(n_passes[[1]]), fail_at_1)],

    ## Requirements for the second block (numeric, length-5, non-missing)
    if (fail_at_2[[1]] > 1) c(`*` = "All conditions must be satified:"),
    c(
      `*` = "{.arg {x_name}} must be numeric.",
      `*` = "{.arg {x_name}} must be length 5.",
      `*` = "{.arg {x_name}} must be non-NA."
    )[union(sequence(n_passes[[2]]), fail_at_2)]
  )

  # NOTE: In practice, I think the real way to do this is with functions!
  # - you make some kind of wrapper and provide it with the necessary user
  #   supplied bullets
  if (FALSE) {
    # Given the bullets and locations of passed / failed tests, these functions
    # do the work of gathering and formatting the correct bullets.
    requirements <- list(
      require_bullets(
        bullets = c(
          `*` = "{.arg {x_name}} must be logical.",
          `*` = "{.arg {x_name}} must be length 2."
        ),
        fail_at = fail_at_1,
        n_pass = n_passes[[1]]
      ),
      require_bullets(
        bullets = c(
          `*` = "{.arg {x_name}} must be numeric.",
          `*` = "{.arg {x_name}} must be length 5.",
          `*` = "{.arg {x_name}} must be non-NA."
        ),
        fail_at = fail_at_2,
        n_pass = n_passes[[2]]
      )
    )
  }

  message <- list(
    requirements,
    pass,
    fail
  )
  message <- vctrs::vec_c(!!!purrr::map(message, purrr::compact))

  # TODO: We'll definitely want to indent the bullets under "All conditions..."
  # cli::cli_abort(unlist(message))

  # Messaging Helpers ------------------------------------------------------------

  # These are the functions that will be within the *generated* specifyr functions.
  # They handle the messaging under the hood. All these should really receive are
  # the appropriate bullets.

  ## How to: And and Or ----------------------------------------------------------

  # `and()`, no nesting:

  # ! header
  # x bullets[first_fail]

  # `or()`, no nesting, header provided / default header:
  # ! header
  # x bullets[[1]]
  # ...
  # x bullets[[n]]

  # `or()`, no nesting, only inner headers provided
  # ! Any of the following must be satisfied:
  # * header[[1]]
  # ...
  # * header[[n]]
  # x bullets[[1]]
  # ...
  # x bullets[[n]]

  # `and()`, with nested `or()` and no top level header. E.g. `and(or(p1, p2), p3)`
  # ! Any of the following must be satisfied:
  # * p1_header
  # * p2_header
  # x p1_bullets
  # x p2_bullets
  #
  # Note: this supposes that `or(p1, p2)` fails.

  # `and()`, with nested `or()`, with top level header. E.g. `and(or(p1, p2), p3, .header = h1)`
  # ! h1
  # x p1_bullets
  # x p2_bullets

  # TODO: I think the `list("bullet 1" = p1, "bullet 2" = p2)` dynamic in `and()`
  # shouldn't allow inner `and()` or `or()`.

  # `or()`, with nested `and()`

  # NOTE: What if we force a single top level header!

  # Right now, the below generates the following message:
  # and(
  #   or(
  #     info(t1, fail = "t1 is FALSE.", header = "t1 must be TRUE."),
  #     info(t2, fail = "t2 is FALSE.", header = "t2 must be TRUE.")
  #   ),
  #   info(t3, fail = "t3 is FALSE.", header = "t3 must be TRUE.")
  # )
  # ! All of the following must be satisfied.
  # * Any of the following must be satisfied:
  #   * t1 must be TRUE.
  #   * t2 must be TRUE.
  # * t3 must be TRUE.
  # x t1 is FALSE.
  # x t2 is FALSE.

  # But really, we should demand a single header. AND, if it doesn't exist, we
  # should just use the default:
  #
  # ! `(t1 || t2) && t3` must be `TRUE`.
  # x t1 is FALSE.
  # x t2 is FALSE.

  # Providing a default header.
  # and(
  #   .header = "t1 or t2, and t3, must be satisfied.",
  #   or(
  #     info(t1, fail = "t1 is FALSE.", header = "t1 must be TRUE."),
  #     info(t2, fail = "t2 is FALSE.", header = "t2 must be TRUE.")
  #   ),
  #   info(t3, fail = "t3 is FALSE.", header = "t3 must be TRUE.")
  # )
  # ! t1 or t2, and t3, must be satisfied.
  # x t1 is FALSE.
  # x t2 is FALSE.

  # Use the `.fail` argument of `or` to specify a single fail bullet for the entire
  # or block.
  # and(
  #   .header = "t1 or t2, and t3, must be satisfied.",
  #   or(
  #     info(t1, fail = "t1 is FALSE.", header = "t1 must be TRUE."),
  #     info(t2, fail = "t2 is FALSE.", header = "t2 must be TRUE."),
  #     .fail = "t1 and t2 are FALSE."
  #   ),
  #   info(t3, fail = "t3 is FALSE.", header = "t3 must be TRUE.")
  # )
  # ! t1 or t2, and t3, must be satisfied.
  # x t1 and t2 are FALSE.

  # The above is equivalent to the simpler:
  # and(
  #   .header = "t1 or t2, and t3, must be satisfied.",
  #   or(t1, t2, .fail = "t1 and t2 are FALSE."),
  #   info(t3, fail = "t3 is FALSE.")
  # )
  # ! t1 or t2, and t3, must be satisfied.
  # x t1 and t2 are FALSE.

  ## How to End ------------------------------------------------------------------

  # TODO:
  # Note, `&&` and `||` are associative, so you can collapse `(p1 && p2) && p3`
  # into `p1 && p2 && p3`. Likewise `(p1 || p2) || p3` is just `p1 || p2 || p3`.
  #
  # So, `or(or(), ...)` should be just a single call to `or(...)`. Likewise
  # for `and(and(), ...)`. The arguments of the outermost argument will always
  # take precedence. Although, maybe this is too much work? No, doing it this
  # way makes it easy to collapse pre-made `or()` info blocks into a single `or()`
  # via `or(premade_or_1, premade_or_2)`.
  #
  # All that being said, the `or_fail/pass/info()` and `and_fail/pass/info()`
  # functions can assume that they'll never be nested within themselves (e.g.
  # no `or_fail(or_fail())`).

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

  # - receives the `fail` bullets
  and_fail <- function(fail, fail_at) {
    # TODO: Expect a list of vectors of bullets for a `fail`, and a logical vector
    # indicating which failed for `fail_at`.
    stopifnot(is.list(fail), is.logical(fail_at))

    bullets <- flatten_lst_of_chrish(fail[fail_at])
    class(bullets) <- c("specifyr_and_fail", class(bullets))
    bullets
  }

  # bullets for `is.logical(x) && length(x) == 1`
  fail <- list(
    x = cli::style_bold("{.arg {x_name}} is {.a {x}}."),  # is.logical(x)
    x = c(
      "{.arg {x_name}} is length {length(x)}.",        # length(x) == 1
      "{.arg {x_name}} is no good"
    )
  )

  # Provides the `fail`, `failed`, `header`
  # - note, either every `||` failed or it's a pass, so `failed` is just a TRUE/FALSE
  or_fail <- function(fail, failed, header) {

    # TODO: Add formal checks.
    stopifnot(is.list(fail), is.list(header), rlang::is_bool(failed))

    # If we didn't fail, we have nothing to worry about
    if (!failed) {
      return(character())
    }

    # If we have more than one header, preface that all must be satisfied.
    if (length(header) > 1) {
      # Any unnamed header bullets become `*`. Usually, {cli} would automatically
      # add a `!` to a single unnamed header.
      names(header) <- ifelse(rlang::names2(header) == "", "*", names(header))
      header <- c("One of the following must be satisfied:", header)
    }

    # TODO: Nested `or()` bullets should be intelligently flattened when they are
    # initially created.
    # Eg. `or(is.chill(x)`, or(is.nice(x), and(is.funny(x), is.humble(x)))` becomes
    # `or(is.chill(x), is.nice(x), and(is.funny(x), is.humble(x)))`
    #
    # This way, I don't need to worry about `or()` within `or()`, just `or()` within
    # `and()` and `and()` within `or()`.

    bullets <- c(flatten_lst_of_chrish(header), flatten_lst_of_chrish(fail))
    # class(bullets) <- c("specifyr_or_fail", class(bullets))
    bullets
  }

  and_fail(
    fail = list(
      or_fail(
        fail = list(
          x = "`x` is not length 1.",
          x = "`x` is not length 5"
        ),
        header = list("`x` must be length 1 or 5."),
        failed = TRUE
      ),
      x = "`x` is not a logical vector"
    ),
    fail_at = c(TRUE, TRUE)
  )

  # - receives the `fail` bullets
  or_fail <- function(...) {

  }

  # - recieves the `pass` bullets
  and_pass <- function(...) {

  }

  # - recieves the `pass` bullets
  or_pass <- function(...) {

  }

  # - receives the `header` or `must` bullets
  or_context <- function(...) {

  }

  # Formats `{.arg {x_name}} must...`, combining the `...` as "..1, ..2, ..., and ..n."
  x_must <- function(...) {
    paste0("{.arg {x_name}} must ", oxford(c(...), last = "and"), ".")
  }

  # cli theme --------------------------------------------------------------------

  # NOTE: You can see how {usethis} implements a custom theme here:
  # - https://github.com/r-lib/usethis/blob/main/R/utils-ui.R

  specifyr_theme <- function() {

    builtin <- cli::builtin_theme()
    bullets <- builtin[grepl("^\\.bullets \\.bullet-(i|v|!|x|v|\\*|>)", names(builtin))]
    bullet_symbols <- gsub(".*\\.bullet-(i|v|!|x|v|\\*|>)$", "\\1", names(bullets))

    # Single indented and double indented versions of {cli} bullets. E.g. `v` is a
    # check with no indent, `vv` is single indented, `vvv` is double indented.
    single_indent_bullets <- bullets |>
      purrr::map(\(bullet) c(bullet, `margin-left` = 2)) |>
      rlang::set_names(paste0(names(bullets), bullet_symbols))
    double_indent_bullets <- bullets |>
      purrr::map(\(bullet) c(bullet, `margin-left` = 4)) |>
      rlang::set_names(paste0(names(bullets), strrep(bullet_symbols, 2)))

    rlang::list2(
      !!!single_indent_bullets,
      !!!double_indent_bullets,

      # E.g. "at locations `c(1, 2, 3, 4, 5)` and 5 more"
      span.at = list(
        transform = function(x) at_location_friendly(x)
      ),

      # "{.a {x}}" is shorthand for "{.obj_type_friendly {x}}"
      span.a = builtin$span.obj_type_friendly
    )
  }

  specifyr_message <- function(message) {
    cli::cli_div(theme = specifyr_theme())
    cli::cli_inform(message)
  }

  at_location_friendly <- function(x, trunc = 5) {
    if (is.logical(x)) {
      is.na(x) <- FALSE
      x <- which(x)
    }
    if (!is_integerish(x)) {
      stop("`x` must be integerish.")
    }

    if (length(x) > trunc) {
      and_more <- paste(" and", length(x) - trunc, "more")
      length(x) <- trunc
    } else {
      and_more <- ""
    }

    n <- length(x)
    if (!n) {
      return(paste0("at location `c()`", and_more))
    } else if (n <= 1) {
      return(paste0("at location `", x, "`", and_more))
    }
    paste0("at locations `c(", commas(x), ")`", and_more)
  }

  commas <- function(x) {
    paste(x, collapse = ", ")
  }

  oxford <- function(x, sep = ", ", last = "or", trunc = Inf) {
    x_len <- length(x)
    if (x_len <= 1) {
      return(paste(x))
    } else if (x_len > trunc) {
      length(x) <- trunc
      return(paste(paste(x[-trunc], collapse = sep), "...", paste(last, x[[trunc]]), sep = sep))
    }
    if (x_len == 2) sep <- " "
    paste(paste(x[-x_len], collapse = sep), paste(last, x[[x_len]]), sep = sep)
  }

  # New Examples -----------------------------------------------------------------

  # Rules:
  # 1. Every `info()` must have a single top-line header
  # 2. Messages are structured like so:
  #
  # - `and()`, emit the first error bullet encountered
  #> ! header
  #> x first_error_bullet
  #
  # - `or()`, emit every error bullet encountered
  #> ! header
  #> x error_bullet[[1]]
  #> ...
  #> x error_bullet[[n]]
  #
  # - `and(or(), ...)`, or within `and()`, exact same - just emit all the or bullets
  #> ! header
  #> x or_error_bullet[[1]]
  #> ...
  #> x or_error_bullet[[n]]
  #
  # - `or(and(), ...)`, and within `or()`, optionally emit `i` bullets for partial passes
  #> ! header
  #> i first_partial_pass_bullet
  #> x first_error_bullet

  # `or(and(), ...)` example:
  # or(
  #   and(
  #     info(test(is.numeric(x)), pass = "`x` is numeric.", fail = "`x` is {.a {x}}."),
  #     info(test(length(x) == 1), pass = "`x` is length 1.", fail = "`x` is {length(x)}.")
  #   ),
  #   test(is.character(x))
  # )
  #
  # Supposing `x = 1:2`
  # ! (is.numeric(x) && length(x) == 1) || is.character(x) must be `TRUE`.
  # i `x` is numeric.
  # x `x` is length 2.
  # x `is.character(x)` is `FALSE`.

  and_info <- function(fail, pass, fail_at, pass_at) {
    c(
      flatten_lst_of_chrish(pass[pass_at]),
      flatten_lst_of_chrish(fail[fail_at])
    )
  }

  or_info <- function(fail, failed) {
    if (!failed) {
      return(character())
    }
    flatten_lst_of_chrish(fail)
  }

  # Test for `x` is numeric AND length 1, or `x` is a character
  suppressWarnings(rm(t1, t2, t3, t4))
  x <- 1:2
  if ((t4 <- (t1 <- is.numeric(x)) && (t2 <- length(x) == 1)) || (t3 <- is.character(x))) {
    cli::cli_alert_success("`x` is good!")
  }

  or_info(
    fail = list(

      # `x` must be a numeric && length 1
      and_info(
        fail = list(x = "`x` is not numeric.", x = "`x` is not length 1."),
        pass = list(i = "`x` is numeric.", i = "`x` is length 1."),
        # Retrieve the pass/fail results from the test assignments to `t1`, ..., `tn`
        fail_at = c(!rlang::env_get(nm = "t1", default = TRUE), !rlang::env_get(nm = "t2", default = TRUE)),
        pass_at = c(rlang::env_get(nm = "t1", default = FALSE), rlang::env_get(nm = "t2", default = FALSE))
      ),

      # `x` must be a character vector
      x = "`x` is not a character vector."
    ),
    # TODO: Think about what we'd need to know to get `failed` working. Every single
    # `info` segment (i.e. each discrete test) will probably need an assignment to `tk <- `.
    failed = rlang::env_get(nm = "t3", default = TRUE) && rlang::env_get(nm = "t4", default = TRUE)
  ) |> cli::cli_inform()

  # How could we re-construct the tests, info, headers, etc. from the information
  # provided in the test and in the `and_info/or_info` block?

  # 1.  We know that every discrete test that was provided will get an assignment
  #     to some `tk <- `.
  #
  # 2. We can dictate the structure of the `and_info` and `or_info` to facilitate this.

  # Input Test: `(p1 && p2) || p3`
  # Output Test: `(t3 <- ((t1 <- p1) && (t2 <- p2))) || (t4 <- p3))`

  # This is nice. Every single input to a `and()` or `or()` function gets assigned
  # to a single `t*`. Even the nested ones. So here, we have:
  #
  # Input: `or(and(p1, p2), p3)`
  # 1. Starting from the inside `and()`:
  # - we assign each input statement `p*` to a result `t*`: `t1 <- p1`, `t2 <- p2`,
  # - we combine each assign/input using the correct operation: `(t1 <- p1) && (t2 <- p2)`
  #
  # 2. Moving to the outside `or()`:
  # - assign each input: `t3 <- (t1 <- p1) && (t2 <- p2)`, `t4 <- p3`
  # - combine the input with the or operator: `((t3 <- (t1 <- p1) && (t2 <- p2)) || (t4 <- p3))`
  #
  # 3. TODO: This algorithm should be fairly straightforward to reverse!

  # Output Test: ((t3 <- (t1 <- p1) && (t2 <- p2)) || (t4 <- p3))
  #
  # 1. Get the call:
  # - first call is `||`, so we know we're in an `or()`
  # 2. Extract the individual tests:
  # - lhs `(t3 <- ((t1 <- p1) && (t2 <- p2)))`
  # - rhs `(t4 <- p3)`
  # 3. The test is whatever value is being assigned
  # - lhs test is `((t1 <- p1) && (t2 <- p2))`
  # - rhs test is `p3`
  #
  # 4. If you're left with no assignments to `t* <- `, then you have the test.
  #    Otherwise, repeat steps 1-3.
  # - rhs has no assignment, so test is just `p3`
  # - lhs has assignments `t1` and `t2`, so we need to repeat
  #
  # Inner Output Test: (t1 <- p1) && (t2 <- p2)
  # 1. Get the call, `&&`, so we're in `and()`
  # 2. Extract the tests, lhs `t1 <- p1`, rhs `t2 <- p2`
  # 3. The test is whatever value is being assigned to: `p1`, `p2`
  # 4. We're left with no assignments, so we can finish

  # Trial Run --------------------------------------------------------------------

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

  # Helpers to re-produce a test from expression components
  single_test <- function(expr) expr

  and_single_test <- function(...) expr_and(..., .double = FALSE)

  and_double_test <- function(...) expr_and(..., .double = TRUE)

  or_test <- function(...) expr_or(..., .double = TRUE)

  # Call predicates
  is_assignment <- function(expr) {
    rlang::is_call(expr) && identical(expr[[1]], quote(`<-`))
  }

  get_assignment_arg <- function(expr, arg = c("x", "value")) {
    stopifnot(is_assignment(expr))
    rlang::arg_match0(arg, c("x", "value"))
    if (arg == "x") expr[[2]] else expr[[3]]
  }

  # Note, `|` is intentionally left out here
  is_logical_operator <- function(expr) {
    rlang::is_symbol(expr) &&
      (
        identical(expr, quote(`&&`)) ||
          identical(expr, quote(`&`)) ||
          identical(expr, quote(`||`))
      )
  }

  # Return the type of an expression as a string
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

  # Return symbols in an expression which match a `pattern` or are in `syms`
  find_syms <- function(expr, pattern, syms) {

    # TODO: Add some nicer error messages
    stopifnot(rlang::is_expression(expr))
    rlang::check_exclusive(pattern, syms, .require = FALSE)
    if (!missing(pattern)) stopifnot(rlang::is_string(pattern))
    if (!missing(syms)) stopifnot(is.character(syms), !anyNA(syms))

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

  # Test whether an expression contains any symbols which match a `pattern` or are
  # in `syms`.
  contains_any_syms <- function(expr, pattern, syms) {
    length(find_syms(expr, pattern, syms)) > 0
  }

  # Remove outer braces `(` from an expression. E.g. `(((x)))` becomes `x`
  remove_expr_braces <- function(expr) {
    new_expr <- expr
    while (rlang::is_call(new_expr) && identical(new_expr[[1]], quote(`(`))) {
      new_expr <- new_expr[[2]]
    }
    new_expr
  }

  # Lets do it for real. Note, the only guarantee is that the input test expressions
  # don't contain any symbols matching the pattern "^\\.t[0-9]+\\.$" (e.g. `.t1.`)
  test <- rlang::expr((.t3. <- ((.t1. <- p1) && (.t2. <- p2))) || (.t4. <- p3))

  # Grab the test symbols (i.e. `t1`, `t2`, ..., `tn`)
  test_symbols <- find_syms(test, pattern = "^\\.t[0-9]+\\.$")
  test_symbol_names <- purrr::map_chr(test_symbols, rlang::as_string)

  # If there's only one test symbol (e.g. test is `t1 <- is.numeric(x) && all(x > 10)`)
  # then we're not in an `or()` or `and()` test.
  if (length(test_symbols) == 1) {
    print("Single test, no `and()` or `or()`.")
  }

  # Grab the call, may be any of `||`, `&&`, `&`
  call <- test[[1]]

  is_and_double <- identical(call, quote(`&&`))
  is_and_single <- identical(call, quote(`&`))
  is_or <- identical(call, quote(`||`))
  if (!is_and_double && !is_and_single && !is_or) stop("`call` is unexpected.")

  # Identify whether the `lhs`, `rhs` are single items or multiple items
  lhs <- remove_expr_braces(test[[2]])
  rhs <- remove_expr_braces(test[[3]])

  # Both lhs and rhs should be an assignment to a test symbol
  if (!identical(lhs[[1]], quote(`<-`)) || !identical(rhs[[1]], quote(`<-`))) {
    stop("`lhs` or `rhs` is not an assignment.")
  }

  # Grab the test symbol (what is being assigned to) and the test (what is being assigned)
  lhs_test_symbol <- lhs[[2]]
  rhs_test_symbol <- rhs[[2]]

  lhs_test <- lhs[[3]]
  rhs_test <- rhs[[3]]

  # Repeat this process on the `lhs_test` and `rhs_test`...

  # FULL FUNCTION

  # Okay, so we need to think about this a little differently. We know that each "level"
  # of the test has one set of assignments to `.t*.` variables. For example:
  #
  # and(p1, p2, p3) -> (.t1. <- p1) && (.t2. <- p2) && (.t3. <- p3)
  #
  # Whenever we have a nested `and()` or `or()`, there is an inner assignment
  # of more `.t*` variables.
  #
  # and(p1, p2, or(p3, p4)) -> (.t1. <- p1) && (.t2. <- p2) && (.t3. <- (.t4. <- p3) || (.t5. <- p4))
  #
  # So, we know we're at a single level of an `and()` or an `or()` so long as the
  # outermost calls are to `&&`, `&`, or `|`. Until we hit an assignment `<-`,
  # we're within the same level.

  test_expr <- quote((.t1. <- p1) && (.t2. <- p2) && (.t3. <- (.t4. <- p3) || (.t5. <- p4)))

  # Note, precedence goes from left to right. So the second `&&` is the first call we hit.
  test_expr[[2]]
  test_expr[[3]]

  # The outer call
  test <- test_expr
  call <- test[[1]]

  # Capture the tests and calls at this level of the `and()` or `or()` block
  current_tests <- list()
  current_calls <- list()

  while (is_logical_operator(call)) {
    # The right-hand-side of the call (`test[[3]]`) is the last test within an `and()`
    # or `or()` block. E.g. If we're in an `and()` with `test <- e1 && e2 && e3`, then
    # `test[[3]]` is the single `e3`, while `test[[2]]` contains the remaining `&&`
    # call `e1 && e2`. We:
    # - add `e3` to the current tests (i.e. those in the `and()` block)
    # - set the test to `e1 && e2`, repeat until we've exited the `and()` block
    current_tests <- append(current_tests, remove_expr_braces(test[[3]]))
    current_calls <- append(current_calls, call)
    test <- remove_expr_braces(test[[2]])
    call <- test[[1]]
  }
  # The left-over `test` is the last of the current tests
  # TODO: Can I have this happen within the `while`?
  current_tests <- append(current_tests, test)

  current_tests
  current_calls

  # Note, each of the current tests is of the form `.t*. <- (test)`. If `test`
  # contains additional `.t*.` symbols, then it contains more inner tests (i.e.
  # the `test` is an `and()` or `or()` block itself). Otherwise, that is the final
  # test expression which was input by the user.
  inner_tests <- purrr::map(current_tests, get_assignment_arg, "value")
  is_and_or_test <- purrr::map_lgl(inner_tests, contains_any_syms, pattern = "^\\.t.*\\.$")

  and_or_tests <- inner_tests[is_and_or_test]  # Recursive cases
  single_tests <- inner_tests[!is_and_or_test] # Base cases

  rlang::call2(
    .fn = "binary_op",
    exprs =
  )




  recreate_test_expr <- function(test_expr) {

    # The outer call
    test <- test_expr
    call <- test[[1]]

    # Capture the tests and calls at this level of the `and()` or `or()` block
    current_tests <- list()
    current_calls <- list()

    while (is_logical_operator(call)) {
      # TODO: Add better documentation here AND make these names more clear. This
      # is a fairly confusing process.
      current_tests <- append(current_tests, remove_expr_braces(test[[3]]))
      current_calls <- append(current_calls, call)
      test <- remove_expr_braces(test[[2]])
      call <- test[[1]]
    }
    # The left-over `test` is the last of the current tests
    # TODO: Can I have this happen within the `while`?
    current_tests <- append(current_tests, test)

    # Reverse the order of the tests and calls. We've captured them in the reverse
    # order that we want them to be applied.
    current_tests <- rev(current_tests)
    current_calls <- rev(current_calls)

    # Note, each of the current tests is of the form `.t*. <- (test)`. If `test`
    # contains additional `.t*.` symbols, then it contains more inner tests (i.e.
    # the `test` is an `and()` or `or()` block itself). Otherwise, that is the final
    # test expression which was input by the user.
    #
    # The tests containing `.t*.` symbols are the recursive cases and the tests
    # not containing `.t*.` symbols are the base cases.
    current_tests <- purrr::map(current_tests, get_assignment_arg, "value")
    test_is_and_or <- purrr::map_lgl(current_tests, contains_any_syms, pattern = "^\\.t.*\\.$")

    # Base Case: These are the standalone tests supplied by the user. Inject these
    # into the output expression as quoted expressions, so they'll be preserved.
    current_tests <- purrr::modify_if(
      .x = current_tests,
      .p = !test_is_and_or,
      .f = \(x) rlang::expr(quote(!!x))
    )
    # Recursive Case:
    current_tests <- purrr::modify_if(
      .x = current_tests,
      .p = test_is_and_or,
      .f = recreate_test_expr
    )

    # Generate the test generating call
    rlang::call2(
      .fn = "binary_call",
      # Recursively create the test call for any tests which are themselves `and()`
      # or `or()` tests. For this to be evaluated properly, we want `exprs` to be
      # a call to list with `current_tests` as it's arguments - NOT the list
      # `current_tests` itself.
      exprs = rlang::expr(list(!!!current_tests)),
      op_names = purrr::map_chr(current_calls, rlang::as_string)
    )
  }

  print(test_expr)
  recreate_test_expr(test_expr)
  recreate_test_expr(test_expr) |> eval()

  # Now we can combine extracted tests easily.
  and_test <- recreate_test_expr(quote((.t1. <- p1) && (.t2. <- p2)))
  or_test <- recreate_test_expr(quote((.t1. <- p3) || (.t2. <- p4)))

  # Let's "and" these together in a new binary call
  combined_call <- rlang::call2(
    .fn = "binary_call",
    exprs = rlang::expr(list(!!and_test, !!or_test)),
    op_names = list("&&")
  )
  combined_call
  eval(combined_call)

  package_test_expr <- function(test_expr) {

    # Capture the tests and calls at this level of the `and()` or `or()` block
    test <- test_expr
    call <- test[[1]]
    current_tests <- list()
    current_calls <- list()

    while (is_logical_operator(call)) {
      current_tests <- append(current_tests, remove_expr_braces(test[[3]]))
      current_calls <- append(current_calls, call)
      test <- remove_expr_braces(test[[2]])
      call <- test[[1]]
    }
    current_tests <- rev(append(current_tests, test))
    current_calls <- purrr::map_chr(rev(current_calls), rlang::as_string)

    # All tests are of the form `.t*. <- test`, but we want just the `test` expression.
    current_tests <- purrr::map(current_tests, get_assignment_arg, "value")

    # Recursive Case: These tests are `and()` or `or()` blocks.
    current_tests <- purrr::modify_if(
      .x = current_tests,
      .p = purrr::map_lgl(current_tests, contains_any_syms, pattern = "^\\.t.*\\.$"),
      .f = package_test_expr
    )

    # Return the expressions and their operators (i.e. `&&`, `&`, or `||`)
    return(new_blueprint(exprs = current_tests, ops = current_calls))
  }

  package_test_expr(test_expr) |> str()

  # NOTE:
  # Let's make a thing called a `blueprint`, which is the instruction set to create
  # a `test`.

  the <- rlang::new_environment(data = list(t_index = 0))

  new_blueprint <- function(exprs, ops) {
    stopifnot(
      "`exprs` must be a list of expressions or <blueprint> objects." = is.list(exprs),
      "`ops` must be a character vector." = is.character(ops),
      "Length of `ops` must be one less than that of `exprs`." = length(exprs) - 1 == length(ops)
    )
    structure(list(exprs = exprs, ops = ops), class = "blueprint")
  }

  is_blueprint <- function(x) inherits(x, "blueprint")

  new_test <- function(blueprint) {
    stopifnot("`blueprint` must be class <blueprint>." = is_blueprint(blueprint))

    # Every expression in the blueprint is some `test`. We want to modify the test
    # expression to `.t*. <- test`, so we can use the `t_symbol` (i.e. `.t*.`) to
    # record the test result.
    for (i in seq_along(blueprint$exprs)) {
      current_expr <- blueprint$exprs[[i]]

      # Increment `the$t_index` each iteration, so that we never have an overlap
      t_symbol <- t_sym(the$t_index)
      the$t_index <- the$t_index + 1

      # If the expression is itself a blueprint, make it a test prior to assigning
      if (is_blueprint(current_expr)) {
        blueprint$exprs[[i]] <- rlang::expr(!!t_symbol <- !!new_test(current_expr))
      } else {
        blueprint$exprs[[i]] <- rlang::expr(!!t_symbol <- !!current_expr)
      }
    }

    # Collapse the test into an expression `(.t1. <- test1) op ... op (.tn. <- testn)`
    binary_call(blueprint$exprs, blueprint$ops)
  }

  test_expr <- quote(
    (.t1. <- p1) && (.t2. <- (.t3. <- p3) || (.t4. <- p4)) && (.t5. <- (.t6. <- p5) || (.t7. <- p6))
  )

  new_test(package_test_expr(test_expr))

  # NOTE: We really need to package together the test and the information components
  # of the info blueprint, since they're so tied together.

  # info_blueprint
  # - test:
  #   - exprs: the discrete expressions provided by the user (list of expr), length-n
  #   - ops:   the logical operations, i.e. `&&`, `&`, `|` which combine the exprs (character), length-n - 1
  # - messaging:
  #   - header: the header for the block (character), any length
  #   - fail:   one set of bullets per `exprs` (list of character), length-n
  #   - pass:   one set of bullets per `exprs` (list of character), length-n

  new_info_components <- function(info_blueprint) {

    # Make the test and messaging sections from the info_blueprint

  }

  # p1 && p2 && (p11 || p12 || (p21 && p22))
  and_info(
    # Note: Only one header - at the top level
    header = header,
    fail = list(
      fail_1,
      fail_2,
      or_info(
        fail = list(
          fail_11,
          fail_12,
          and_info(
            fail = list(
              fail_21,
              fail_22
            ),
            # We only bother with `and()` passes when nested within an `or()`
            pass = list(
              pass_21,
              pass_22
            ),
            fail_at = get_fail(.t7., .t8.)
          )
        ),
        # Note: There are cases where we might want to emit a pass of an `or()`
        pass = list(
          pass_11,
          pass_12,
          pass_13
        ),
        # Note, `or()` only fails if all results failed
        fail_at = get_fail(.t4., .t5., .t6.),
        pass_at = get_pass(.t4., .t5., .t6.)
      )
    ),
    pass = list(
      pass_1,
      pass_2,
      pass_3
    ),
    fail_at = get_fail(.t1., .t2., .t3.),
    pass_at = get_pass(.t1., .t2., .t3.)
  )

  # p1 && p2 && (p11 || p12 || (p21 && p22))
  #
  # Suppose p1 fails:
  # ! header
  # x fail_1

  # Suppose p2 fails:
  # ! header
  # x fail_2

  # Suppose p21 fails (this means that p11, p12 also failed)
  # ! header
  # x fail_11
  # x fail_12
  # i pass_21
  # x fail_22

  and_info <- function(fail, pass, fail_at, pass_at, header = character()) {
    c(
      header,
      flatten_lst_of_chrish(fail[fail_at]),
      flatten_lst_of_chrish(pass[pass_at])
    )
  }

  or_info <- function(fail, pass, fail_at, pass_at, header = character()) {
    c(
      header,
      flatten_lst_of_chrish(fail[fail_at]),
      flatten_lst_of_chrish(pass[pass_at])
    )
  }

  new_info_blueprint <- function(exprs, ops, header, fail, pass) {
    info_blueprint <- structure(
      list(
        exprs = exprs,
        ops = ops,
        header = header,
        fail = fail,
        pass = pass
      ),
      class = "info_blueprint"
    )
    validate_info_blueprint(info_blueprint)
  }

  validate_info_blueprint <- function(x) {
    n_exprs <- length(x$exprs)
    stopifnot(
      is.list(x$exprs) & purrr::map_lgl(x$exprs, rlang::is_expression),
      is.character(x$ops),
      is.character(x$header),
      is.list(x$fail) & purrr::map_lgl(x$fail, is.character),
      is.list(x$pass) & purrr::map_lgl(x$pass, is.character)
    )
    stopifnot(
      n_exprs == length(x$ops) - 1,
      n_exprs == length(x$fail),
      n_exprs == length(x$pass)
    )
    x
  }

  f <- function(x) {

    t_index <- the$t_index
    on.exit(the$t_index <- t_index)

    for (i in seq_along(x)) {
      t_index <- t_index + 1
    }
    x

  }

  pak <- package_test_expr(test_expr)

  t_sym <- function(index) rlang::sym(paste0(".t", index, "."))

  # This is like we had `and_test <- and(p1, p2)` and `or_test <- or(p3, p4)` and
  # we've just done: `and(and_test, or_test)`.

  # We then recursively break down those inner tests to make our final expression.
  # This would end up like:
  #
  # `and_test(!!!map(current_tests, do_it_recursively))`

  binary_call <- function(exprs, op_names) {
    make_binary_call <- function(lhs, rhs, op) rlang::call2(.fn = op, lhs, rhs)
    purrr::reduce2(exprs, op_names, make_binary_call)
  }

  test_expr

  recreate_test_expr <- function(test_expr) {

    # Remove any excess outer calls to `(`
    test_expr <- remove_expr_braces(test_expr)
    print(test_expr)

    # Grab the test symbols (i.e. `t1`, `t2`, ..., `tn`)
    test_symbols <- find_syms(test_expr, pattern = "^\\.t[0-9]+\\.$")
    test_symbol_names <- purrr::map_chr(test_symbols, rlang::as_string)

    # Base Cases:
    # If there's only one test symbol, the test is not an `or()` or `and()` test.
    # It'll be something like `.t1. <- is.numeric(x)`, from which we can return
    # the test expression `is.numeric(x)`.
    #
    # If there's no test symbols, then we've recursively hit the test itself and
    # we can just return the test expression.
    if (length(test_symbols) == 1) {
      stopifnot(is_assignment(test_expr))
      return(rlang::expr(quote(!!test_expr[[3]])))
    }

    # Grab the call, may be any of `||`, `&&`, `&`
    call <- test_expr[[1]]
    is_and_double <- identical(call, quote(`&&`))
    is_and_single <- identical(call, quote(`&`))
    is_or <- identical(call, quote(`||`))
    if (!is_and_double && !is_and_single && !is_or) stop("`call` is unexpected.")

    # Get the left-hand-side and right-hand-side of the test.
    lhs <- remove_expr_braces(test_expr[[2]])
    rhs <- remove_expr_braces(test_expr[[3]])

    print(lhs)
    print(rhs)

    ## Recursive Case:

    # Get the outside call, which is either `&&`/`&` or `||`, from a test
    # constructed with `and()` or `or()` respectively.
    outer_call <- if (is_and_double) {
      "and_double_test"
    } else if (is_and_single) {
      "and_single_test"
    } else if (is_or) {
      "or_test"
    }

    # Generate the nested `&&`, `&`, `||` tests recursively.
    # TODO: Add a namespace here.
    rlang::call2(
      .fn = outer_call,
      recreate_test_expr(lhs),
      recreate_test_expr(rhs)
    )
  }
}
