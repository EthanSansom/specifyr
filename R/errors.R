# stop -------------------------------------------------------------------------
stop_vector_mistyped <- function(
    x,
    x_name,
    type_desc,
    type_test,
    x_must = NULL,
    len = NULL,
    nas = NULL,
    null = NULL,
    min_len = NULL,
    max_len = NULL,
    lower = NULL,
    upper = NULL,
    finite = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
) {

  # This will also catch if `x` was NULL but shouldn't have been.
  if (!(type_test)) {
    stop_not_is_object(
      x = x,
      x_name = x_name,
      x_must = x_must,
      obj_desc = type_desc,
      null = null,
      error_call = error_call,
      error_class = error_class
    )
  }

  if (is.null(x_must)) {
    x_must <- paste("be", a_vector_friendly(
        type_desc = type_desc,
        len = len,
        nas = nas,
        null = null,
        min_len = min_len,
        max_len = max_len,
        lower = lower,
        upper = upper,
        finite = finite
    ))
  }
  header <- paste0("{.arg {x_name}} must ", x_must, ".")
  bullets <- vector_mistyped_bullets(
    x = x,
    len = len,
    nas = nas,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper,
    finite = finite
  )

  cli::cli_abort(
    c(header, bullets),
    call = error_call,
    class = c(error_class, "specifyr_error")
  )
}

stop_not_is_object <- function(
    x,
    x_name,
    obj_desc,
    x_must = NULL,
    null = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
  ) {
  if (is.null(x_must)) {
    null_or <- if (isTRUE(null)) "`NULL` or " else ""
    x_must <- paste0("be ", null_or, paste(obj_desc, collapse = " "))
  }
  cli::cli_abort(
    c(
      paste0("{.arg {x_name}} must ", x_must, "."),
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
    ),
    call = error_call,
    class = c(error_class, "specifyr_error")
  )
}

stop_not_integerish <- function(
    x,
    x_name,
    x_must = NULL,
    len = NULL,
    nas = NULL,
    null = NULL,
    min_len = NULL,
    max_len = NULL,
    lower = NULL,
    upper = NULL,
    finite = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
  ) {

  x_must <- x_must %||% paste("be", a_vector_friendly(
    type_desc = "vector coercible to an {.cls integer} without loss of precision",
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper,
    finite = finite
  ))

  # If the object is integer-ish, that means one of it's other characteristics
  # are wrong (ex. `length(x) != len`).
  if (checkmate::test_integerish(x)) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      x_must = x_must,
      type_test = \(x) TRUE,
      type_desc = c("an", "integerish"),
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      lower = lower,
      upper = upper,
      finite = finite,
      error_call = rlang::caller_env(),
      error_class = "specifyr_error_mistyped"
    )
  }

  header <- paste0("{.arg {x_name}} must ", x_must, ".")
  if (is.numeric(x) || is.complex(x)) {
    cli::cli_abort(
      c(
        header,
        x = paste(
          "{.arg {x_name}} is a {.cls {class(x)}} vector",
          "not coercible to an {.cls integer} without loss of precision."
        )
      ),
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }
  cli::cli_abort(
    c(
      header,
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
    ),
    call = error_call,
    class = c(error_class, "specifyr_error")
  )
}

stop_arg_missing <- function(
    x,
    x_name,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
  ) {
  cli::cli_abort(
    "{.arg {x_name}} is absent but must be supplied.",
    call = error_call,
    class = c(error_class, "specifyr_error")
  )
}

vector_mistyped_bullets <- function(
    x,
    len = NULL,
    nas = NULL,
    min_len = NULL,
    max_len = NULL,
    lower = NULL,
    upper = NULL,
    finite = NULL
  ) {

  x_len <- length(x)
  len_bullet <- if (isTRUE(x_len < (min_len %||% len) || (max_len %||% len) < x_len)) {
    paste0("{.arg {x_name}} is ", length_n_friendly(x_len), ".")
  }

  x_nas <- is.na(x)
  nas_bullet <- if (isFALSE(nas) && any(x_nas)) {
    paste0("{.arg {x_name}} is NA or NaN ", at_loc_friendly(x_nas), ".")
  }

  skip_min_max <- all(x_nas) || x_len == 0
  x_min <- if (!(skip_min_max || is.null(lower))) min(x, na.rm = TRUE)
  lower_bullet <- if (isTRUE(x_min < lower)) {
    paste0("{.arg {x_name}} has a minimum of ", x_min, ".")
  }
  x_max <- if (!(skip_min_max || is.null(upper))) max(x, na.rm = TRUE)
  upper_bullet <- if (isTRUE(upper < x_max)) {
    paste0("{.arg {x_name}} has a maximum of ", x_max, ".")
  }

  x_inf <- if (!is.null(finite)) is.infinite(x)
  finite_bullet <- if (isTRUE(finite) && any(x_inf)) {
    paste0("{.arg {x_name}} is an infinite value ", at_location_friendly(x_inf), ".")
  }

  c(
    x = len_bullet,
    x = nas_bullet,
    x = lower_bullet,
    x = upper_bullet,
    x = finite_bullet
  )
}

test_type <- function(x, x_type) {
  test_fn <- switch(
    x_type,
    integer = is.integer,
    character = is.character,
    numeric = is.numeric,
    double = is.double,
    logical = is.logical,
    complex = is.complex,
    raw = is.raw,
    list = is.list,
    any = \(x) TRUE
  )
  test_fn(x)
}

# check ------------------------------------------------------------------------

check_is_list_of_check <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_input"
) {
  if (!is.list(x)) {
    check <- check_is_check(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
    return(list(check))
  }
  .mapply(
    check_is_check,
    dots = list(
      x = x,
      x_name = paste0(x_name, "[[", seq_along(x), "]]")
    ),
    MoreArgs = list(
      error_call = error_call,
      error_class = error_class
    )
  )
}

check_is_check <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_input"
  ) {

  if (is_check(x)) {
    return(x)
  }
  stop_not_is_object(
    x = x,
    x_name = x_name,
    obj_desc = c("a", "{.cls specifyr_check}"),
    error_call = error_call,
    error_class = error_class
  )
}

check_vector_type_args <- function(
    ...,
    .error_call = rlang::caller_env(),
    .error_class = "specifyr_error_input"
) {

  args <- rlang::list2(...)

  arg_checks <- list(
    cls = \(x) x %!|% chr(x, x_name = "cls", min_len = 1, nas = FALSE, error_call = .error_call, error_class = .error_class),
    len = \(x) x %!|% count(x, x_name = "len", error_call = .error_call, error_class = .error_class),
    nas = \(x) bool(x, x_name = "nas", error_call = .error_call, error_class = .error_class),
    null = \(x) bool(x, x_name = "null", error_call = .error_call, error_class = .error_class),
    min_len = \(x) x %!|% count(x, x_name = "min_len", error_call = .error_call, error_class = .error_class),
    max_len = \(x) x %!|% count(x, x_name = "max_len", error_call = .error_call, error_class = .error_class),
    lower = \(x) vec(x, x_name = "lower", len = 1, nas = FALSE, error_call = .error_call, error_class = .error_class),
    upper = \(x) vec(x, x_name = "upper", len = 1, nas = FALSE, error_call = .error_call, error_class = .error_class),
    finite = \(x) bool(x, x_name = "finite", error_call = .error_call, error_class = .error_class)
  )
  arg_checks$lst_len <- arg_checks$len
  arg_checks$lst_min_len <- arg_checks$min_len
  arg_checks$lst_max_len <- arg_checks$max_len
  arg_checks$lst_null <- arg_checks$null

  arg_nms <- rlang::names2(args)
  if (!all(arg_nms %in% names(arg_checks))) {
    bad_nms <- setdiff(arg_nms, names(arg_checks))
    cli::cli_abort(
      "{.arg ...} contains {length(bad_nms)} invalid name{?s}: {.val {bad_nms}}.",
      .internal = TRUE
    )
  }

  args <- .mapply(
    FUN = \(arg, arg_check) arg_check(arg),
    dots = list(
      arg = args,
      arg_check = arg_checks[arg_nms]
    ),
    MoreArgs = list()
  )
  names(args) <- arg_nms

  if (all(c("lower", "upper") %in% arg_nms)) {
    check_valid_bounds(
      lower, upper, strict = TRUE,
      error_call = .error_call,
      error_class = .error_class
    )
  }
  if (all(c("min_len", "max_len") %in% arg_nms)) {
    check_valid_bounds(
      min_len, max_len,
      error_call = .error_call,
      error_class = .error_class
    )
  }
  if (all(c("lst_min_len", "lst_max_len") %in% arg_nms)) {
    check_valid_bounds(
      lst_min_len, lst_max_len,
      error_call = .error_call,
      error_class = .error_class
    )
  }

  args
}

check_valid_bounds <- function(
    lower,
    upper,
    strict = FALSE,
    lower_name = rlang::caller_arg(lower),
    upper_name = rlang::caller_arg(upper),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_input"
) {

  compare <- if (strict) `>` else `>=`
  if (try_is_true(compare(lower, upper))) {
    be <- if (strict) "be strictly" else "be"
    cli::cli_abort(
      c(
        paste("{.arg {lower_name}} must", be, "less than {.arg {upper_name}}."),
        i = "{.arg {lower_name}} is {.val {lower}}.",
        i = "{.arg {upper_name}} is {.val {upper}}."
      ),
      call = error_call,
      class = c(error_class, "specifyr_error")
    )
  }
}

check_vector_type_arg_nms <- function(
    x,
    x_name = rlang::caller_arg(x),
    x_is_names = FALSE,
    error_message = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_input"
) {

  vector_type_args <- c(
    "cls",
    "len",
    "nas",
    "null",
    "min_len",
    "max_len",
    "lower",
    "upper",
    "finite"
  )
  x_names <- if (x_is_names) x else rlang::names2(x)
  if (all(x_names %in% vector_type_args)) {
    return(x)
  }
  invalid_arg_nms <- x_names %notin% vector_type_args
  error_message <- error_message %||% c(
    "Element's of {.arg {x_name}} must have names in {.val {vector_type_args}}.",
    x = "{.arg {x_name}} has unrecognized names {at_loc_friendly(invalid_arg_nms)}.",
    x = "{.arg {x_name}} unrecognized names: {.val {x_names[invalid_arg_nms]}}."
  )
  cli::cli_abort(
    error_message,
    call = error_call,
    class = c(error_class, "specifyr_error")
  )
}

# REMOVE -----------------------------------------------------------------------

# Everything below this point is old code, keeping for backwards compatibilty.
# Once everything has been re-worked, delete this.

# user -------------------------------------------------------------------------

# TODO Ethan: Improve this function. Look at some other packages for how you
# might want to standardize your errors in a better way. Post debugging, you'll
# probably want to strip down the internal errors as well.
stop_must <- function(
    must,
    info = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
) {
  cli::cli_abort(
    c(must, x = info),
    call = error_call,
    class = error_class
    # .envir = rlang::caller_env()
  )
}

specifyr_error <- function(
    ...,
    .error_call = rlang::caller_env(),
    .error_class = "specifyr_api_error"
) {
  cli::cli_abort(
    c(...),
    call = .error_call,
    class = .error_class,
    .envir = rlang::caller_env()
  )
}

# check ------------------------------------------------------------------------

check_is_string <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
    ) {

  if (!rlang::is_string(x)) {
    x_is <- if (is.character(x)) {
      x_len <- length(x)
      if (x_len == 1) {
        "NA."
      } else {
        "length {x_len} character."
      }
    } else {
      "{.obj_type_friendly {x}}."
    }
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a non-NA length-1 character.",
        x = paste("{.arg {x_name}} is", x_is)
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_bool <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
  ) {

  if (!rlang::is_bool(x)) {
    x_is <- if (is.logical(x)) {
      x_len <- length(x)
      if (x_len == 1) {
        "NA."
      } else {
        "length {x_len} logical vector."
      }
    } else {
      "{.obj_type_friendly {x}}."
    }
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a `TRUE` or `FALSE` value.",
        x = paste("{.arg {x_name}} is", x_is)
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_env <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
) {

  if (!is.environment(x)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be an environment.",
        x = paste("{.arg {x_name}} is {.obj_type_friendly {x}}.")
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_list <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
) {

  if (!is.list(x)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a list.",
        x = paste("{.arg {x_name}} is {.obj_type_friendly {x}}.")
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_spec <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
  ) {
   if (!is_object_spec(x)) {
     cli::cli_abort(
       c(
         "{.arg {x_name}} must be a {.cls obj_spec} object.",
         x = paste("{.arg {x_name}} is {.obj_type_friendly {x}}.")
       ),
       call = error_call,
       class = error_class
     )
   }
  x
}

check_is_check <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
  ) {

  if (!is_check(x)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a {.cls specifyr_check}.",
        x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
      ),
      call = error_call,
      class = error_class
    )
  }
  x
}

check_is_len <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
  ) {

  if (is.null(x)) {
    return(x)
  }

  if (rlang::is_integerish(x)) {
    x_len <- length(x)
    x_nas <- is.na(x)
    len_msg <- if (x_len %notin% 1:2) "{.arg {x_name}} is length {x_len}."
    nas_msg <- if (any(x_nas)) "{.arg {x_name}} is NA {at_locations(x_nas)}."
    if (is.null(len_msg) && is.null(nas_msg)) {
      return(x)
    }
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a non-NA integerish vector of length 1 or 2.",
        x = len_msg,
        x = nas_msg
      ),
      call = error_call,
      class = error_class
    )
  }
  if (is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a non-NA integerish vector of length 1 or 2.",
        i = "{.arg {x_name}} is a {.cls numeric} vector.",
        x = "Can't coerce {.arg {x_name}} to an integer without loss of precision."
      ),
      call = error_call,
      class = error_class
    )
  }

  cli::cli_abort(
    c(
      "{.arg {x_name}} must be a non-NA integerish vector of length 1 or 2.",
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
    ),
    call = error_call,
    class = error_class
  )
}

# TODO Ethan: Borrow a little from this error formatting for the `list_of_spec` messages!
check_is_cls <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_api_error"
) {

  error_bullets <- function(x, x_name) {
    x_len <- length(x)
    x_nas <- is.na(x)
    len_msg <- if (x_len == 0) paste0("{.arg ", x_name, "} is an empty {.cls character} vector.")
    nas_msg <- if (any(x_nas)) paste0("{.arg ", x_name, "} is a {.cls character} which is NA ", at_locations(x_nas), ".")
    c(x = len_msg, x = nas_msg)
  }

  if (is.character(x)) {
    error_bullets <- error_bullets(x, x_name)
    if (is.null(error_bullets)) {
      return(x)
    }
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a (list of) non-NA non-empty {.cls character} vector(s).",
        error_bullets
      ),
      call = error_call,
      class = error_class
    )
  }

  if (is.list(x)) {
    if (length(x) == 0) {
      cli::cli_abort(
        c(
          "{.arg {x_name}} must be a (list of) non-NA non-empty {.cls character} vector(s).",
          x = "{.arg {x_name}} is an empty list."
        ),
        call = error_call,
        class = error_class
      )
    }

    element_is_character <- vapply(x, is.character, logical(1L))
    if (!all(element_is_character)) {
      n_errors <- sum(!element_is_character)
      non_chr_at <- which(!element_is_character)[seq(min(n_errors, 3))]
      and_more_msg <- if (n_errors > 3) paste("... and", n_errors - 3, "more problems.")
      bullets <- paste0("{.arg ", x_name, "[[", non_chr_at, "]]} is {.obj_type_friendly {x[[", non_chr_at, "]]}}.")

      cli::cli_abort(
        c(
          "{.arg {x_name}} must be a (list of) non-NA non-empty {.cls character} vector(s).",
          i = "{.arg {x_name}} is a list.",
          rlang::set_names(bullets, "x"),
          and_more_msg
        ),
        call = error_call,
        class = error_class
      )
    }

    element_error_bullets <- mapply(
      x = x,
      x_name = paste0(x_name, "[[", seq_along(x), "]]"),
      FUN = error_bullets,
      SIMPLIFY = FALSE
    )
    element_is_cls <- vapply(element_error_bullets, is.null, logical(1L))
    if (all(element_is_cls)) {
      return(x)
    }

    non_cls_at <- which(!element_is_cls)
    n_errors <- length(non_cls_at)
    and_more_msg <- if (n_errors > 3) paste("... and", n_errors - 3, "more problems.")

    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a (list of) non-NA non-empty {.cls character} vector(s).",
        i = "{.arg {x_name}} is a list.",
        rlang::set_names(as.character(element_error_bullets[non_cls_at]), "x"),
        and_more_msg
      ),
      call = error_call,
      class = error_class
    )
  }

  cli::cli_abort(
    c(
      "{.arg {x_name}} must be a (list of) non-NA non-empty {.cls character} vector(s).",
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
    ),
    call = error_call,
    class = error_class
  )
}

# internal ---------------------------------------------------------------------

# This is inspired by `assertthat` (https://github.com/hadley/assertthat). While
# it feels a little illegal, does make for some more informative internal errors
# for my benefit while keeping the call terse.

# TODO: Re-do this using the function itself!!! This is no good. I don't like having
# to pass around the function name and namespace as characters.

specifyr_internal_error <- function(
    x,
    fn_name,
    fn_ns = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {
  # `quote(arg)` prevents a confusing chain of events: If `x = sym("y")` then
  # `call2(is.symbol, x)` produces `is.symbol(y)` instead of `is.symbol(x)`.
  test_call <- rlang::call2(fn_name, quote(x), .ns = fn_ns)
  if (!isTRUE(eval(test_call))) {
    cli::cli_abort(
      specifyr_internal_error_message(arg_name, fn_name),
      call = error_call,
      .internal = TRUE
    )
  }
}

specifyr_internal_error_message <- function(arg_name, fn_name) {
  expected <- switch(
    fn_name,
    # internal
    "is_object_spec" = "a {.cls specifyr_object_spec} object.",
    "is_object_blueprint" = "a {.cls specifyr_object_blueprint} object.",
    "is_check" = "a {.cls specifyr_check} object.",
    "is_check_blueprint" = "a {.cls specifyr_check_blueprint} object.",
    # base::
    "is.numeric" = "a numeric vector.",
    "is.integer" = "an integer vector.",
    "is.logical" = "a logical vector.",
    "is.character" = "a character vector.",
    "is.list" = "a list.",
    "is.symbol" = "a symbol.",
    "is.function" = "a function.",
    "is.call" = "a call.",
    # rlang::
    "is_bool" = "a single TRUE of FALSE value.",
    "is_string" = "a non-NA length-1 character.",
    "is_integerish" = "an integerish number.",
    "is_scalar_integerish" = "a length-1 integerish number.",
    "is_quosure" = "a quosure.",
    "is_expression" = "a defused expression.",
    cli::cli_abort(
      "Can't construct message for `fn_name = {.val {fn_name}}`.",
      .internal = TRUE
    )
  )
  c(
    paste0("{.arg {x_name}} must be ", expected),
    i = "{.arg {x_name}} is {.obj_type_friendly {x}}."
  )
}

specifyr_internal_error_if_not <- function(
    predicate,
    message,
    error_call = rlang::caller_env()
) {
  if (!isTRUE(predicate)) {
    cli::cli_abort(
      message,
      call = error_call,
      .internal = TRUE
    )
  }
}
