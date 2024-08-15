# todos ------------------------------------------------------------------------

# TODO:
# - See `check_property_dots` for an important todo

# stop mistyped ----------------------------------------------------------------

#' @export
stop_vector_mistyped <- function(
    x,
    x_name,
    type_desc,
    type_test,
    len = NULL,
    nas = NULL,
    null = NULL,
    min_len = NULL,
    max_len = NULL,
    lower = NULL,
    upper = NULL,
    finite = NULL,
    x_must = NULL,
    # To omit either the header or bullets, a user can supply `character(0L)`
    error_header = NULL,
    error_bullets = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped",
    # TODO: Maybe change `max_name_width` to be in terms of `cli::console_width`.
    #       Think about what  the longest possible message would be.
    max_name_width = 40,
    name_replacement = "Argument"
) {

  # TODO: It's not clear where, or if, we should raise an error for invalid
  #       arguments like `error_class`, `error_bullets`, etc. These won't
  #       effect the type check, but can cause an error here. These errors
  #       can only be realized when the type check is run, not when it is
  #       called (i.e. `int("A", error_bullets = mean)` will error here).

  if (isFALSE(null) && is.null(x) || !type_test) {
    stop_not_type(
      x = x,
      x_name = x_name,
      x_must = x_must,
      type_desc = type_desc,
      null = null,
      error_header = error_header,
      error_bullets = error_bullets,
      error_call = error_call,
      error_class = error_class,
      max_name_width = max_name_width,
      name_replacement = name_replacement
    )
  }

  # By default, a message like "be a length 1 integer vector". Used in the
  # error message header if no `error_header` is supplied.
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

  # By default, a message like "`x_name` must be a length 1 integer vector". If
  # `x_name` is a long string, we refer to `x_name` using `name_replacement`.
  name_too_long <- FALSE
  if (!is.null(error_header)) {
    header <- error_header
  } else if (nchar(x_name) > max_name_width) {
    object <- tolower(name_replacement)
    header <- c(
      paste0("Mistyped ", object, ": {.arg {x_name}}."),
      i = paste0(upper1(object), " must ", x_must, ".")
    )
    name_too_long <- TRUE
  } else {
    header <- paste0("{.arg {x_name}} must ", x_must, ".")
  }

  bullets <- error_bullets %||% vector_mistyped_bullets(
    x = x,
    len = len,
    nas = nas,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper,
    finite = finite,
    name_too_long = name_too_long,
    name_replacement = name_replacement
  )

  cli::cli_abort(
    c(header, bullets),
    call = error_call,
    class = c(error_class, "specifyr_error")
  )
}

stop_not_type <- function(
    x,
    x_name,
    type_desc,
    null = NULL,
    x_must = NULL,
    error_header = NULL,
    error_bullets = NULL,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped",
    max_name_width = 40,
    name_replacement = "Argument"
) {

  if (is.null(x_must)) {
    null_or <- if (isTRUE(null)) "`NULL` or " else ""
    x_must <- paste0("be ", null_or, paste(type_desc, collapse = " "))
  }

  if (!is.null(error_header)) {
    header <- error_header
    object <- "{.arg {x_name}}"
  } else if (nchar(x_name) > max_name_width) {
    object <- upper1(name_replacement)
    header <- c(
      "Mistyped argument: {.arg {x_name}}.",
      i = paste0(object, " must ", x_must, ".")
    )
  } else {
    header <- paste0("{.arg {x_name}} must ", x_must, ".")
    object <- "{.arg {x_name}}"
  }

  cli::cli_abort(
    c(
      header,
      error_bullets %||% c(x = paste(object, "is {.obj_type_friendly {x}}."))
    ),
    call = error_call,
    class = c(error_class, "specifyr_error")
  )
}

# TODO: Add the `{x_name}` too long stuff from `stop_vector_mistyped`
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

stop_not_character <- function() {

}

stop_not_factor <- function() {

}

stop_not_funish <- function() {

}

## mistyped helpers ------------------------------------------------------------

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

stop_wrong_names <- function(
    x,
    x_name,
    nms,
    strict = FALSE,
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
) {

}

vector_mistyped_bullets <- function(
    x,
    len = NULL,
    nas = NULL,
    min_len = NULL,
    max_len = NULL,
    lower = NULL,
    upper = NULL,
    finite = NULL,
    name_too_long = FALSE,
    name_replacement = "Argument"
) {

  object <- if (name_too_long) upper1(name_replacement) else "{.arg {x_name}}"

  x_len <- length(x)
  len_bullet <- if (isTRUE(x_len < (min_len %||% len) || (max_len %||% len) < x_len)) {
    paste0(object, " is ", length_n_friendly(x_len), ".")
  }
  x_nas <- is.na(x)
  nas_bullet <- if (isFALSE(nas) && any(x_nas)) {
    paste0(object, " is NA or NaN ", at_loc_friendly(x_nas), ".")
  }
  skip_min_max <- all(x_nas) || x_len == 0
  x_min <- if (!(skip_min_max || is.null(lower))) min(x, na.rm = TRUE)
  lower_bullet <- if (isTRUE(x_min < lower)) {
    paste0(object, " has a minimum of ", x_min, ".")
  }
  x_max <- if (!(skip_min_max || is.null(upper))) max(x, na.rm = TRUE)
  upper_bullet <- if (isTRUE(upper < x_max)) {
    paste0(object, " has a maximum of ", x_max, ".")
  }
  x_inf <- if (!is.null(finite)) is.infinite(x)
  finite_bullet <- if (isTRUE(finite) && any(x_inf)) {
    paste0(object, " is an infinite value ", at_loc_friendly(x_inf), ".")
  }

  c(
    x = len_bullet,
    x = nas_bullet,
    x = lower_bullet,
    x = upper_bullet,
    x = finite_bullet
  )
}

# check package objects --------------------------------------------------------

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

check_is_list_of_alias <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_input"
) {

  if (!is.list(x)) {
    alias <- check_is_alias(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
    return(list(alias))
  }
  .mapply(
    check_is_alias,
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

check_is_alias <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_input"
) {
  # `x` may be a quosure to allow recursive aliasers to receive the `..etc` symbol
  if (rlang::is_quosure(x) && is_symbol(quo_get_expr(x), "..etc")) {
    return(TRUE)
  }
  if (is_alias(rlang::eval_tidy(x))) {
    return(x)
  }
  stop_not_is_object(
    x = rlang::eval_tidy(x),
    x_name = x_name,
    obj_desc = c("a", "{.cls specifyr_type_alias}"),
    error_call = error_call,
    error_class = error_class
  )
}

check_property_dots <- function(
    ...,
    .list_of_properties = FALSE,
    .error_call = rlang::caller_env(),
    .error_class = "specifyr_error_invalid_arg"
) {

  properties <- rlang::list2(...)
  property_nms <- rlang::names2(properties)

  # Check names, check for duplicates
  if (any(property_nms == "")) {
    cli::cli_abort(
      c(
        "Arguments to `...` must be named.",
        x = '`...` arguments are unnamed {at_loc_friendly(property_nms == "")}.'
      ),
      call = .error_call,
      class = c("specifyr_error", .error_class)
    )
  } else if (any(property_nms %notin% builtin_properties(.list_of_properties))) {
    unrecognized <- setdiff(property_nms, builtin_properties(.list_of_properties))
    cli::cli_abort(
      c(
        "Arguments to `...` must be valid object properties to test or check.",
        x = "Supplied unrecognized properties: {.arg {unrecognized}}.",
        i = "Recognized properties are: {.arg {builtin_properties(.list_of_properties)}}."
      ),
      call = .error_call,
      class = c("specifyr_error", .error_class)
    )
  } else if (any(duplicated(property_nms))) {
    cli::cli_abort(
      c(
        "Arguments to `...` must have unique names.",
        x = "Duplicate argument names suppled to `...` {at_loc_friendly(duplicated(property_nms))}."
      ),
      call = .error_call,
      class = c("specifyr_error", .error_class)
    )
  }

  # Check that each property is valid, if it was supplied
  "null" %notin% property_nms || bool(properties$null, x_name = "null", error_call = .error_call, error_class = .error_class)
  "nas" %notin% property_nms || bool(properties$nas, x_name = "nas", error_call = .error_call, error_class = .error_class)
  "finite" %notin% property_nms || bool(properties$finite, x_name = "finite", error_call = .error_call, error_class = .error_class)
  len <- properties$len %!|% count(properties$len, x_name = "len", error_call = .error_call, error_class = .error_class)
  min_len <- properties$min_len %!|% count(properties$min_len, x_name = "min_len", error_call = .error_call, error_class = .error_class)
  max_len <- properties$max_len %!|% count(properties$max_len, x_name = "max_len", error_call = .error_call, error_class = .error_class)

  # Check the relationship between entangled arguments len, min_len, max_len
  stop_incompatible_length_properties(
    len = len,
    min_len = min_len,
    max_len = max_len,
    error_call = .error_call,
    error_class = .error_class
  )

  # Check the relationship between entangled arguments lower, upper. If
  # they exist, we'll need to take their quoted value, since they may
  # be a call to create a complex bound object (i.e. a Date).
  if ("lower" %in% property_nms && "upper" %in% property_nms) {
    stop_incompatible_bounds_properties(
      lower = properties$lower,
      upper = properties$upper
    )
  }

  # TODO: ACTUALLY! You need to just quote EVERY argument, so that the default
  # argument could be a call or some other argument. Might as well ALL be quoted.
  # So, after checking all of the VALUES of the evaluated built-in (with {specifyr})
  # properties, return them as quoted expressions!
  properties$lower <- rlang::enquos(...)$lower
  properties$upper <- rlang::enquos(...)$upper

  # Note that accessing with `$` has been creating NULL objects if they didn't
  # actually exist in `properties` (i.e. weren't provided to `...`). Now, we
  # use the names to return only the provided properties.
  properties[property_nms]
}

# Stop if:
# - both `len` and `min_len` or `max_len` are supplied
# - `min_len` is greater than `max_len`
stop_incompatible_length_properties <- function(
    len,
    min_len,
    max_len,
    len_name = "len",
    min_len_name = "min_len",
    max_len_name = "max_len",
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (!is.null(len) && (!is.null(min_len) || !is.null(max_len))) {
    non_null <- oxford(
      # `x` is the names of non-NULL arguments len, min_len, max_len
      x = paste0("{.arg ", c(len_name, min_len %!|% min_len_name, max_len %!|% max_len_name), "}"),
      last = "and"
    )
    cli::cli_abort(
      c(
        "Either {.arg {len_name}} must be NULL or both of {.arg {min_len_name}} and {.arg {max_len_name}} must be NULL.",
        x = paste("Arguments", non_null, "are not NULL.")
      ),
      call = error_call,
      class = c("specifyr_error", error_class)
    )
  } else if (isTRUE(min_len > max_len)) {
    cli::cli_abort(
      c(
        "{.arg {min_len_name}} must be less than or equal to {.arg {max_len}}.",
        x = "{.arg {min_len_name}} is {.val {min_len}}.",
        x = "{.arg {max_len_name}} is {.val {max_len}}."
      ),
      call = error_call,
      class = c("specifyr_error", error_class)
    )
  }
}

stop_incompatible_bounds_properties <- function(
    lower,
    upper,
    lower_name = "lower",
    upper_name = "upper",
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  valid_bounds <- rlang::try_fetch(
    lower <= upper,
    error = function(cnd) {
      cli::cli_abort(
        c(
          "{.arg {lower_name}} must be less than or equal to {.arg {upper_name}}.",
          x = "Can't evaluate `{lower_name} <= {upper_name}`.",
          i = "{.arg {lower_name}} is {.obj_type_friendly {lower}}.",
          i = "{.arg {upper_name}} is {.obj_type_friendly {upper}}."
        ),
        call = error_call,
        class = c("specifyr_error", error_class),
        parent = cnd
      )
    }
  )
  if (!valid_bounds) {
    cli::cli_abort(
      c(
        "{.arg {lower_name}} must be less than or equal to {.arg {upper_name}}.",
        x = "{.arg {lower_name}} is {.val {lower}}.",
        x = "{.arg {upper_name}} is {.val {upper}}."
      ),
      call = error_call,
      class = c("specifyr_error", error_class)
    )
  }
}

builtin_properties <- function(list_of_properties) {
  c(
    "len",
    "min_len",
    "max_len",
    "nas",
    "null",
    "lower",
    "upper",
    "finite",
    if (isTRUE(list_of_properties)) {
      c(
        "lst_len",
        "lst_min_len",
        "lst_max_len",
        "lst_null"
      )
    }
  )
}

# check package arguments ------------------------------------------------------

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

# internal error ---------------------------------------------------------------

#' @export
stop_malformed_alias <- function(error_call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "{.cls specifyr_type_alias} is malformed.",
      i = "Alias did not raise an error after a failed test."
    ),
    .internal = TRUE
  )
}

#' @export
stop_malformed_type_check <- function(error_call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "{.cls specifyr_type_check} is malformed.",
      i = "Type check did not raise an error after a failed test."
    ),
    .internal = TRUE
  )
}

# messages ---------------------------------------------------------------------

a_vector_friendly <- function(
    type_desc,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    upper = NULL,
    lower = NULL,
    finite = FALSE
) {

  null_or <- if (isTRUE(null)) "`NULL` or" else ""
  a_len_n <- a_length_n_friendly(len %||% c(min_len, max_len))
  non_na <- if (isFALSE(nas)) "non-NA" else ""
  finite <- if (isTRUE(finite)) "finite" else ""
  in_range <- in_range_friendly(lower, upper)

  # Allows `type_desc` to use the correct article (i.e. "an integer", not "a integer")
  if (length(type_desc) == 1) type_desc <- c("a", type_desc)
  if (a_len_n == "a" && non_na == "" && finite == "") {
    a_len_n <- type_desc[[1]]
    type_desc <- type_desc[[2]]
  } else {
    type_desc <- type_desc[[2]]
  }

  cli::format_inline(
    paste(null_or, a_len_n, non_na, finite, type_desc, in_range),
    keep_whitespace = FALSE
  )
}

at_loc_friendly <- function(loc, n_max = 5) {
  loc <- if (is.logical(loc)) which(loc & !is.na(loc)) else loc
  loc <- as.numeric(loc)
  n <- length(loc)
  at <- ngettext(min(n, n_max), "at location ", "at locations ")
  if (n > n_max) {
    paste0(at, "`", deparse(loc[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(loc), "`")
  }
}

# TODO: Revise this to include a `min_len` and `max_len` argument.
a_length_n_friendly <- function(len) {
  if (is.null(len)) {
    "a"
  } else if (length(len) == 2) {
    len <- format_len(len)
    paste0("a length [", len[[1]], "-", len[[2]], "]")
  } else if (len == 1) {
    "a scalar"
  } else if (len > 0) {
    paste0("a length-", format_len(len))
  } else {
    "an empty"
  }
}

# TODO: Revise this to include a `min_len` and `max_len` argument.
length_n_friendly <- function(len) {
  if (is.null(len)) {
    ""
  } else if (length(len) == 2) {
    len <- format_len(len)
    paste0("length [", len[[1]], "-", len[[2]], "]")
  } else if (len > 0) {
    paste0("length-", format_len(len))
  } else {
    "empty"
  }
}

format_len <- function(x) format(x, scientific = FALSE, big.mark = ",")

in_range_friendly <- function(lower, upper) {
  no_lower <- is.null(lower) || is.infinite(lower)
  no_upper <- is.null(upper) || is.infinite(upper)
  if (no_lower && no_upper) {
    ""
  } else if (no_lower) {
    paste("upper bounded by", upper)
  } else if (no_upper) {
    paste("lower bounded by", lower)
  } else {
    paste0("in range [", lower, ", ", upper, "]")
  }
}

upper1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

commas <- function(x, n = NULL) {
  if (isTRUE(length(x) > n)) {
    length(x) <- n
    paste0(paste(x, collapse = ", "), ", ...")
  } else {
    paste(x, collapse = ", ")
  }
}

oxford <- function(x, sep = ", ", last = "or", n = NULL) {
  x_len <- length(x)
  if (x_len <= 1) {
    return(paste(x))
  } else if (!is.null(n) && x_len > n) {
    length(x) <- n
    return(paste0(paste(x, collapse = sep), sep, " ..."))
  }
  if (x_len == 2) sep <- " "
  paste(paste(x[-x_len], collapse = sep), last, x[[x_len]], sep = sep)
}


