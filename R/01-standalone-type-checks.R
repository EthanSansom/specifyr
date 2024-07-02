# The {specifyr} type checks are defined using generator functions, which means
# that I can't check the argument types supplied *to the generator functions*
# using my own type checks. So, defining some shims here for internal type
# checking.

# type -------------------------------------------------------------------------

# TODO: Follow the example in `int`, for how the type shims should be made.
#       Eventually, delete the ones that you don't need.

## int -------------------------------------------------------------------------

int_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg",
    error_header = NULL,
    error_bullets = NULL
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_integer(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper
  )) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("an", "{.cls integer} vector"),
    type_test = is.integer(x),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper,
    finite = finite,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_int_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (lst_null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = int,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      lower = lower,
      upper = upper,
      error_call = error_call,
      error_class = error_class
    )
  )
}

# num --------------------------------------------------------------------------

num_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    finite = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_numeric(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper,
    finite = finite
  )) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", "{.cls numeric} vector"),
    type_test = is.numeric(x),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper,
    finite = finite,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_num_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    finite = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (lst_null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = num,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      lower = lower,
      upper = upper,
      finite = finite,
      error_call = error_call,
      error_class = error_class
    )
  )
}

# dbl --------------------------------------------------------------------------

dbl_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    finite = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_double(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper,
    finite = finite
  )) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", "{.cls double} vector"),
    type_test = is.double(x),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper,
    finite = finite,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_dbl_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    finite = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (lst_null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = dbl,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      lower = lower,
      upper = upper,
      finite = finite,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## cpl -------------------------------------------------------------------------

cpl_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_complex(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", "{.cls complex} vector"),
    type_test = is.complex(x),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_cpl_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (lst_null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = cpl,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## lgl -------------------------------------------------------------------------

lgl_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_logical(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", "{.cls logical} vector"),
    type_test = is.logical(x),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_lgl_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = lgl,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## chr -------------------------------------------------------------------------

chr_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_character(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", "character vector"),
    type_test = is.character(x),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_chr_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (lst_null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = chr,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## byt -------------------------------------------------------------------------

byt_ <- function(
    x,
    len = NULL,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_raw(
    x,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", "raw vector"),
    type_test = is.raw(x),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_byt_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = byt,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## lst -------------------------------------------------------------------------

lst_ <- function(
    x,
    len = NULL,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    types = character(0L),
    missing = TRUE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_list(
    x,
    types = types,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )) {
    return(x)
  }
  of <- if (is_empty(types)) {
    character(0L)
  } else {
    paste(" of", oxford(paste0("{.cls ", types, "}")), "objects")
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", paste0("list", of)),
    type_test = is.list(x),
    len = len,
    null = null,
    min_len = min_len,
    max_len = max_len,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_lst_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    types = character(0L),
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = lst,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      null = null,
      min_len = min_len,
      max_len = max_len,
      types = types,
      error_call = error_call,
      error_class = error_class
    )
  )
}

# psx --------------------------------------------------------------------------

# TODO: POSIXct

# dte --------------------------------------------------------------------------

# TODO: Date

# fac --------------------------------------------------------------------------

# TODO: Factor

## intish -----------------------------------------------------------------------

intish_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_integerish(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper
  )) {
    return(as.integer(x))
  }

  stop_not_integerish(
    x = x,
    x_name = x_name,
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_intish_ <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (lst_null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list(x),
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }
  .mapply(
    FUN = intish,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      len = len,
      nas = nas,
      null = null,
      min_len = min_len,
      max_len = max_len,
      lower = lower,
      upper = upper,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## count -----------------------------------------------------------------------

count_ <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::qtest(x, "X1")) {
    return(as.integer(x))
  }
  stop_not_integerish(
    x = x,
    x_name = x_name,
    x_must = "be a whole number",
    len = 1,
    nas = FALSE,
    error_call = error_call,
    error_class = error_class
  )
}

## bool ------------------------------------------------------------------------

bool_ <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {
  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::qtest(x, "B1")) {
    return(x)
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_test = is.logical(x),
    type_desc = c("a", "logical vector"),
    x_must = "be a single `TRUE` or `FALSE` value",
    len = 1,
    nas = FALSE,
    error_call = error_call,
    error_class = error_class
  )
}

## string ----------------------------------------------------------------------

string_ <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::qtest(x, "S1")) {
    return(x)
  }
  if (checkmate::qtest(x, "S1")) {
    stop_not_is_object(
      x = x,
      x_name = x_name,
      x_must = "be a non-empty string"
    )
  }
  stop_vector_mistyped(
    x = x,
    x_name = x_name,
    type_desc = c("a", "character vector"),
    type_test = is.character(x),
    x_must = if (empty) "be a string" else "be a non-empty string",
    len = 1,
    nas = FALSE,
    error_call = error_call,
    error_class = error_class
  )
}

## vir -------------------------------------------------------------------------

vir_ <- function(
    x,
    null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (is.environment(x) || (null && is.null(x))) {
    return(x)
  }
  stop_not_type(
    x = x,
    x_name = x_name,
    type_desc = c("an", "{.cls environment}"),
    null = null,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_vir_ <- function(
    x,
    null = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_list(
    x,
    types = "environment",
    any.missing = null,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list,
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }

  .mapply(
    FUN = vir,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      null = null,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## fun -------------------------------------------------------------------------

fun_ <- function(
    x,
    null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (is.function(x) || (null && is.null(x))) {
    return(x)
  }
  stop_not_type(
    x = x,
    x_name = x_name,
    type_desc = c("a", "function"),
    null = null,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_fun_ <- function(
    x,
    null = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (checkmate::test_list(
    x,
    types = "function",
    any.missing = null,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list,
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }

  .mapply(
    FUN = fun,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      null = null,
      error_call = error_call,
      error_class = error_class
    )
  )
}

## funish ----------------------------------------------------------------------

funish_ <- function(
    x,
    null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }

  if (is.function(x) || (null && is.null(x))) {
    return(x)
  }
  if (rlang::is_formula(x, lhs = FALSE)) {
    return(rlang::as_function(x))
  }
  stop_not_type(
    x = x,
    x_name = x_name,
    type_desc = c("a", "function or purrr-style lambda"),
    null = null,
    error_call = error_call,
    error_class = error_class
  )
}

lst_of_funish_ <- function(
    x,
    null = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_invalid_arg"
) {

  if (rlang::is_missing(x)) {
    stop_arg_missing(
      x = x,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  if (null && is.null(x)) {
    return(x)
  }
  if (!checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )) {
    stop_vector_mistyped(
      x = x,
      x_name = x_name,
      type_desc = c("a", "list"),
      type_test = is.list,
      len = lst_len,
      null = lst_null,
      min_len = lst_min_len,
      max_len = lst_max_len,
      error_call = error_call,
      error_class = error_class
    )
  }

  .mapply(
    FUN = funish,
    dots = list(
      x = x,
      x_name = sprintf("%s[[%i]]", x_name, seq_along(x))
    ),
    MoreArgs = list(
      null = null,
      error_call = error_call,
      error_class = error_class
    )
  )
}

# misc -------------------------------------------------------------------------

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
