# vir --------------------------------------------------------------------------

vir <- function(
    x,
    null = FALSE,
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
  if (is.environment(x) || (null && is.null(x))) {
    return(x)
  }
  stop_not_is_object(
    x = x,
    x_name = x_name,
    obj_name = "an {.cls environment}",
    null = null,
    error_call = error_call,
    error_class = error_class
  )
}
class(vir) <- c("specifyr_type_check", "function")

test_vir <- function(x, null = FALSE) {
  is.environment(x) || (null && is.null(x))
}
class(test_vir) <- c("specifyr_type_test", "function")

lst_of_vir <- function(
    x,
    null = FALSE,
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
class(lst_of_vir) <- c("specifyr_type_check", "function")

test_lst_of_vir <- function(
    x,
    null = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE
) {
  checkmate::test_list(
    x,
    types = "environment",
    any.missing = null,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )
}
class(test_lst_of_vir) <- c("specifyr_type_test", "function")

# fun --------------------------------------------------------------------------

fun <- function(
    x,
    null = FALSE,
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
  if (is.function(x) || (null && is.null(x))) {
    return(x)
  }
  stop_not_is_object(
    x = x,
    x_name = x_name,
    obj_name = "a function",
    null = null,
    error_call = error_call,
    error_class = error_class
  )
}
class(fun) <- c("specifyr_type_check", "function")

test_fun <- function(x, null = FALSE) {
  is.function(x) || (null && is.null(x))
}
class(test_fun) <- c("specifyr_type_test", "function")

lst_of_fun <- function(
    x,
    null = FALSE,
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
class(lst_of_fun) <- c("specifyr_type_check", "function")

test_lst_of_fun <- function(
    x,
    null = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE
) {
  checkmate::test_list(
    x,
    types = "function",
    any.missing = null,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  )
}
class(test_lst_of_fun) <- c("specifyr_type_test", "function")

# funish -----------------------------------------------------------------------

# TODO: This is a function or one-sided formula (purrr-style lambda). If a lambda,
# we convert to a function before returning.

funish <- function(
    x,
    null = FALSE,
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

  if (is.function(x) || (null && is.null(x))) {
    return(x)
  }
  if (rlang::is_formula(x, lhs = FALSE)) {
    return(rlang::as_function(x))
  }
  stop_not_is_object(
    x = x,
    x_name = x_name,
    obj_name = "a function or purrr-style lambda",
    null = null,
    error_call = error_call,
    error_class = error_class
  )
}
class(funish) <- c("specifyr_type_check", "function")

test_funish <- function(x, null = FALSE) {
  is.function(x) || rlang::is_formula(x, lhs = FALSE) || (null && is.null(x))
}
class(test_funish) <- c("specifyr_type_test", "function")

lst_of_funish <- function(
    x,
    null = FALSE,
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
class(lst_of_funish) <- c("specifyr_type_check", "function")

test_lst_of_funish <- function(
    x,
    null = FALSE,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE
) {
  checkmate::test_list(
    x,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_funish,
    FUN.VALUE = logical(1L),
    null = null
  ))
}
class(test_lst_of_funish) <- c("specifyr_type_test", "function")
