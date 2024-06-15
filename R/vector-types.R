# todo -------------------------------------------------------------------------


# int --------------------------------------------------------------------------

# A "check" returns `x` if `x` is the correct type, errors otherwise.
# A "test" returns TRUE if `x` is the correct type, returns FALSE otherwise.

int <- function(
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
class(int) <- c("specifyr_type_check", "function")

test_int <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf
) {
  checkmate::test_integer(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper
  )
}
class(test_int) <- c("specifyr_type_test", "function")

lst_of_int <- function(
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
class(lst_of_int) <- c("specifyr_type_check", "function")

test_lst_of_int <- function(
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
    error_class = "specifyr_error_mistyped"
  ) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_int,
    FUN.VALUE = logical(1L),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper
  ))
}
class(test_lst_of_int) <- c("specifyr_type_test", "function")

# num --------------------------------------------------------------------------

num <- function(
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
class(num) <- c("specifyr_type_check", "function")

test_num <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    finite = FALSE
) {
  checkmate::test_numeric(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper,
    finite = finite
  )
}
class(test_num) <- c("specifyr_type_test", "function")

lst_of_num <- function(
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
class(lst_of_num) <- c("specifyr_type_check", "function")

test_lst_of_num <- function(
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
    error_class = "specifyr_error_mistyped"
) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_num,
    FUN.VALUE = logical(1L),
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
class(test_lst_of_num) <- c("specifyr_type_test", "function")

# dbl --------------------------------------------------------------------------

dbl <- function(
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
class(dbl) <- c("specifyr_type_check", "function")

test_dbl <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    finite = FALSE
) {
  checkmate::test_double(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper,
    finite = finite
  )
}
class(test_dbl) <- c("specifyr_type_test", "function")

lst_of_dbl <- function(
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
class(lst_of_dbl) <- c("specifyr_type_check", "function")

test_lst_of_dbl <- function(
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
    error_class = "specifyr_error_mistyped"
) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_dbl,
    FUN.VALUE = logical(1L),
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
class(test_lst_of_dbl) <- c("specifyr_type_test", "function")

# cpl --------------------------------------------------------------------------

cpl <- function(
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
class(cpl) <- c("specifyr_type_check", "function")

test_cpl <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf
) {
  checkmate::test_complex(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )
}
class(test_cpl) <- c("specifyr_type_test", "function")

lst_of_cpl <- function(
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
class(lst_of_cpl) <- c("specifyr_type_check", "function")

test_lst_of_cpl <- function(
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
    error_class = "specifyr_error_mistyped"
) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_cpl,
    FUN.VALUE = logical(1L),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len
  ))
}
class(test_lst_of_cpl) <- c("specifyr_type_test", "function")

# lgl --------------------------------------------------------------------------

lgl <- function(
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
class(lgl) <- c("specifyr_type_check", "function")

test_lgl <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf
) {
  checkmate::test_logical(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )
}
class(test_lgl) <- c("specifyr_type_test", "function")

lst_of_lgl <- function(
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
class(lst_of_lgl) <- c("specifyr_type_check", "function")

test_lst_of_lgl <- function(
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
    error_class = "specifyr_error_mistyped"
) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = null
  ) && all(vapply(
    X = x,
    FUN = test_lgl,
    FUN.VALUE = logical(1L),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len
  ))
}
class(test_lst_of_lgl) <- c("specifyr_type_test", "function")

# chr --------------------------------------------------------------------------

chr <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    # TODO: Maybe add some more of the character arguments! `checkmate` has a bunch,
    #       that might be worthwhile (and easy) to throw in.
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
class(chr) <- c("specifyr_type_check", "function")

test_chr <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf
) {
  checkmate::test_character(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )
}
class(test_chr) <- c("specifyr_type_test", "function")

lst_of_chr <- function(
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
class(lst_of_chr) <- c("specifyr_type_check", "function")

test_lst_of_chr <- function(
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
    error_class = "specifyr_error_mistyped"
) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_chr,
    FUN.VALUE = logical(1L),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len
  ))
}
class(test_lst_of_chr) <- c("specifyr_type_test", "function")

# byt --------------------------------------------------------------------------

byt <- function(
    x,
    len = NULL,
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
class(byt) <- c("specifyr_type_check", "function")

test_byt <- function(
    x,
    len = NULL,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf
) {
  checkmate::test_raw(
    x,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )
}
class(test_byt) <- c("specifyr_type_test", "function")

lst_of_byt <- function(
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
class(lst_of_byt) <- c("specifyr_type_check", "function")

test_lst_of_byt <- function(
    x,
    len = NULL,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lst_len = NULL,
    lst_min_len = NULL,
    lst_max_len = NULL,
    lst_null = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_byt,
    FUN.VALUE = logical(1L),
    len = len,
    null = null,
    min_len = min_len,
    max_len = max_len
  ))
}
class(test_lst_of_byt) <- c("specifyr_type_test", "function")

# lst --------------------------------------------------------------------------

lst <- function(
    x,
    len = NULL,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    types = character(0L),
    missing = TRUE,
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
class(lst) <- c("specifyr_type_check", "function")

lst_test <- function(
    x,
    len = NULL,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    types = character(0L),
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "specifyr_error_mistyped"
) {
  checkmate::test_list(
    x,
    types = types,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null
  )
}
class(lst) <- c("specifyr_type_test", "function")

lst_of_lst <- function(
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
class(lst_of_lst) <- c("specifyr_type_check", "function")

test_lst_of_lst <- function(
    x,
    len = NULL,
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

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_lst,
    FUN.VALUE = logical(1L),
    len = len,
    null = null,
    min_len = min_len,
    max_len = max_len,
    types = types
  ))
}
class(test_lst_of_lst) <- c("specifyr_type_test", "function")

# vec --------------------------------------------------------------------------

# TODO: Some generic vector. Add the other functions.
vec <- function(
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
  if (checkmate::test_vector(
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
    type_test = checkmate::test_vector(x),
    type_desc = c("a", "vector"),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    error_call = error_call,
    error_class = error_class
  )
}
class(vec) <- c("specifyr_type_check", "function")

# psx --------------------------------------------------------------------------

# TODO: POSIXct

# dte --------------------------------------------------------------------------

# TODO: Date

# intish -----------------------------------------------------------------------

intish <- function(
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
class(intish) <- c("specifyr_type_check", "function")

test_intish <- function(
    x,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf
) {
  checkmate::test_integerish(
    x,
    any.missing = nas,
    len = len,
    min.len = min_len,
    max.len = max_len,
    null.ok = null,
    lower = lower,
    upper = upper
  )
}
class(test_intish) <- c("specifyr_type_test", "function")

lst_of_intish <- function(
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
class(lst_of_intish) <- c("specifyr_type_check", "function")

test_lst_of_intish <- function(
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
    lst_null = FALSE
) {

  checkmate::test_list(
    x,
    len = lst_len,
    min.len = lst_min_len,
    max.len = lst_max_len,
    null.ok = lst_null
  ) && all(vapply(
    X = x,
    FUN = test_intish,
    FUN.VALUE = logical(1L),
    len = len,
    nas = nas,
    null = null,
    min_len = min_len,
    max_len = max_len,
    lower = lower,
    upper = upper
  ))
}
class(test_lst_of_intish) <- c("specifyr_type_test", "function")

# count ------------------------------------------------------------------------

count <- function(
    x,
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
  if (checkmate::qtest(x, "X1")) {
    return(as.integer(x))
  }
  stop_not_integerish(
    x = x,
    x_name = x_name,
    x_must = "be a whole number.",
    len = 1,
    nas = FALSE,
    error_call = error_call,
    error_class = error_class
  )
}

test_count <- function(x) {
  checkmate::qtest(x, "X1")
}

# bool -------------------------------------------------------------------------

bool <- function(
    x,
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

test_bool <- function(x) {
  checkmate::qtest(x, "B1")
}

# string -----------------------------------------------------------------------

string <- function(
    x,
    empty = TRUE,
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
  if (checkmate::qtest(x, "S1") && (empty || x != "")) {
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

test_string <- function(x) {
  checkmate::qtest(x, "S1")
}

# new --------------------------------------------------------------------------

# NOTE:
# `type_must` is used to override the default vector must be message.
# This is helpful if you want to skip the `length-n non-NA in range` bit.
# For example, instead of:
# "a non-NA numeric vector in range [0, 1]"
# We can say:
# "a probability"

# NOTE:
# - adding a `names` check is out of scope for types. This can be added to
#   specifications via adding the `check_names` check to the specification.

# In the documentation, mention the operators used by the test, to ensure that
# the type obeys these rules as expected.
# - len, min_len, max_len:  `base::length`
# - nas:                    `base::is.na`
# - null:                   `base::is.null`
# - lower, upper:           `<=`, `>=`
# - finite:                 `base::is.finite`

# TODO: Re-write this in the style of `new_atomic_type`
new_vector_type <- function(
    type_test,
    type_class,
    type_desc,
    ...,
    type_must = NULL,
    checks = list(),
    env = rlang::caller_env(),
    returns = quote(x)
) {

  is_cls <- rlang::check_exclusive(type_test, type_class) == "type_class"
  is_atomic_cls <- is_cls && is_atomic_class(chr(type_class, nas = FALSE))

  type_desc <- chr(type_desc, min_len = 1, max_len = 2, nas = FALSE)
  type_args <- check_vector_type_args(...)
  type_arg_nms <- rlang::names2(type_args)
  checks <- check_is_list_of_check(checks)
  env <- vir(env)

  if (is_atomic_cls) {
    # TODO: We're checking `args` twice here (once also in `make_checkmate_atomic_test`)
    # - fix the `make_*_test` functions to throw better errors
    type_test <- make_class_test(type_class)
    test <- make_checkmate_atomic_test(type_class, type_arg_nms)
  } else if (is_cls) {
    type_test <- rlang::expr(inherits(x, !!type_class))
    test <- make_base_vector_test(type_test, type_arg_nms)
  } else {
    type_test <- rlang::enexpr(type_test)
    test <- make_base_vector_test(type_test, type_arg_nms)
  }

  return_if_null <- if (!is_atomic_cls && "null" %in% type_arg_nms) {
    rlang::expr(if (null && is.null(x)) { return(x) })
  }
  stop_if_missing <- rlang::expr(
    if (rlang::is_missing(x)) {
      stop_arg_missing(
        x = x,
        x_name = x_name,
        error_call = error_call,
        error_class = error_class
      )
    }
  )
  stop <- rlang::call2(
    .fn = "stop_vector_mistyped",
    x = quote(x),
    x_name = quote(x_name),
    x_must = type_must,
    !!!rlang::set_names(rlang::syms(type_arg_nms), type_arg_nms),
    type_test = type_test,
    type_desc = type_desc,
    error_call = quote(error_call),
    error_class = quote(error_class)
  )

  # `lower` and `upper` might be calls to create objects (e.g. `as.Date(...)`),
  # which need to be quoted to preserve.
  if ("lower" %in% type_arg_nms) type_args$lower <- rlang::enexprs(...)$lower
  if ("upper" %in% type_arg_nms) type_args$upper <- rlang::enexprs(...)$upper

  if (is_empty(checks)) {
    body <- expr_squash(
      stop_if_missing,
      rlang::expr(if (!!test) { return(!!returns) }),
      stop
    )
  } else {
    check_tests <- lapply(checks, as_test)
    check_stops <- lapply(checks, as_stop)

    stop_if_mistyped <- rlang::expr(if (!(!!test)) { !!stop })
    stop_if_checks_fail <- .mapply(
      \(check_test, check_stop) rlang::expr(if (!(!!test)) { !!stop }),
      dots = list(
        test = check_tests,
        stop = check_stops
      ),
      MoreArgs = list()
    )
    # If we reach the final `stop_if_check_fail` expression, the last check
    # must have failed, so we can just emit the error instead.
    stop_if_checks_fail[[length(checks)]] <- check_stops[[length(checks)]]

    # Test the type and additional checks at the onset to return early on a pass
    test <- expr_and(test, !!!check_tests)
    body <- expr_squash(
      stop_if_missing,
      rlang::expr(if (isTRUE(!!test)) { return(!!returns) }),
      stop_if_mistyped,
      !!!stop_if_checks_fail
    )
  }

  args <- rlang::pairlist2(
    x = ,
    !!!type_args,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::caller_env()),
    error_class = "specifyr_error_mistyped"
  )
  type_check <- rlang::new_function(
    args = args,
    body = body,
    env = env
  )
  class(type_check) <- c("specifyr_type_check", "function")
  type_check
}

if (FALSE) {
  rm(list = ls())
  load_all()

  date <- new_vector_type(
    type_test = lubridate::is.Date(x),
    type_desc = c("a", "date"),
    len = NULL,
    nas = TRUE
  )

  my_num <- new_vector_type(
    type_class = "numeric",
    type_desc = c("a", "number"),
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = -Inf,
    upper = Inf,
    finite = FALSE
  )
}

# TODO: IF `new_vector_type` works as expected, this should be obsolete - delete it.
new_atomic_type <- function(
    type_class,
    type_desc,
    ...,
    type_must = NULL,
    checks = list(),
    env = rlang::caller_env(),
    returns = quote(x)
) {

  type_class <- string(type_class)
  type_desc <- chr(type_desc, min_len = 1, max_len = 2, nas = FALSE)
  type_args <- check_vector_type_args(...)
  checks <- check_is_list_of_check(checks)
  env <- vir(env)

  type_test <- make_class_test(type_class)
  type_arg_nms <- rlang::names2(type_args)

  stop_if_missing <- rlang::expr(
    if (rlang::is_missing(x)) {
      stop_arg_missing(
        x = x,
        x_name = x_name,
        error_call = error_call,
        error_class = error_class
      )
    }
  )
  test <- make_checkmate_atomic_test(type_class, type_arg_nms)
  stop <- rlang::call2(
    .fn = "stop_vector_mistyped",
    x = quote(x),
    x_name = quote(x_name),
    x_must = type_must,
    !!!rlang::set_names(rlang::syms(type_arg_nms), type_arg_nms),
    type_test = make_class_test(type_class),
    type_desc = type_desc,
    error_call = quote(error_call),
    error_class = quote(error_class)
  )

  # `lower` and `upper` might be calls to create objects (e.g. `as.Date(...)`),
  # which need to be quoted to preserve.
  if ("lower" %in% type_arg_nms) type_args$lower <- rlang::enexprs(...)$lower
  if ("upper" %in% type_arg_nms) type_args$upper <- rlang::enexprs(...)$upper

  if (rlang::is_empty(checks)) {
    body <- expr_squash(
      stop_if_missing,
      rlang::expr(if (!!test) { return(!!returns) }),
      stop
    )
  } else {
    check_tests <- lapply(checks, as_test)
    check_stops <- lapply(checks, as_stop)

    stop_if_mistyped <- rlang::expr(if (!(!!test)) { !!stop })
    stop_if_checks_fail <- .mapply(
      \(check_test, check_stop) rlang::expr(if (!(!!test)) { !!stop }),
      dots = list(
        test = check_tests,
        stop = check_stops
      ),
      MoreArgs = list()
    )
    # If we reach the final `stop_if_check_fail` expression, the last check
    # must have failed, so we can just emit the error instead.
    stop_if_checks_fail[[length(checks)]] <- check_stops[[length(checks)]]

    # Test the type and additional checks at the onset to return early on a pass
    test <- expr_and(test, !!!check_tests)
    body <- expr_squash(
      stop_if_missing,
      rlang::expr(if (isTRUE(!!test)) { return(!!returns) }),
      stop_if_mistyped,
      !!!stop_if_checks_fail
    )
  }

  args <- rlang::pairlist2(
    x = ,
    !!!type_args,
    x_name = quote(rlang::caller_arg(x)),
    error_call = quote(rlang::caller_env()),
    error_class = "specifyr_error_mistyped"
  )
  type_check <- rlang::new_function(
    args = args,
    body = body,
    env = env
  )
  class(type_check) <- c("specifyr_type_check", "function")
  type_check
}

new_atomic_test <- function(
    type_class,
    ...,
    checks = list(),
    env = rlang::caller_env()
) {

  type_class <- string(type_class) # FIXME: Check for valid atomic types
  type_args <- check_vector_type_args(...)
  checks <- check_is_list_of_check(checks)
  env <- vir(env)

  type_test <- make_class_test(type_class)
  type_arg_nms <- rlang::names2(type_args)
  test <- make_checkmate_atomic_test(type_class, type_arg_nms)

  # `lower` and `upper` maybe be calls, so they need to be quoted
  if ("lower" %in% type_arg_nms) type_args$lower <- rlang::enexprs(...)$lower
  if ("upper" %in% type_arg_nms) type_args$lower <- rlang::enexprs(...)$upper

  if (!rlang::is_empty(checks)) {
    test <- expr_and(test, !!!lapply(checks, as_test))
  }
  type_test <- rlang::new_function(
    args = rlang::pairlist2(x = , !!!type_args),
    body = test,
    env = env
  )
  class(type_test) <- c("specifyr_type_test", "function")
  type_test
}

# factories --------------------------------------------------------------------

make_class_test <- function(cls) {
  switch(
    cls,
    integer = quote(is.integer(x)),
    numeric = quote(is.numeric(x)),
    double = quote(is.double(x)),
    complex = quote(is.complex(x)),
    logical = quote(is.logical(x)),
    character = quote(is.character(x)),
    raw = quote(is.raw(x)),
    factor = quote(is.factor(x)),
    list = quote(is.list(x)),
    environment = quote(is.environment(x)),
    data.frame = quote(is.data.frame(x)),
    `function` = quote(is.function(x)),
    rlang::expr(inherits(x, !!cls))
  )
}

make_checkmate_atomic_test <- function(
    cls,
    args,
    error_call = rlang::caller_env()
  ) {

  # If `args` is a list, we make a test with fixed parameters (e.g. length(x) == 1L),
  # otherwise the test assumes arguments have been provided (e.g. length(x) == len).
  checkmate_fn <- paste0("test_", cls)
  if (is.character(args)) {
    args <- rlang::set_names(rlang::syms(args), translate_checkmate_args(args))
    args <- as.pairlist(args)
  } else if (is.list(args)) {
    args <- check_vector_type_args(!!!args)
    names(args) <- translate_checkmate_args(names(args))
  } else {
    stop("Bad args") # FIXME
  }
  rlang::call2(
    .fn = checkmate_fn,
    .ns = "checkmate",
    x = quote(x),
    !!!args
  )
}

make_base_vector_test <- function(
    type_test,
    args,
    error_call = rlang::caller_env()
  ) {

  # If `args` is a list, we make a test with fixed parameters (e.g. length(x) == 1L),
  # otherwise the test assumes arguments have been provided (e.g. length(x) == len).
  fixed_args <- is.list(args)
  if (fixed_args) {
    args <- check_vector_type_args(!!!args)
    arg_nms <- names(args)
  } else if (is.character(args)) {
    arg_nms <- args
    args <- rlang::syms(arg_nms)
  } else {
    stop("Bad args") # FIXME
  }

  nas_test <- if (fixed_args) {
    if (isFALSE(args$nas)) quote(!anyNA(x)) else NULL
  } else {
    quote(nas || !anyNA(x))
  }
  vector_tests <- list(
    nas = nas_test,
    len = rlang::expr((is.null(len) || length(x) == !!args$len)),
    min_len = rlang::expr((is.null(min_len) || length(x) >= !!args$min_len)),
    max_len = rlang::expr((is.null(min_len) || length(x) >= !!args$min_len)),
    lower = rlang::expr((all(!!args$lower <= x, na.rm = TRUE))),
    upper = rlang::expr((all(!!args$upper >= x, na.rm = TRUE))),
    bounds = rlang::expr((all(!!args$lower <= x && x <= !!args$upper, na.rm = TRUE)))
  )

  arg_tests <- vector_tests[arg_nms]
  if (all(c("lower", "upper") %in% arg_nms)) {
    arg_tests$lower <- NULL
    arg_tests$upper <- NULL
    arg_tests$bounds <- vector_tests$bounds
  }
  expr_and(type_test, !!!arg_tests)
}

translate_checkmate_args <- function(arg_nms) {
  dict <- c(
    len = "len",
    lst_len = "len",
    nas = "any.missing",
    null = "null.ok",
    lst_null = "null.ok",
    min_len = "min.len",
    lst_min_len = "min.len",
    max_len = "max.len",
    lst_max_len = "max.len",
    lower = "lower",
    upper = "upper",
    finite = "finite"
  )
  if (!all(arg_nms %in% names(dict))) {
    bad_nms <- setdiff(arg_nms, names(dict))
    cli::cli_abort(
      "{.arg arg_nms} contains {length(bad_nms)} invalid name{?s}: {.val {bad_nms}}.",
      .internal = TRUE
    )
  }
  dict[arg_nms]
}

is_atomic_class <- function(cls) {
  length(cls) == 1 &&
    cls %in% c(
    "integer",
    "numeric",
    "double",
    "complex",
    "logical",
    "character",
    "raw"
  )
}

# TODO: Add more
is_checkmateable_class <- function(cls) {
  cls %in% c(
    "integer",
    "numeric",
    "double",
    "complex",
    "logical",
    "character",
    "raw",
    "factor",
    "list",
    "environment",
    "data.frame",
    "function"
  )
}

# helpers ----------------------------------------------------------------------

is_type_check <- function(x) {
  inherits(x, "specifyr_type_check")
}

is_type_test <- function(x) {
  inherits(x, "specifyr_type_test")
}

is_type_spec <- function(x) {
  inherits(x, "specifyr_type_spec")
}
