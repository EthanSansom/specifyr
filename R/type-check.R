# int --------------------------------------------------------------------------

# A "check" returns `x` if `x` is the correct type, errors otherwise.
# A "test" returns TRUE if `x` is the correct type, returns FALSE otherwise.

int <- new_builtin_type(
  type_class = "integer",
  type_desc = c("an", "{.cls integer} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = -Inf,
  upper = Inf
)

test_int <- new_builtin_type(
  type_class = "integer",
  type_desc = c("an", "{.cls integer} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = -Inf,
  upper = Inf
)

lst_of_int <- new_list_of_type(int)

test_lst_of_int <- new_list_of_type(test_int)

# num --------------------------------------------------------------------------

num <- new_builtin_type(
  type_class = "numeric",
  type_desc = c("a", "{.cls numeric} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = -Inf,
  upper = Inf
)

test_num <- new_builtin_type(
  type_class = "numeric",
  type_desc = c("a", "{.cls numeric} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = -Inf,
  upper = Inf
)

lst_of_num <- new_list_of_type(num)

test_lst_of_num <- new_list_of_type(test_num)

# dbl --------------------------------------------------------------------------

dbl <- new_builtin_type(
  type_class = "double",
  type_desc = c("a", "{.cls double} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = -Inf,
  upper = Inf
)

test_dbl <- new_builtin_type(
  type_class = "double",
  type_desc = c("a", "{.cls double} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = -Inf,
  upper = Inf
)

lst_of_dbl <- new_list_of_type(dbl)

test_lst_of_dbl <- new_list_of_type(test_dbl)

# cpl --------------------------------------------------------------------------

cpl <- new_builtin_type(
  type_class = "complex",
  type_desc = c("a", "{.cls complex} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

test_cpl <- new_builtin_type(
  type_class = "complex",
  type_desc = c("a", "{.cls complex} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

lst_of_cpl <- new_list_of_type(cpl)

test_lst_of_cpl <- new_list_of_type(test_cpl)

# lgl --------------------------------------------------------------------------

lgl <- new_builtin_type(
  type_class = "logical",
  type_desc = c("a", "{.cls logical} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

test_lgl <- new_builtin_type(
  type_class = "logical",
  type_desc = c("a", "{.cls logical} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

lst_of_lgl <- new_list_of_type(lgl)

test_lst_of_lgl <- new_list_of_type(test_lgl)

# TODO: Keep adding below here

# chr --------------------------------------------------------------------------

chr <- new_builtin_type(
  type_class = "character",
  type_desc = c("a", "{.cls character} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

test_chr <- new_builtin_type(
  type_class = "character",
  type_desc = c("a", "{.cls character} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

lst_of_chr <- new_list_of_type(chr)

test_lst_of_chr <- new_list_of_type(test_chr)

# psx --------------------------------------------------------------------------

psx <- new_builtin_type(
  type_class = "posixct",
  type_desc = c("a", "{.cls POSIXct} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = NULL,
  upper = NULL
)

test_psx <- new_builtin_type(
  type_class = "posixct",
  type_desc = c("a", "{.cls POSIXct} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = NULL,
  upper = NULL
)

lst_of_psx <- new_list_of_type(psx)

test_lst_of_psx <- new_list_of_type(test_psx)

# dte --------------------------------------------------------------------------

dte <- new_builtin_type(
  type_class = "date",
  type_desc = c("a", "{.cls Date} vector"),
  fun_is = "check",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = NULL,
  upper = NULL
)

test_dte <- new_builtin_type(
  type_class = "date",
  type_desc = c("a", "{.cls Date} vector"),
  fun_is = "test",
  len = NULL,
  nas = TRUE,
  null = FALSE,
  min_len = NULL,
  max_len = NULL,
  lower = NULL,
  upper = NULL
)

lst_of_dte <- new_list_of_type(dte)

test_lst_of_dte <- new_list_of_type(test_dte)

# fac --------------------------------------------------------------------------

# TODO: Factor

# byt --------------------------------------------------------------------------

byt <- new_builtin_type(
  type_class = "raw",
  type_desc = c("a", "{.cls raw} vector"),
  fun_is = "check",
  len = NULL,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

test_byt <- new_builtin_type(
  type_class = "raw",
  type_desc = c("a", "{.cls raw} vector"),
  fun_is = "test",
  len = NULL,
  null = FALSE,
  min_len = NULL,
  max_len = NULL
)

lst_of_byt <- new_list_of_type(byt)

test_lst_of_byt <- new_list_of_type(test_byt)

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
  mapply(
    FUN = intish,
    x = x,
    x_name = sprintf("%s[[%i]]", x_name, seq_along(x)),
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
    x_must = "be a whole number",
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

test_string <- function(x) {
  checkmate::qtest(x, "S1")
}

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

# helpers ----------------------------------------------------------------------

is_type_check <- function(x) {
  inherits(x, "specifyr_type_check")
}

is_type_test <- function(x) {
  inherits(x, "specifyr_type_test")
}

# interactive testing ----------------------------------------------------------

if (FALSE) {

  library(ivs)

  ivv <- new_vector_fun(
    type_test = is_iv(x),
    type_desc = c("an", "interval vector"),
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    lower = NULL,
    upper = NULL,

   # TODO: Allow checks to be added in the ... arguments!!

    # !!! You need to make a way for:
    # 1. Checks to accept additional arguments
    # 2. A check to be used as a named argument to `new_vector_property`!!!
    #
    # User provides this...
    # `intersects = check_iv_intersects(x, intersects = intersects)`
    #
    # In the function at the check stage we do...
    # if (!is_missing(intersects) && test_iv_intersects(x, intersects)) {
    #   emit_iv_intersects_stop()
    # }
    #
    # The argument looks like `intersects = rlang::missing_arg()`

    # Users can provide unnamed checks, which are always run! Or a named check,
    # which is only run when the argument is supplied!!!
  )

  my_iv <- ivs::iv(1:5, 7:11)
  ivv(my_iv, len = 4L)
}

