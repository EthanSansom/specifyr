`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

`%!|%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%|0%` <- function(lhs, rhs) if (is_empty(lhs)) rhs else lhs

`%!0%` <- function(lhs, rhs) if (is_empty(lhs)) lhs else rhs

`%notin%` <- Negate(`%in%`)

is_empty <- function(x) length(x) == 0

if_true <- function(x, pass, fail) if (isTRUE(x)) pass else fail

if_false <- function(x, pass, fail) if (isFALSE(x)) pass else fail

if_null <- function(x, pass, fail) if (is.null(x)) pass else fail

# Same as `identical()`, but handles missing arguments explicitly.
# `identical(rlang::missing_arg(), rlang::missing_arg())` is `TRUE`, but
# `x <- y <- rlang::missing_arg(); identical(x, y)` throws an error.
identical2 <- function(x, y) {
  x_missing <- rlang::is_missing(x)
  y_missing <- rlang::is_missing(y)
  (x_missing && y_missing) || (!(x_missing || y_missing) && identical(x, y))
}

all_identical <- function(elements) {
  if (length(elements) <= 1) {
    return(TRUE)
  }
  first <- elements[[1]]
  all(purrr::map_lgl(elements[-1], identical2, first))
}
