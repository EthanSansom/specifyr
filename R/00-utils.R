`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

`%!|%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%|?%` <- function(lhs, rhs) if (rlang::is_missing(lhs)) rhs else lhs

`%!?%` <- function(lhs, rhs) if (rlang::is_missing(lhs)) lhs else rhs

`%notin%` <- Negate(`%in%`)

is_empty <- function(x) length(x) == 0

if_true <- function(x, pass, fail) if (isTRUE(x)) pass else fail

if_false <- function(x, pass, fail) if (isFALSE(x)) pass else fail

if_null <- function(x, pass, fail) if (is.null(x)) pass else fail

try_is_true <- function(expr) {
  isTRUE(rlang::try_fetch(expr, error = function(cnd) NULL))
}

try_null <- function(expr, null_if = ~ FALSE) {
  null_if <- rlang::as_function(null_if)
  result <- rlang::try_fetch(expr, error = function(cnd) NULL)
  if (is.null(result) || isTRUE(null_if(result))) {
    NULL
  } else {
    result
  }
}

blueprint <- function(x) {
  attr(x, "blueprint")
}

`blueprint<-` <- function(x, value) {
  attr(x, "blueprint") <- value
  x
}
