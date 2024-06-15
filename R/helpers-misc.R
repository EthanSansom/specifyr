`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

`%!|%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%notin%` <- Negate(`%in%`)

`%missing%` <- function(lhs, rhs) if (rlang::is_missing(lhs)) rhs else lhs

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

is_empty <- function(x) length(x) == 0

if_true <- function(true, pass, fail) if (isTRUE(true)) pass else fail

if_false <- function(false, pass, fail) if (isFALSE(false)) pass else fail

if_null <- function(null, pass, fail) if (is.null(null)) pass else fail

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
