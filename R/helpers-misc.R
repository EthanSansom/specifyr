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
