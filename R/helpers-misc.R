`%notin%` <- Negate(`%in%`)

`%missing%` <- function(lhs, rhs) if (rlang::is_missing(lhs)) rhs else lhs

# TODO: Not sure if this is correct or useful. FIX
expr_squash <- function(...) {
  dots <- purrr::compact(rlang::list2(...))
  bquote({ ..(dots) }, splice = TRUE)
}
