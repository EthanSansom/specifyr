
`%notin%` <- Negate(`%in%`)

indices <- function(x) {
  names <- rlang::names2(x)
  named_at <- names != ""
  out <- as.list(seq_along(x))
  out[named_at] <- names[named_at]
  out
}
