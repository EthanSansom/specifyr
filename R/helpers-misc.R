`%notin%` <- Negate(`%in%`)

indices <- function(x) {
  names <- rlang::names2(x)
  # Provide numeric indices for duplicate names, since `x['a']` only accesses
  # the first element named "a" in `x`.
  named_at <- names != "" & !duplicated(names)
  out <- as.list(seq_along(x))
  out[named_at] <- names[named_at]
  out
}

len_type <- function(len) {
  if (is.null(len)) {
    "any"
  } else if (length(len) == 2) {
    "range"
  } else {
    "exact"
  }
}
