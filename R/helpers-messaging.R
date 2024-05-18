commas <- function(x) {
  paste(x, collapse = ", ")
}

oxford <- function(x, sep = ", ", sep2 = " and ", last = sep2) {
  x_len <- length(x)
  if (x_len <= 2) {
    paste(x, collapse = sep2)
  } else {
    paste0(paste(x[-x_len], collapse = sep), sep, last, x[[x_len]])
  }
}

numbered <- function(x, from = 1) {
  x_length <- length(x)
  if (x_length <= 0) {
    return("")
  }
  numbers <- paste0(seq(from, from + x_length - 1), ".")
  numbers <- style_subtle(formatC(numbers, width = max(nchar(numbers))))
  paste(numbers, x)
}

at_locations <- function(locations, n_max = 5) {
  if (is.logical(locations)) {
    locations <- as.numeric(which(vapply(locations, isTRUE, logical(1L))))
  }
  specifyr_internal_error(locations, "is_integerish", "rlang")
  n_locations <- length(locations)
  prefix <- if (min(n_locations, n_max) == 1) "at location " else "at locations "
  if (n_locations > n_max) {
    at <- deparse(locations[seq(n_max)])
    paste0(prefix, "`", at, "` and ", n_locations - n_max, " more")
  } else {
    paste0(prefix, "`", deparse(locations), "`")
  }
}

format_index <- function(index) {
  specifyr_internal_error(index, "is.list")
  if (is_empty(index)) {
    ""
  } else {
    paste(paste0("[[", purrr::map_chr(index, deparse), "]]"), collapse = "")
  }
}
