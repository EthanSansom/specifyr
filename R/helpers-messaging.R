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
