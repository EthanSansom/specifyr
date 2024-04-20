style_subtle <- cli::make_ansi_style("#A9A9A9")

numbered <- function(x, from = 0) {
  x_length <- length(x)
  if (x_length <= 0) {
    return("")
  }
  numbers <- paste0(seq(from, from + x_length - 1), ".")
  numbers <- style_subtle(formatC(numbers, width = max(nchar(numbers))))
  paste(numbers, x)
}

lettered <- function(x) {
  x_length <- length(x)
  if (x_length == 0) {
    return("")
  }
  # Creates sequence A-Z, AA-ZA, AB-ZB, ..., up to AZ-ZZ (x_length <= 702)
  first_letters <- LETTERS[rep_len(1:26, x_length)]
  next_letters <- c("", LETTERS)[1 + ((seq(x_length) - 1) %/% 26)]
  letters <- paste0(first_letters, next_letters, ".")
  letters <- style_subtle(formatC(letters, width = max(nchar(letters))))
  paste(letters, x)
}

fmt_nas <- function(nas) {
  if (nas) "" else "[non-NA]"
}

fmt_opt <- function(opt) {
  if (opt) "[optional]" else ""
}

fmt_cls <- function(cls) {
  if (is.null(cls)) "" else paste(cls, collapse = "/")
}

fmt_len <- function(len) {
  switch(
    len_type(len),
    any = "",
    range = paste0("[", len[[1]], "-", len[[2]], "]"),
    exact = paste0("[", len, "]")
  )
}

fmt_checks <- function(checks) {
  if (is_empty(checks)) "" else "[*]"
}
