commas <- function(x) {
  paste(x, collapse = ", ")
}

oxford <- function(x, sep = ", ", sep2 = " and ", last = sep2) {
  x_len <- length(x)
  if (x_len <= 2) {
    paste(x, collapse = sep2)
  } else {
    paste0(paste(x[-x_len], collapse = sep), last, x[[x_len]])
  }
}

c_string <- function(x) {
  if (is.null(x)) {
    "`NULL`"
  } else if (length(x) == 1) {
    paste0("`", x, "`")
  } else {
    paste(c("`c(", commas(x), ")`"), collapse = "")
  }
}

at_locations <- function(at) {
  n_locations <- length(at)
  if (n_locations == 1) {
    msg <- paste0("at location: `", at, "`")
  } else if (n_locations > 5) {
    loc <- c_string(at[1:5])
    msg <- paste0("at locations: ", loc, " and ", n_locations - 5, " more")
  } else {
    loc <-
      msg <- paste0("at locations: ", c_string(at), ".")
  }
  msg
}

as_index <- function(..., .recycled = FALSE) {
  dots <- purrr::map_if(
    rlang::list2(...),
    .p = is.character,
    .f = \(x) paste0("'", x, "'")
  )
  if (.recycled) {
    n_dots <- length(dots)
    first_dots <- if (n_dots <= 1) "" else paste0("[[", dots[-n_dots], "]]")
    last_dot <- dots[[n_dots]]
    last_dot <- if (is.character(last_dot)) {
      paste0("[", dots[[n_dots]], "]")
    } else {
      paste0("[>=", dots[[n_dots]], "]")
    }
    return(paste(c(first_dots, last_dot), collapse = ""))
  }
  paste(paste0("[[", dots, "]]"), collapse = "")
}

style_subtle <- cli::make_ansi_style("#A9A9A9")

numbered <- function(x, from = 1) {
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

# fmt --------------------------------------------------------------------------

fmt_nas <- function(nas) {
  if (nas) "" else "[non-NA]"
}

fmt_opt <- function(opt) {
  if (opt) "[optional]" else ""
}

fmt_cls <- function(cls) {
  if (is.null(cls)) "" else paste(cls, collapse = "|")
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
