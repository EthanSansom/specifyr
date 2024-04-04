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
    last_dot <- paste0("[>=", dots[[n_dots]], "]")
    return(paste(c(first_dots, last_dot), collapse = ""))
  }
  paste(paste0("[[", dots, "]]"), collapse = "")
}
