# friendly ---------------------------------------------------------------------

at_loc_friendly <- function(loc, n_max = 5) {
  loc <- if (is.logical(loc)) which(loc & !is.na(loc)) else loc
  loc <- as.numeric(loc)
  n <- length(loc)
  at <- ngettext(min(n, n_max), "at location ", "at locations ")
  if (n > n_max) {
    paste0(at, "`", deparse(loc[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(loc), "`")
  }
}

# TODO: Revise this to include a `min_len` and `max_len` argument.
a_length_n_friendly <- function(len) {
  if (is.null(len)) {
    "a"
  } else if (length(len) == 2) {
    len <- format_len(len)
    paste0("a length [", len[[1]], "-", len[[2]], "]")
  } else if (len == 1) {
    "a scalar"
  } else if (len > 0) {
    paste("a length", format_len(len))
  } else {
    "an empty"
  }
}

# TODO: Revise this to include a `min_len` and `max_len` argument.
length_n_friendly <- function(len) {
  if (is.null(len)) {
    ""
  } else if (length(len) == 2) {
    len <- format_len(len)
    paste0("length [", len[[1]], "-", len[[2]], "]")
  } else if (len == 1) {
    "length 1"
  } else if (len > 0) {
    paste("length", format_len(len))
  } else {
    "empty"
  }
}

format_len <- function(x) format(x, scientific = FALSE, big.mark = ",")

in_range_friendly <- function(lower, upper) {
  no_lower <- is.null(lower) || is.infinite(lower)
  no_upper <- is.null(upper) || is.infinite(upper)
  if (no_lower && no_upper) {
    ""
  } else if (no_lower) {
    paste("equal to or less than", upper)
  } else if (no_upper) {
    paste("equal to or greater than", lower)
  } else {
    paste0("in range [", lower, ", ", upper, "]")
  }
}

a_vector_friendly <- function(
    type_desc,
    len = NULL,
    nas = TRUE,
    null = FALSE,
    min_len = NULL,
    max_len = NULL,
    upper = NULL,
    lower = NULL,
    finite = FALSE
  ) {

  null_or <- if (isTRUE(null)) "`NULL` or" else ""
  a_len_n <- a_length_n_friendly(len %||% c(min_len, max_len))
  non_na <- if (isFALSE(nas)) "non-NA" else ""
  finite <- if (isTRUE(finite)) "finite" else ""
  in_range <- in_range_friendly(lower, upper)

  # Allows `type_desc` to use the correct article (i.e. "an integer", not "a integer")
  if (length(type_desc) == 1) type_desc <- c("a", type_desc)
  if (a_len_n == "a" && non_na == "" && finite == "") {
    a_len_n <- type_desc[[1]]
    type_desc <- type_desc[[2]]
  } else {
    type_desc <- type_desc[[2]]
  }

  cli::format_inline(
    paste(null_or, a_len_n, non_na, finite, type_desc, in_range),
    keep_whitespace = FALSE
  )
}

# misc -------------------------------------------------------------------------

upper1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

commas <- function(x, n = NULL) {
  if (!is.null(n) && length(x) > n) {
    length(x) <- n
    paste0(paste(x, collapse = ", "), ", ...")
  } else {
    paste(x, collapse = ", ")
  }
}

oxford <- function(x, sep = ", ", last = "or", n = NULL) {
  x_len <- length(x)
  if (x_len <= 1) {
    return(paste(x))
  }
  if (!is.null(n) && x_len > n) {
    length(x) <- n
    paste0(paste(x, collapse = sep), sep, " ...")
  }
  if (x_len == 2) sep <- " "
  paste(paste(x[-x_len], collapse = sep), last, x[[x_len]], sep = sep)
}
