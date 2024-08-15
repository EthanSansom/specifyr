# friendly ---------------------------------------------------------------------

at_location_friendly <- function(x, trunc = 5) {
  if (is.logical(x)) {
    is.na(x) <- FALSE
    x <- which(x)
  }
  if (!rlang::is_integerish(x)) {
    stop("`x` must be integerish.")
  }

  if (length(x) > trunc) {
    and_more <- paste(" and", length(x) - trunc, "more")
    length(x) <- trunc
  } else {
    and_more <- ""
  }

  n <- length(x)
  if (!n) {
    return(paste0("at location `c()`", and_more))
  } else if (n <= 1) {
    return(paste0("at location `", x, "`", and_more))
  }
  paste0("at locations `c(", commas(x), ")`", and_more)
}

# misc -------------------------------------------------------------------------

commas <- function(x) {
  paste(x, collapse = ", ")
}

oxford <- function(x, sep = ", ", last = "or", trunc = Inf) {
  x_len <- length(x)
  if (x_len <= 1) {
    return(paste(x))
  } else if (x_len > trunc) {
    length(x) <- trunc
    return(paste(paste(x[-trunc], collapse = sep), "...", paste(last, x[[trunc]]), sep = sep))
  }
  if (x_len == 2) sep <- " "
  paste(paste(x[-x_len], collapse = sep), paste(last, x[[x_len]]), sep = sep)
}
