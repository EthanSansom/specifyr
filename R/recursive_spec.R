is_rec_spec <- function(x) {
  inherits(x, "specifyr_rec_spec")
}

compact_elements <- function(elements) {

  # Don't need to compact if < 2 elements or if the last element is named
  i <- length(elements)
  nms <- rlang::names2(elements)
  if (i <= 1 || nms[[i]] != "") {
    return(elements)
  }

  # Remove identical same named elements from the tail of the blueprint
  last_element <- elements[[i]]
  while (i > 1) {
    if (nms[[i - 1]] != "") {
      break
    }
    if (!identical(last_element, elements[[i - 1]])) {
      break
    }
    i <- i - 1
  }
  vctrs::vec_slice(elements, seq(i))
}
