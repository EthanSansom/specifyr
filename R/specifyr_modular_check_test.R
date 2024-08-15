
library(bench)
library(rlang)

f1 <- function(x) {

  if (
    (is.numeric(x) && length(x) == 1) ||
    (is.character(x) && length(x) == 5)
  ) {
    return(x)
  }

}


f2 <- function(x, error_header = "x must be a length 1 number or a length 5 character") {

  # NOTE: When propositions like this are used within a `specifyr` generated function,
  #       we'll just make sure that the `p*` are incremented so we never need to worry
  #       about them interrupting one another. This might present issues if the user
  #       has included one of these symbols in their test expression...
  #
  # Record the number of propositions checked. A higher number of checks means
  # that this is the more relevant path.
  if (
    ((p1 <- is.numeric(x)) && (p2 <- length(x) == 1)) ||
    ((p3 <- is.character(x)) && (p4 <- length(x) == 5))
  ) {
    return(x)
  }

  # Find out which test had the greatest number of propositions checked
  most_checked <- which.max(
    c(
      sum(rlang::env_has(nms = c("p1", "p2"))),
      sum(rlang::env_has(nms = c("p3", "p4")))
    )
  )

  # Depending on which option was the most checked, emit the appropriate error
  if (most_checked == 1) {

    p1 <- env_get(nm = "p1", default = NULL)
    p2 <- env_get(nm = "p2", default = NULL)

    bullets <- c(
      # If `x` was a number, supply this as a hint, otherwise show the `x is not a number` message
       if (isTRUE(p1)) {
        c(v = "x is a number.")
      } else if (isFALSE(p1)) {
        c(x = "x is {.obj_type_friendly {x}}.")
      },
      # If `x` is a number and the length was checked, then show the incorrect length message
      if (isFALSE(p2)) {
        c(x = "x is length {length(x)}")
      }
    )

  }

  if (most_checked == 2) {

    p3 <- env_get(nm = "p3", default = NULL)
    p4 <- env_get(nm = "p4", default = NULL)

    bullets <- c(
      # If `x` was a character, supply this as a hint, otherwise show the
      # `x is not a character` message
      if (isTRUE(p3)) {
        c(v = "x is a character")
      } else if (isFALSE(p3)) {
        c(x = "x is {.obj_type_friendly {x}}.")
      },
      # If `x` is a character and the length was checked, then show the incorrect length message
      if (isFALSE(p4)) {
        c(x = "x is length {length(x)}")
      }
    )

  }

  # Emit the error
  cli::cli_abort(
    c(error_header, bullets)
  )
}

# The second version has a *tiny* ~100ns penalty because of the test
# assignment to `p*`, which isn't a problem at all.
bench::mark(
  f1(c("A", "B", "C", "D", "E")),
  f2(c("A", "B", "C", "D", "E"))
)
