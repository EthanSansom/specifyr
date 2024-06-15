# constructor ------------------------------------------------------------------

blueprint <- function(x) {
  attr(x, "blueprint")
}

is_object_spec <- function(x) {
  inherits(x, "specifyr_object_spec")
}

#' @export
new_spec <- function(blueprint, ...) {
  UseMethod("new_spec")
}

# TODO NOTE: To keep the naming clear, where possible we should keep the
# names of specification functions as `*_spec` and the names of functions
# that *act* on specifications as `spec_*`.
# - `vector_spec`, `object_spec`, etc.
# vs.
# - `spec_pluck`, `spec_error`, etc.

# TODO Implement:
# - as.function     -> unclass the `*_spec` or `check` (i.e. remove the blueprint)
# - as_test         -> convert a `*_spec` or `check` into a test (i.e. returns FALSE/TRUE)
# - as_spec         -> convert a `test` or a `check` into a `*_spec`
# - as_check        -> convert a `test` or a `*_spec` into a `check`
#
# To turn a `check` into a `*_spec`, just convert to an object specification with
# no `cls`, `len`, etc. set. These converted classes will need an intelligent blueprint
# so that we can switch the *back* in an intelligent way.
#
# ... this might be complicating things. Maybe dial down the terminology? Maybe
# specifications are really just `check` objects, or a sub-class of them. I'm
# not sure.

# TODO Implement:
# - `bool_spec`, `string_spec`, `count_spec`, `integerish_spec`, `logicalish_spec`
#   - specifications for common object types, with slightly more customized error messages
#
# - `list_of_spec`
#   - list of one type of object (i.e. one specification)
#   - allows for more informative error messages
#
# TODO `list_of_spec`
# - accepts other `list_of_spec` and other non-recursive specifications
# - formats errors with multiple problems (per blueprint item, ex. `cls`, `len`, etc.)
#
#> list_of_integers <- list_of_spec(vec_spec("integer", nas = FALSE))
#> x <- list(10L, 1:5, NA_integer, c(1, NA_interger, NA_integer), 10L)
#> list_of_integers(x)
# Error:
# ! `x` must be a list of non-NA integers.
# i `x` is a list.
# x `x[[3]]` is an integer which is NA at location `1`.
# x `x[[4]]` is an integer which is NA at locations `c(2, 3)`.
#
# NOTE: In this case, we report all `cls` errors, instead of the `nas` errors
#> y <- list("A", "B", NA_integer, 10L, "E", c(1L, NA_integer), "F", G", H")
#> list_of_integers(y)
# Error:
# ! `y` must be a list of non-NA integers.
# i `y` is a list.
# x `y[[1]]` is a character vector.
# x `y[[2]]` is a character vector.
# x `y[[5]]` is a character vector.
# ... and 2 more problems.
#
# NOTE: Here, we proceed as normal.
#> list_of_integer(10)
# Error:
# ! `10` must be a list of non-NA integers.
# x `10` is a <numeric> vector.
