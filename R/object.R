# constructor ------------------------------------------------------------------

blueprint <- function(x) {
  attr(x, "blueprint")
}

is_object_spec <- function(x) {
  inherits(x, "specifyr_object_spec")
}

# TODO: Implement
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
