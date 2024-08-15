

# TODO: I want to revise this so that you can give a `check` as a named argument to `...`


# Concept:
#
# We have *types* which are the storage mode of an object. They are usually tested
# using an `is_*()` or `is.*()` function. Then, we have *properties*, which are
# characteristics of individual instances of a *type*. The type of an object
# determines the set of properties that it can have.
#
# For example, being of type `data.frame` allows an object to have the "number of
# rows" property.
#
# {specfiyr} provides generators to quickly create functions for checking or testing
# the type and properties of arbitrary objects in R. For common objects, such as
# `data.frame`s, vectors, and `list`s, we provide more tools to help speed up the
# process.
#
# The `new_vector_fun()` and `new_object_fun()`, functions:
# These are generators used to define a type-checking function for either a
# vector or arbitrary object in R.
#
# - vector: something vector-y. `new_vector_fun()` has included arguments for
#   checking the following properties of a vector:
#   - is it actually null
#   - length
#   - contains NAs/NaNs
#   - within some bounds (upper and lower)
#   - is it finite (might exclude this one)
# These builtin-properties are also reported differently in error messages. When
# the object is of the correct type, but these property messages fail, then they
# are all bundled together into a single message.
#
# - object: anything in R. `new_object_fun()` has included arguments for:
#   - is it actually null
#
# The `new_check()` function:
# This is a generator used to define a property-checking function.
#
#
# HOLEY SHIT! What if I make the whole thing true-ly modular. I can define those
# built-in properties using checks...
#
# Okay... Here's supplying a property check to a type function.

# What does the `new_check()` interface look like?
if (FALSE) {
  # Is this a nice interface for `check()`? I don't like `stopifnot` style because
  # the names take up so much space.
  check(
    header = "{.arg {x_name}} must be a even number between 0 and 100.",
    "{.arg {x_name}} is not even {.at {x %% 2 != 0}}." = all(x %% 2 == 0),
    "{.arg {x_name}} is not not between 0 and 100 {.at {!between(x, 0, 100)}}." = all(0 <= x & x <= 100)
  )

  # This way, you can do all of the blocking stuff with `x_blk` or whatever and
  # this doesn't take a million lines.
  check(
    tests = c(all(x %% 2 == 0), all(0 <= x & x <= 100)),
    header = "{.arg {x_name}} must be a even number between 0 and 100.",
    bullets = c(
      x = "{.arg {x_name}} is not even {.at {x %% 2 != 0}}.",
      x = "{.arg {x_name}} is not not between 0 and 100 {.at {!between(x, 0, 100)}}."
    )
  )

  # I want a nice `or` message. Like, `x` must be a function or purrr-style lambda.
  # OOOOOOHHH, this is a good bet. Go around looking at existing nice and niche
  # error messages in the wild, and see how many you can easily define with this
  # interface.

  # Three new boys...
  # - test_and: the default for check tests, all are checked and `x` must have ALL (like `&&`)
  # - test_or: only one of the tests must pass (like `||`)
  #
  # How would the `test_or` error message work? What are some standard bullets options.

  # In this case, the or test needs only one resolution since both tests are type tests
  # we can resolve both of them by giving a single bullet.
  #
  #> ! x` must be a function or a purrr-style lambda.
  #> x `x` is a character.

  # You might want to lead the user down the full path of options which lead to
  # the error. Here, `x` IS a number, but it's sometimes odd.
  #
  #> ! `x` must be an even number or a five letter string.
  #> i `x` is a number.
  #> x `x` is odd at locations `c(1, 2, 3)`

  # Sort of tricky how to split these OR cases nicely...
  # is.numeric(x) && x %% 2 == 0 || is.character(x) && length(x) == 1L && nchar(x) == 5

  # Actually, this kind of thing would really occur in a type test, in which case
  # you could check if it passed either of the type tests (i.e. is.numeric(x) or
  # is.character(x)) and if so, go down the path that it passed. Otherwise, just
  # emit the type failure message for the first condition.
}



new_vector_fun <- function(
    # NOTE: Make sure it's clear in the documentation that `type_test` should
  #       return FALSE on `NULL` values, to prevent `NULL` values from having
  #       their length, bounds, etc. tested (possibly throwing an error). This
  #       is usually the case for `is_*()` and `is.*()` functions anyways.
  type_test,
  type_desc,
  ...,
  fun_is = c("check", "test", "alias", "aliaser"), # TODO: Create `alias` option
  checks = list(),
  x_name = NULL,
  error_header = NULL,
  error_bullets = NULL,
  error_class = "specifyr_error_mistyped"
) {

  parent_env <- rlang::caller_env()

  type_test <- rlang::enexpr(type_test)
  type_desc <- chr_(type_desc, min_len = 1, max_len = 2, nas = FALSE)
  fun_is <- rlang::arg_match(fun_is)
  checks <- check_is_list_of_check(checks)
  error_class <- chr_(error_class, nas = FALSE)
  x_name <- chr_(x_name, len = 1, nas = FALSE, null = TRUE)
  error_header <- chr_(error_header, len = 1, nas = FALSE, null = TRUE)
  error_bullets <- chr_(error_bullets, nas = FALSE, null = TRUE)

  properties <- check_property_dots(...)
  property_nms <- rlang::names2(properties)

  # The `test` part of supplied checks, each is an expression like `x %% 2 == 0`
  check_tests <- lapply(checks, as_test)

  # Expression like `is.Date(x) && length(x) == len && ...`
  type_and_prop_test <- make_base_variable_properties_test(type_test, property_nms)

  # `is.Date(x) && length(x) == len && ... && check_test1(x) && ...`
  test <- expr_and(type_and_prop_test, !!!check_tests)

  # Return early if we're generating a test function
  if (fun_is == "test") {
    out <- rlang::new_function(
      args = rlang::pairlist2(x = , !!!properties),
      body = test,
      env = parent_env
    )
    class(out) <- c("specifyr_type_test", "function")
    return(out)
  }

  # `stop_vector_mistyped(x, x_name, len = len, nas = nas, ...)`
  stop_mistyped <- make_variable_stop_vector_mistyped(
    type_desc = type_desc,
    type_test = type_test,
    property_nms = property_nms
  )

  if (!is_empty(checks)) {
    # `x` failed either the `type_and_prop_test` or one of `checks`. We retest
    # `x` to determine whether to raise a mistyped error or a check error.
    stop_mistyped <- rlang::expr(if (!(!!type_and_prop_test)) { !!stop_mistyped })

    # ith element is `if (!check_tests[[i]]) { check_stops[[i]](x, x_name, ...) }`
    stop_if_checks_failed <- .mapply(
      FUN = \(stop, test) { rlang::expr(if (!(!!test)) { !!stop }) },
      dots = list(
        stop = lapply(checks, as_stop),
        test = check_tests
      ),
      MoreArgs = list()
    )
  } else {
    stop_if_checks_failed <- list()
  }

  #> # Return `x` if the type, properties, and checks are correct
  #> if (is.numeric(x) && (nas || !anyNA(x)) && check_test1(x)) { return(x) }
  #>
  #> # Raise a mistyped error if the type or properties are incorrect
  #> if (!(is.numeric(x) && (nas || !anyNA(x))) { stop_mistyped(x, ...) }
  #>
  #> # Raise a check error if the corresponding check failed
  #> if (!check_test1(x)) { stop_check1(x, ...) }`
  body <- expr_squash(
    rlang::expr(if (!!test) { return(x) }),
    stop_mistyped,
    !!!stop_if_checks_failed,
    # Type check function should never reach this stage, we raise an error if so
    quote(specifyr::stop_malformed_type_check())
  )
  args <- rlang::pairlist2(
    x = ,
    !!!properties,
    error_call = quote(rlang::caller_env()),
    error_class = error_class,
    x_name = x_name %||% quote(rlang::caller_arg(x)),
    error_header = error_header,
    error_bullets = error_bullets
  )
  out <- rlang::new_function(
    args = args,
    body = body,
    env = parent_env
  )
  class(out) <- c("specifyr_type_check", "function")
  out
}
