# Testing ----------------------------------------------------------------------

# I want a way to convert any check into checking a list of that thing. These
# might be most easily done as ADVERBS. Like `purrr::quietly()`, `purrr::negate()`,
# etc. These will allow the interior code to not be extremely bloated.

# Define a test function, we're going to vectorize this over a list
check_above <- check(
  test = info(
    test(is.null(above) || all(x > above), x = , above = NULL),
    fail = "{.arg {x_name}} has a minimum of {min(x, na.rm = TRUE)}."
  ),
  header = "{.arg {x_name}} must be greater than {.val {above}}."
)

# Note that {purrr} uses an internal `as_mapper` function to capture a function
# in a specified environment. They reference the function as `.f`.
purrr:::as_mapper.default
purrr::negate

check_above_vectorized <- vectorize_check(check_above)

x_lst <- list(53, a = 1:10, 2:11)
check_above_vectorized(x_lst, above = 20)

# Vectorize a check
# TODO: `along` should be able to take multiple arguments. see the implementation
#       of `base::Vectorize`. We can add a special index to each of them.
vectorize_check <- function(fn, along) {

  stopifnot(is_check(fn))
  env <- rlang::env()

  args <- rlang::fn_fmls(fn)
  main_args <- main_args(fn)
  main_arg_names <- rlang::names2(main_args)

  if (rlang::is_missing(along)) {
    along <- main_arg_names[[1]]
    along_sym <- rlang::sym(along)
    along_name_sym <- rlang::sym(paste0(along, "_name"))
  } else {
    # TODO: Make this `stop()` better
    stopifnot(rlang::is_string(along) && along %in% main_arg_names)
    along_sym <- rlang::sym(along)
    along_name_sym <- rlang::sym(paste0(along, "_name"))
  }

  # `more_args` are the other arguments forwarded to `lapply`
  more_args <- forward_pairlist(args)
  # - remove the `along` and `along_name` argument, which are included in `dots`
  more_args <- more_args[rlang::names2(more_args) %notin% c(along, paste0(along, "_name"))]

  # We `.mapply` along `along` and we return `along` as well.
  dots <- rlang::set_names(
    list(
      along_sym,
      # TODO: Need to namespace *every* internal {specifyr} function used
      rlang::expr(paste0(!!along_name_sym, "[[", pretty_indices(!!along_sym), "]]"))
    ),
    nm = c(along, paste0(along, "_name"))
  )
  body <- rlang::expr({
    .mapply(
      FUN = fn,
      dots = list(!!!dots),
      MoreArgs = list(!!!more_args)
    )
    return(!!along_sym)
  })

  rlang::new_function(
    args = args,
    body = body,
    env = env
  )
}

# Create a pairlist of the form, `arg = arg`, given a list/pairlist of args.
forward_pairlist <- function(args) {
  arg_nms <- rlang::names2(args)
  rlang::pairlist2(!!!rlang::set_names(rlang::syms(arg_nms), arg_nms))
}

# Retrieve the main (i.e. user-supplied) arguments of a test, info, or check
main_args <- function(x) {
  stopifnot(is_test(x) || is_info(x) || is_check(x))
  if (is_test(x)) {
    return(rlang::fn_fmls(x))
  }
  args <- rlang::fn_fmls(x)
  first_name_arg <- which.max(grepl("_name$", rlang::names2(args)))
  args[sequence(first_name_arg - 1)]
}

# Retrieve the name (i.e. `arg_name`) arguments of a test, info, or check
name_args <- function(x) {
  stopifnot(is_info(x) || is_check(x))
  purrr::keep_at(rlang::fn_fmls(x), grepl("_name$", rlang::names2(args)))
}

pretty_indices <- function(x) {
  x_nms <- rlang::names2(x)
  is_named <- x_nms != ""
  if (!any(is_named)) {
    return(as.character(seq_along(x)))
  }
  indices <- encodeString(x_nms, quote = "\"")
  indices[!is_named] <- which(!is_named)
  indices
}
