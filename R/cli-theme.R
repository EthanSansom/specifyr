# You can see how {usethis} implements a custom theme here:
# - https://github.com/r-lib/usethis/blob/main/R/utils-ui.R
specifyr_theme <- function() {

  builtin <- cli::builtin_theme()
  bullets <- builtin[grepl("^\\.bullets \\.bullet-(i|v|!|x|v|\\*|>)", names(builtin))]
  bullet_symbols <- gsub(".*\\.bullet-(i|v|!|x|v|\\*|>)$", "\\1", names(bullets))

  # Single indented and double indented versions of {cli} bullets. E.g. `v` is a
  # check with no indent, `vv` is single indented, `vvv` is double indented.
  single_indent_bullets <- bullets |>
    purrr::map(\(bullet) c(bullet, `margin-left` = 2)) |>
    rlang::set_names(paste0(names(bullets), bullet_symbols))
  double_indent_bullets <- bullets |>
    purrr::map(\(bullet) c(bullet, `margin-left` = 4)) |>
    rlang::set_names(paste0(names(bullets), strrep(bullet_symbols, 2)))

  rlang::list2(
    !!!single_indent_bullets,
    !!!double_indent_bullets,

    # E.g. "at locations `c(1, 2, 3, 4, 5)` and 5 more"
    span.at = list(
      transform = function(x) at_location_friendly(x)
    ),

    # "{.a {x}}" is shorthand for "{.obj_type_friendly {x}}"
    span.a = builtin$span.obj_type_friendly
  )
}
