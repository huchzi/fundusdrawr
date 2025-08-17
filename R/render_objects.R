render_objects <- function(object_list) {
  if (is.null(object_list)) {
    return("")
  }

  render <- function(obj) {
    switch(obj$type,
      tear = tear(obj),
      equatorial = equatorial_degeneration(obj),
      detachment = closed_form(obj$path)
    )
  }

  purrr::map_chr(object_list, render) |>
    stringr::str_c(collapse = "\n")
}
