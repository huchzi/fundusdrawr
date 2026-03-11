render_objects <- function(object_list) {
  if (is.null(object_list)) {
    return("")
  }

  render <- function(obj) {
    switch(obj$type,
      tear = tear(obj), # |> read_xml(),
      equatorial = equatorial_degeneration(obj), # |> read_xml(),
      detachment = closed_form(obj$path) # |> read_xml()
    )
  }

  purrr::map_chr(object_list, render)
}
