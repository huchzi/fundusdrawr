render_objects <- function(object_list) {
  if (is.null(object_list)) {
    return("")
  }

  render <- function(obj) {
    print(obj)
    obj_string <-
    switch(obj$type,
      tear = tear(obj),
      equatorial = equatorial_degeneration(obj),
      detachment = closed_form(obj$path)
    )
    print(read_xml(obj_string))
    read_xml(obj_string)
  }

  purrr::map(object_list, render)
}
