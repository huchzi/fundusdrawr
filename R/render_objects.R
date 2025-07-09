render_objects <- function(object_list) {

  if (is.null(object_list)) return("")

  render <- function(obj) {
    switch(obj$type,
           tear = tear(obj),
           detachment = detachment(obj))
  }

  purrr::map_chr(object_list, render) |>
    stringr::str_c(collapse = "\n")
}
