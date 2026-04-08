roundhole <- function(obj, parent) {
  clock <- 30 * (obj$clock - 3) + 90
  ecc <- obj$ecc

  # Transformationen analog zu rotateElement $ translateElement
  transform_str <- glue::glue("rotate({clock} 200 200) translate(0 -{ecc})")

  xml_add_child(parent,
    "circle",
    r = 5,
    cx = "200",
    cy = "200",
    fill = "red",
    stroke = "blue",
    `stroke-width` = "2",
    transform = transform_str
  )
}
