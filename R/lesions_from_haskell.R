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

horseshoe_path <- function(size) {
  gr <- ifelse(size == "large", 20, 12)
  sr <- ifelse(size == "large", 15, 8)

  xpos <- 200
  ypos <- 200

  leftBorder <- xpos - sr
  rightBorder <- xpos + sr

  paste0(
    "M", leftBorder, ",", ypos,
    " A", gr, ",", gr, " 0 0,0 ", rightBorder, ",", ypos,
    " A", sr, ",", sr, " 0 0,1 ", leftBorder, ",", ypos,
    " Z"
  )
}

horseshoe_element <- function(tear_obj) {
  path <- horseshoe_path(tear_obj$size)

  attrs <- c(
    d = path,
    fill = "red",
    stroke = "blue",
    `stroke-width` = "2",
    transform = paste0(
      "rotate(", tear_obj$clock, ") ",
      "translate(", tear_obj$ecc, ")"
    )
  )

  tear <- xml2::xml_new_root(
    "path",
    .attrs = attrs,
    .namespace = c(svg = "http://www.w3.org/2000/svg")
  )

  tear
}
