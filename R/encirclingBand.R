encirclingBand <- function(obj, parent) {
  xml_add_child(parent,
    "circle",
    cx = 200,
    cy = 200,
    r = 98,
    fill = "none",
    style = "stroke:brown;stroke-width:4px;"
  )
}
