encirclingBand <- function(obj) {
  eb <- read_xml("<circle/>")
  xml_attrs(eb) <- c(
    cx = 200,
    cy = 200,
    r = 98,
    fill = "none",
    style = "stroke:brown;stroke-width:4px;"
  )
  eb
}
