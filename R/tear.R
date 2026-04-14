tear_coords <- function(obj) {
  if (is.null(obj$eccentricity)) {
    warning("Eccentricity not provided. Setting to default of 95°.")
    eccentricity <- 95
  } else {
    eccentricity <- obj$eccentricity
  }

  uhr <- as.numeric(obj$clock)
  angle <- 2 * pi * (uhr %% 12) / 12
  cx <- 200 + eccentricity * sin(angle)
  cy <- 200 - eccentricity * cos(angle)

  list(clock = uhr, eccentricity = eccentricity, angle = angle, cx = cx, cy = cy)
}

tear <- function(obj, parent) {
  coords <- tear_coords(obj)

  cx <- coords$cx
  cy <- coords$cy
  angle <- coords$angle

  gr <- ifelse(obj$size == "large", 20, 12)
  sr <- ifelse(obj$size == "large", 15, 8)

  rotation <- glue::glue("rotate({angle * 180 / pi}, {cx}, {cy})")
  path_d <- glue::glue("M{cx - sr},{cy} A{gr},{gr} 0 0,0 {cx + sr},{cy} A{sr},{sr} 0 0,1 {cx - sr},{cy} Z")


  xml2::xml_add_child(parent,
    "path",
    d = path_d,
    fill = "red",
    stroke = "blue",
    `stroke-width` = "2",
    transform = rotation
  )
}
