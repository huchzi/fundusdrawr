#' @export
tear <- function(tear_obj) {

  if (is.null(tear_obj$eccentricity)) {
    eccentricity <- 95
  } else {
    eccentricity <- tear_obj$eccentricity
  }

  uhr <- as.numeric(tear_obj$clock)
  angle <- 2 * pi * (uhr %% 12) / 12
  cx <- 200 + eccentricity * sin(angle)
  cy <- 200 - eccentricity * cos(angle)

  gr <- ifelse(tear_obj$size == "large", 20, 12)
  sr <- ifelse(tear_obj$size == "large", 15, 8)

  rotation <- glue::glue("rotate({angle * 180 / pi}, {cx}, {cy})")
  path_d <- glue::glue("M{cx - sr},{cy} A{gr},{gr} 0 0,0 {cx + sr},{cy} A{sr},{sr} 0 0,1 {cx - sr},{cy} Z")

  glue::glue('<path d="{path_d}" fill="red" stroke="blue" stroke-width="2" transform="{rotation}" />')
}
