render_tear_list <- function(tear_list) {
  if (is.null(tear_list)) return("")
  purrr::map_chr(tear_list, render_tear) |> stringr::str_c(collapse = "\n")
}

render_tear <- function(tear) {
  uhr <- as.numeric(tear$lokalisation)
  angle <- 2 * pi * (uhr %% 12) / 12
  cx <- 200 + 95 * sin(angle)
  cy <- 200 - 95 * cos(angle)

  gr <- ifelse(tear$groesse == "gross", 20, 12)
  sr <- ifelse(tear$groesse == "gross", 15, 8)

  rotation <- glue::glue("rotate({angle * 180 / pi}, {cx}, {cy})")
  path_d <- glue::glue("M{cx - sr},{cy} A{gr},{gr} 0 0,0 {cx + sr},{cy} A{sr},{sr} 0 0,1 {cx - sr},{cy} Z")

  glue::glue('<path d="{path_d}" fill="red" stroke="blue" stroke-width="2" transform="{rotation}" />')
}
