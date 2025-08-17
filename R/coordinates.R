clock_to_xy <- function(clock, eccentricity) {
  angle <- 2 * pi * (clock %% 12) / 12
  cx <- 200 + eccentricity * sin(angle)
  cy <- 200 - eccentricity * cos(angle)
  list(x = cx, y = cy)
}
