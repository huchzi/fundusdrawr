clock_to_xy <- function(clock, eccentricity) {
  angle <- 2 * pi * (clock %% 12) / 12
  cx <- 200 + eccentricity * sin(angle)
  cy <- 200 - eccentricity * cos(angle)
  list(x = cx, y = cy)
}

xy_to_clock_eccentricity <- function(cx, cy) {
  dx <- cx - 200
  dy <- 200 - cy
  eccentricity <- sqrt(dx^2 + dy^2)
  clock <- (atan2(dx, dy) %% (2 * pi)) * 12 / (2 * pi)
  clock <- ifelse(clock == 0, 12, clock)

  list(clock = clock, eccentricity = eccentricity)
}
