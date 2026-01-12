form_obj <- data.frame(
  n = 1:4,
  clock = c(10.5, 1.5, 4.5, 7.5),
  ecc = rep(50, 4)
)

clock_to_rad <- function(clock) {
  (clock %% 12) * 30 * pi / 180
}

toXY <- function(item) {
  center_x <- 200
  center_y <- 200
  x <- item$ecc * cos(item$clock |> clock_to_rad()) + 200
  y <- item$ecc * sin(item$clock |> clock_to_rad()) + 200
  data.frame(x, y)
}

closed_form <- function(item, radius = 10) {
  if (!is.data.frame(item)) {
    return("")
  }

  if (is.data.frame(item) && nrow(item) == 1) {
    x1 <- item$cx[1]
    y1 <- item$cy[1]
    return(glue::glue('<circle cx="{x1}" cy="{y1}" r="3" fill="black"/>'))
  }

  if (is.data.frame(item) && nrow(item) == 2) {
    x1 <- item$cx[1]
    y1 <- item$cy[1]
    x2 <- item$cx[2]
    y2 <- item$cy[2]
    return(glue::glue('<path d="M {x1},{y1} L {x2},{y2} Z" stroke="blue" stroke-width="2" fill="blue" fill-opacity="0.5"/>'))
  }

  # Hilfsfunktionen
  get_midpoint <- function(p1, p2) {
    c((p1[1] + p2[1]) / 2, (p1[2] + p2[2]) / 2)
  }

  # Punkte holen
  points <- item |>
    toXY() |>
    as.matrix()
  n <- nrow(points)
  if (n < 3) stop("Mindestens 3 Punkte für geschlossene Kurve nötig")

  # Punkte schließen
  points <- rbind(points, points[1, ]) # schließe Pfad

  path_commands <- c()

  for (i in 1:n) {
    p0 <- points[i, ]
    p1 <- points[i + 1, ]
    p_prev <- if (i == 1) points[n, ] else points[i - 1, ]

    # Einrückpunkte berechnen (einfache Methode)
    p0_in <- get_midpoint(p_prev, p0)
    p0_out <- get_midpoint(p0, p1)

    if (i == 1) {
      path_commands <- c(path_commands, glue::glue("M {p0_in[1]},{p0_in[2]}"))
    }

    # Bézierkurve von p0_in nach p0_out mit Kontrollpunkt p0
    path_commands <- c(
      path_commands,
      glue::glue("Q {p0[1]},{p0[2]} {p0_out[1]},{p0_out[2]}")
    )
  }

  path_string <- paste(path_commands, collapse = " ") |> paste("Z")

  glue::glue('<path d="{path_string}" stroke="blue" stroke-width="2" fill="blue" fill-opacity="0.5" stroke-linejoin="round" clip-path="url(#oraClip)"/>')
}
