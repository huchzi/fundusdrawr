laser <- function(obj, parent) {
  if (is.null(obj$clock)) {
    stop("Laser requires 'clock'.")
  }

  if (is.null(obj$eccentricity) && is.null(obj$ecc)) {
    warning("Laser: eccentricity set to default of 95°.")
    eccentricity <- 95
  } else if (!is.null(obj$eccentricity)) {
    eccentricity <- obj$eccentricity
  } else {
    eccentricity <- obj$ecc
  }

  clock <- as.numeric(obj$clock)
  eccentricity <- as.numeric(eccentricity)

  rotation <- 30 * (clock %% 12)
  counter_rotation <- 30 * ((12 - clock) %% 12)

  transform_str <- glue::glue(
    "rotate({rotation} 200 200) translate(0 -{eccentricity}) rotate({counter_rotation} 200 200)"
  )

  xml2::xml_add_child(parent,
    "text",
    "L",
    x = "200",
    y = "200",
    fill = "darkgreen",
    `font-family` = "sans-serif",
    `font-weight` = "bold",
    `font-size` = "20",
    `text-anchor` = "middle",
    `dominant-baseline` = "middle",
    transform = transform_str
  )
}
