laser_path <- function(obj) {
  if (!is.null(obj$path)) {
    path <- obj$path

    if (length(path) == 0) {
      return(data.frame(
        cx = numeric(0),
        cy = numeric(0),
        clock = numeric(0),
        eccentricity = numeric(0)
      ))
    }

    if (!is.data.frame(path)) {
      path <- do.call(rbind, lapply(path, as.data.frame))
    }

    if (!("eccentricity" %in% names(path)) && "ecc" %in% names(path)) {
      path$eccentricity <- path$ecc
    }

    if (!all(c("cx", "cy") %in% names(path)) &&
        all(c("clock", "eccentricity") %in% names(path))) {
      coords <- clock_to_xy(as.numeric(path$clock), as.numeric(path$eccentricity))
      path$cx <- coords$x
      path$cy <- coords$y
    }

    if (!all(c("clock", "eccentricity") %in% names(path)) &&
        all(c("cx", "cy") %in% names(path))) {
      coords <- xy_to_clock_eccentricity(as.numeric(path$cx), as.numeric(path$cy))
      path$clock <- coords$clock
      path$eccentricity <- coords$eccentricity
    }

    return(path[, c("cx", "cy", "clock", "eccentricity")])
  }

  if (is.null(obj$clock)) {
    stop("Laser requires 'clock' or 'path'.")
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
  coords <- clock_to_xy(clock, eccentricity)

  data.frame(
    cx = coords$x,
    cy = coords$y,
    clock = clock,
    eccentricity = eccentricity
  )
}

laser <- function(obj, parent) {
  path <- laser_path(obj)

  if (nrow(path) == 0) {
    return(invisible(parent))
  }

  for (i in seq_len(nrow(path))) {
    xml2::xml_add_child(parent,
      "text",
      "L",
      x = as.character(path$cx[i]),
      y = as.character(path$cy[i]),
      fill = "darkgreen",
      `font-family` = "sans-serif",
      `font-weight` = "bold",
      `font-size` = "14",
      `text-anchor` = "middle",
      `dominant-baseline` = "middle"
    )
  }

  invisible(parent)
}
