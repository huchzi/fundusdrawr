lattice <- function(obj, parent) {
  if (is.null(obj$from) || is.null(obj$to)) {
    stop("Lattice requires 'from' and 'to'.")
  }

  from <- as.numeric(obj$from)
  to <- as.numeric(obj$to)

  if (is.null(obj$eccentricity)) {
    warning("Lattice: eccentricity set to default of 95°.")
    eccentricity <- 95
  } else {
    eccentricity <- as.numeric(obj$eccentricity)
  }

  span <- (to - from) %% 12
  if (isTRUE(all.equal(span, 0))) {
    span <- 12
  }

  arc_radii <- c(eccentricity + 8, eccentricity + 8, eccentricity - 8, eccentricity - 8)
  arc_coords <- clock_to_xy(c(from, to, from, to), arc_radii)

  outer_radius <- eccentricity + 10
  inner_radius <- eccentricity - 8

  new_group <- xml2::xml_add_child(parent, "g")

  arc_path <- glue::glue(
    "M {arc_coords$x[1]},{arc_coords$y[1]} ",
    "A {outer_radius},{outer_radius} 0 0,1 {arc_coords$x[2]},{arc_coords$y[2]} ",
    "A 8,8 0 0,1 {arc_coords$x[4]},{arc_coords$y[4]} ",
    "A {inner_radius},{inner_radius} 0 0,0 {arc_coords$x[3]},{arc_coords$y[3]} ",
    "A 8,8 0 0,1 {arc_coords$x[1]},{arc_coords$y[1]}"
  )

  xml2::xml_add_child(new_group, "path",
    d = arc_path,
    fill = "none",
    stroke = "black",
    `stroke-width` = "2",
    opacity = "0.8"
  )

  quarter_hours <- seq(0, 11.75, by = 0.25)
  relative_positions <- (quarter_hours - from) %% 12
  keep <- relative_positions >= 0.25 & relative_positions <= (span - 0.25)
  lattice_clocks <- quarter_hours[keep]

  for (clock in lattice_clocks) {
    xml2::xml_add_child(new_group,
      "text",
      "X",
      x = "200",
      y = "207",
      fill = "black",
      `font-family` = "sans-serif",
      `font-size` = "20",
      `text-anchor` = "middle",
      opacity = "0.8",
      transform = glue::glue("rotate({clock * 30} 200 200) translate(0 -{eccentricity})")
    )
  }

  invisible(parent)
}
