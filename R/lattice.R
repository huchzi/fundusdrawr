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

  midpoint <- (from + span / 2) %% 12
  if (isTRUE(all.equal(midpoint, 0))) {
    midpoint <- 12
  }

  start_clock <- 12 - span / 2
  stop_clock <- 12 + span / 2

  outer_radius <- eccentricity + 10
  inner_radius <- eccentricity - 8
  cap_radius <- (outer_radius - inner_radius) / 2

  outer_points <- clock_to_xy(c(start_clock, stop_clock), rep(outer_radius, 2))
  inner_points <- clock_to_xy(c(start_clock, stop_clock), rep(inner_radius, 2))

  large_arc_flag <- ifelse(span > 6, 1, 0)
  rotation <- midpoint * 30

  new_group <- xml2::xml_add_child(
    parent,
    "g",
    transform = glue::glue("rotate({rotation} 200 200)")
  )

  path_specs <- list(
    glue::glue(
      "M {outer_points$x[1]},{outer_points$y[1]} ",
      "A {outer_radius},{outer_radius} 0 {large_arc_flag},1 {outer_points$x[2]},{outer_points$y[2]}"
    ),
    glue::glue(
      "M {inner_points$x[1]},{inner_points$y[1]} ",
      "A {inner_radius},{inner_radius} 0 {large_arc_flag},1 {inner_points$x[2]},{inner_points$y[2]}"
    ),
    glue::glue(
      "M {outer_points$x[1]},{outer_points$y[1]} ",
      "A {cap_radius},{cap_radius} 0 0,0 {inner_points$x[1]},{inner_points$y[1]}"
    ),
    glue::glue(
      "M {outer_points$x[2]},{outer_points$y[2]} ",
      "A {cap_radius},{cap_radius} 0 0,1 {inner_points$x[2]},{inner_points$y[2]}"
    )
  )

  for (path_d in path_specs) {
    xml2::xml_add_child(new_group, "path",
      d = path_d,
      fill = "none",
      stroke = "black",
      `stroke-width` = "2",
      opacity = "0.8"
    )
  }

  local_clocks <- seq(0, 12, by = 0.25)
  start_limit <- 11.75 - span / 2
  stop_limit <- span / 2 + 0.25
  lattice_clocks <- local_clocks[local_clocks < stop_limit | local_clocks > start_limit]

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
