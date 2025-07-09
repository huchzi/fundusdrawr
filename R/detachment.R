detachment <- function(detachment_obj) {
  inner_radii <- detachment_obj$inner_radii
  outer_radius = 200
  if (length(inner_radii) != 12) {
    stop("inner_radii must be a vector of 12 numeric values (one for each hour).")
  }

  deg2rad <- function(deg) pi * deg / 180
  angles_deg <- seq(-60, 270, by = 30) # positions in vector should correspond to clock
  angles_rad <- deg2rad(angles_deg)

  outer_points <- data.frame(
    x = outer_radius * cos(angles_rad) + 200,
    y = outer_radius * sin(angles_rad) + 200
  )

  inner_points <- data.frame(
    x = inner_radii * cos(angles_rad) + 200,
    y = inner_radii * sin(angles_rad) + 200
  )

  # Beginne mit äußerem Ring (im Uhrzeigersinn)
  path <- sprintf("M %.2f %.2f", outer_points$x[1], outer_points$y[1])
  for (i in 2:12) {
    path <- paste0(path, sprintf(" L %.2f %.2f", outer_points$x[i], outer_points$y[i]))
  }
  path <- paste0(path, " Z")

  # Dann inneren Ring (gegen den Uhrzeigersinn)
  path <- paste0(path, sprintf(" M %.2f %.2f", inner_points$x[12], inner_points$y[12]))
  for (i in 11:1) {
    path <- paste0(path, sprintf(" L %.2f %.2f", inner_points$x[i], inner_points$y[i]))
  }
  path <- paste0(path, " Z")

  glue::glue('<path d="{path}" fill="blue" fill-opacity="0.5" clip-path="url(#oraClip)"/>')
}

