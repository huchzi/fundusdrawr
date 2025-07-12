svg_to_grob <- function(x) {
  charToRaw(x) |>
    rsvg::rsvg() |>
    grid::rasterGrob(interpolate = TRUE)
}
