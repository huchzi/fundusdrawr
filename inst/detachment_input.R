library(ggplot2)
library(ggimage)

polar_to_cart <- function(df) {
  angle <- 2 * pi * (df$clock %% 12) / 12
  df$cx <- 200 + df$ecc * sin(angle)
  df$cy <- 200 - df$ecc * cos(angle)
  df
}

raster <- data.frame(clock = rep(1:12, each=18), ecc = rep(1:18, 12))
raster <- polar_to_cart(raster)

fundus_template_plain <- readLines("inst/fundus_skizze_cleaned_plain.svg") |>
  paste(collapse = "\n")

background_image <-
  fundus_image(left_eye(fundus_template_plain)) |>
  charToRaw() |>
  rsvg::rsvg()

grob <- grid::rasterGrob(background_image, interpolate = TRUE)

ggplot(raster, aes(x = cx, y = cy)) +
  annotation_custom(grob) +
  geom_point(alpha = .3) +
  coord_equal() +
  theme_void()

