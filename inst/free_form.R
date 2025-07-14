library(fundusdrawr)

form_obj <- data.frame(n = 1:4,
                       clock = c(10.5, 1.5, 4.5, 7.5),
                       ecc = rep(50, 4))

clock_to_rad <- function(clock) {
  (clock %% 12) * 30 * pi / 180
}

toXY <- function(item) {
  center_x <- 200
  center_y <- 200
  x = item$ecc * cos(item$clock |> clock_to_rad()) + 200
  y = item$ecc * sin(item$clock |> clock_to_rad()) + 200
  data.frame(x, y)
}

closed_form <- function(item) {

  if (is.data.frame(item) && nrow(item) == 1) {
    x1 <- item$cx[1]
    y1 <- item$cy[1]
    return(glue::glue('<circle cx="{x1}" cy="{y1}" r="3" fill="black"/>'))
  }

  coord_list <- purrr::pmap(item |> toXY() |> tibble::tibble(),
                 function(x, y) glue::glue('{x},{y}'))

  path_string <- stringr::str_c("M ",
                                paste(coord_list, collapse = " L "), " Z")

  glue::glue('<path d="{path_string}" stroke="blue" stroke-width="2" stroke-opacity="0.5" fill="blue" fill-opacity="0.5" clip-path="url(#oraClip)"/>')
}

# stringr::str_c(ora_clip,
#                fundus_template,
#                closed_form(form_obj)) |>
#   fundus_image() |> writeLines(con = "inst/test.svg")
