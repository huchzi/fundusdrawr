library(shiny)
library(here)
library(ggplot2)
i_am("inst/app_input.R")

polar_to_cart <- function(df) {
  angle <- 2 * pi * (df$clock %% 12) / 12
  df$cx <- 200 + df$ecc * sin(angle)
  df$cy <- 200 + df$ecc * cos(angle)
  df
}

cart_to_polar <- function(df) {
  dx = df$cx - 200
  dy = df$cy - 200
  r = sqrt(dx^2 + dy^2)
  theta_deg = (atan2(dy, dx) * 180 / pi) %% 360

  df$clock <- theta_deg / 30
  df$ecc <- r
  df
}

raster <- data.frame(clock = integer(0), ecc = integer(0), cx = integer(0), cy = integer(0))
for (xx in seq(0, 400, 10)) {
  for (yy in seq(0, 400, 10)) {
    raster <- rbind(raster, data.frame(clock = NA, ecc = NA, cx = xx, cy = yy))
  }
}
# raster <- raster[sqrt((raster$cx- 200)^2 + (raster$cy - 200)^2) < 200, ]

# raster <- data.frame(clock = rep(1:12, each=18), ecc = rep(1:18 * 10, 12))
# raster <- polar_to_cart(raster)

ui <- fluidPage(
  plotOutput("drawing", click = "select_point"),
  tableOutput("data"),
  textOutput("svg_object")
)

server <- function(input, output, session) {

  form_obj <- reactiveVal(list(type = "detachment",
                                     path = list()))

  observeEvent(input$select_point, {
    tab <- nearPoints(raster, input$select_point, xvar = "cx", yvar = "cy")
    new_obj <- form_obj()
    new_obj$path <- rbind(new_obj$path, cart_to_polar(tab))
    form_obj(new_obj)
    # req(nrow(tab) > 0)
    #
    # new_radii <- detachment_obj()
    # new_radii$inner_radii[tab$clock[1]] <- tab$ecc[1]
    # detachment_obj(new_radii)
  })

  svg_to_grob <- function(x) {
    charToRaw(x) |>
      rsvg::rsvg() |>
      grid::rasterGrob(interpolate = TRUE)
    }

  output$drawing <- renderPlot({
    background_image <-
      fundus_image(stringr::str_c(ora_clip_OS,
                                  left_eye(fundus_template),
                                  closed_form(form_obj()$path)
                                  )) |>
      svg_to_grob()

    ggplot(raster, aes(x = cx, y = cy)) +
      annotation_custom(background_image) +
      geom_point(alpha = .3, size = .2) +
      coord_equal() +
      scale_y_reverse() +
      theme_void()
  })

  output$data <- renderTable({
    form_obj()$path
  })

  output$svg_object <- renderText({
    closed_form(form_obj()$path)
  })
}

shinyApp(ui, server)
