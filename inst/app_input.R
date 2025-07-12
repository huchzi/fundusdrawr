library(shiny)
library(here)
i_am("inst/app_input.R")

polar_to_cart <- function(df) {
  angle <- 2 * pi * (df$clock %% 12) / 12
  df$cx <- 200 + df$ecc * sin(angle)
  df$cy <- 200 + df$ecc * cos(angle)
  df
}

raster <- data.frame(clock = rep(1:12, each=18), ecc = rep(1:18 * 10, 12))
raster <- polar_to_cart(raster)

ui <- fluidPage(
  plotOutput("drawing", click = "select_point"),
  tableOutput("data")
)

server <- function(input, output, session) {

  detachment_obj <- reactiveVal(list(type = "detachment",
                                     inner_radii = rep(180, 12)))

  observeEvent(input$select_point, {
    tab <- nearPoints(raster, input$select_point, xvar = "cx", yvar = "cy")
    req(nrow(tab) > 0)

    new_radii <- detachment_obj()
    new_radii$inner_radii[tab$clock[1]] <- tab$ecc[1]
    detachment_obj(new_radii)
  })

  svg_to_grob <- function(x) {
    charToRaw(x) |>
      rsvg::rsvg() |>
      grid::rasterGrob(interpolate = TRUE)
    }

  output$drawing <- renderPlot({
    background_image <-
      fundus_image(stringr::str_c(left_eye(fundus_template_plain), detachment(detachment_obj()))) |>
      svg_to_grob()

    ggplot(raster, aes(x = cx, y = cy)) +
      annotation_custom(background_image) +
      geom_point(alpha = .3) +
      coord_equal() +
      theme_void()
  })

  output$data <- renderTable({
    nearPoints(raster, input$select_point, xvar = "cx", yvar = "cy")
  })
}

shinyApp(ui, server)
