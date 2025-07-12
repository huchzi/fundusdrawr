library(shiny)
library(jsonlite)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("eye", "Augenseite", choices = c("OD", "OS"), selected = "OD"),
      actionButton("add_tear", "+ Hufeisenriss"),
      actionButton("add_detachment", "+ Netzhautablösung")
    ),
    mainPanel(
      htmlOutput("svg_image"),
      br(),
      textOutput("item_list")
    )
  )
)

create_tear <- modalDialog(
  title = "Create tear",
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      sliderInput("tear_clock", "Lokalisation (Uhrzeiten)",
                  min = 1, max = 12,
                  step = 1, value = 6)
    )
  ),
  footer = tagList(actionButton("save_tear", "Speichern"),
                   modalButton("Abbrechen"))
)

# create_detachment <- modalDialog(
#   title = "Create detachment",
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       lapply(1:12, function (c) {
#       sliderInput(glue::glue("detachment_clock{c}"), glue::glue("{c} Uhr"),
#                   min = 0, max = 180, step = 10, value = 180)})
#     )
#   ),
#   footer = tagList(actionButton("save_detachment", "Speichern"),
#                    modalButton("Abbrechen"))
# )

create_detachment2 <- modalDialog(
  title = "Create detachment",
      plotOutput("detachment", click = "select_point"),
  footer = tagList(actionButton("save_detachment", "Speichern"),
                   modalButton("Abbrechen"))
)

server <- function(input, output, session) {

  new_detachment_obj <- reactiveVal(list(type = "detachment",
                                     inner_radii = rep(180, 12)))
  reset_new_detachment_obj <- function() {
    new_detachment_obj(list(type = "detachment",
                            inner_radii = rep(180, 12)))
  }

  fundus_items <- reactiveVal(list())

  observeEvent(input$save_tear, {
    fundus_items(
      c(
        list(list(type = "tear", clock = input$tear_clock, eccentricity = 95, size = "large")),
        fundus_items()
      )
    )
    reset_new_detachment_obj()
    removeModal()
  })

  observeEvent(input$save_detachment, {
    fundus_items(
      c(
        list(new_detachment_obj()),
        fundus_items()
      )
    )
    removeModal()
  })

  observeEvent(input$add_tear, {
    showModal(create_tear)
  })

  observeEvent(input$add_detachment, {
    showModal(create_detachment2)
  })

  output$svg_image <- renderUI({

    stringr::str_c(ifelse(input$eye == "OS", ora_clip_OS, ora_clip),
                   ifelse(input$eye == "OS", left_eye(fundus_template), fundus_template),
                   render_objects(fundus_items())) |>
      fundus_image() |>
    HTML()
  })

  output$item_list <- renderText({
    req(!identical(fundus_items(), list()))

    toJSON(fundus_items(), pretty = TRUE, flatten = TRUE)
    # try(purrr::map_chr(fundus_items(), function (x) return(x$type)) |>
    #       stringr::str_c(collapse = "\n"))
  })

  output$detachment <- renderPlot({
    background_image <-
      fundus_image(stringr::str_c(left_eye(fundus_template_plain), detachment(new_detachment_obj()))) |>
      svg_to_grob()

    ggplot(raster, aes(x = cx, y = cy)) +
      annotation_custom(background_image) +
      geom_point(alpha = .3) +
      coord_equal() +
      theme_void()
  })

  observeEvent(input$select_point, {
    tab <- nearPoints(raster, input$select_point, xvar = "cx", yvar = "cy")
    req(nrow(tab) > 0)

    new_radii <- new_detachment_obj()
    new_radii$inner_radii[tab$clock[1]] <- tab$ecc[1]
    new_detachment_obj(new_radii)
  })

}

shinyApp(ui, server)
