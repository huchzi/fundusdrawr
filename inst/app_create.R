library(shiny)
library(jsonlite)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textOutput("item_list"),
      actionButton("add_tear", "+ Hufeisenriss"),
      actionButton("add_detachment", "+ Netzhautablösung")
    ),
    mainPanel(
      htmlOutput("svg_image")
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
                  step = 1, value = 6),
      actionButton("save_tear", "Add")
    )
  )
)

create_detachment <- modalDialog(
  title = "Create detachment",
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      lapply(1:12, function (c) {
      sliderInput(glue::glue("detachment_clock{c}"), glue::glue("{c} Uhr"),
                  min = 0, max = 180, step = 10, value = 180)}),
      actionButton("save_detachment", "Add")
    )
  )
)

server <- function(input, output, session) {

  fundus_items <- reactiveVal(list())

  observeEvent(input$save_tear, {
    fundus_items(
      c(
        list(list(type = "tear", lokalisation = input$tear_clock, eccentricity = 95, groesse = "gross")),
        fundus_items()
      )
    )
  })

  observeEvent(input$save_detachment, {
    radii <- sapply(1:12, function(x) { input[[glue::glue("detachment_clock{x}")]] })

    print(radii)
    fundus_items(
      c(
        list(list(type = "detachment", inner_radii = radii)),
        fundus_items()
      )
    )
  })

  observeEvent(input$add_tear, {
    showModal(create_tear)
  })

  observeEvent(input$add_detachment, {
    showModal(create_detachment)
  })

  output$svg_image <- renderUI({

    stringr::str_c(ora_clip,
                   fundus_template,
                   render_objects(fundus_items())) |>
      fundus_image() |>
    HTML()
  })

  output$item_list <- renderText({
    req(!identical(fundus_items(), list()))

    toJSON(fundus_items(), pretty = TRUE)
    # try(purrr::map_chr(fundus_items(), function (x) return(x$type)) |>
    #       stringr::str_c(collapse = "\n"))
  })

}

shinyApp(ui, server)
