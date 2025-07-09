library(shiny)
library(jsonlite)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textOutput("item_list"),
      actionButton("add_tear", "+ Tear")
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

  observeEvent(input$add_tear, {
    showModal(create_tear)
  })

  output$svg_image <- renderUI({
    req(!identical(fundus_items(), list()))

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
