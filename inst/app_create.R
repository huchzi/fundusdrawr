library(shiny)
library(jsonlite)
library(shinyWidgets)

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
      textOutput("item_list"),
      br(),
      uiOutput("modify_selector")
    )
  )
)

modify_tear <- modalDialog(
  title = "Create tear",
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      sliderInput("tear_clock", "Lokalisation (Uhrzeiten)",
                  min = 1, max = 12,
                  step = 1, value = 6),
      radioButtons("tear_size", "Größe",
                  choiceNames = c("groß", "mittel", "klein"),
                  choiceValues = c("large", "medium", "small"),
                  inline = TRUE),
      sliderInput("tear_ecc", label = "Exzentrizität", min = 0, max = 120, step = 5, value = 95)
    )
  ),
  footer = tagList(actionButton("save_tear", "Speichern"),
                   modalButton("Abbrechen"))
)

modify_detachment <- modalDialog(
  title = "Create detachment",
      selectInput("invert_selection", label = "Invert?", choices = c("yes", "no"), selected = "no"),
      plotOutput("detachment", click = "select_point"),
  footer = tagList(actionButton("save_detachment", "Speichern"),
                   modalButton("Abbrechen"))
)

server <- function(input, output, session) {

  fundus_items <- reactiveVal(list())

  new_fundus_item <- reactiveVal(list(type = "detachment",
                                     inner_radii = rep(180, 12)))

  reset_new_fundus_item <- function() {
    new_fundus_item(list(type = "detachment",
                            inner_radii = rep(180, 12)))
  }

  observeEvent(input$save_tear, {
    fundus_items(
      sort_items(
      c(
        list(list(type = "tear",
                  clock = input$tear_clock,
                  eccentricity = input$tear_ecc,
                  size = input$tear_size)),
        fundus_items()
      ))
    )
    reset_new_fundus_item()
    removeModal()
  })

  observeEvent(input$save_detachment, {
    fundus_items(
      sort_items(c(
        list(new_fundus_item()),
        fundus_items()
      ))
    )
    removeModal()
  })

  observeEvent(input$add_tear, {
    showModal(modify_tear)
  })

  observeEvent(input$add_detachment, {
    showModal(modify_detachment)
  })

  output$modify_selector <- renderUI({
    req(length(fundus_items()) > 0)
    bttns <- list()
    for(i in 1:length(fundus_items())) {
      bttns <- list(bttns, actionButton(glue::glue("modify{i}"),
                                        HTML(fundus_image(
                                          stringr::str_c(ifelse(input$eye == "OS", left_eye(fundus_template), fundus_template),
                                          render_objects(fundus_items()[i])),
                                                          scale_image = .1))))
    }
    bttns
  })

  output$svg_image <- renderUI({

    stringr::str_c(ifelse(input$eye == "OS", ora_clip_OS, ora_clip),
                   ifelse(input$eye == "OS", left_eye(fundus_template), fundus_template),
                   render_objects(fundus_items())) |>
      fundus_image(scale_image = 1.5) |>
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
      fundus_image(stringr::str_c(left_eye(fundus_template_plain), detachment(new_fundus_item()))) |>
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

    new_radii <- new_fundus_item()
    new_radii$inner_radii[tab$clock[1]] <- tab$ecc[1]
    new_fundus_item(new_radii)
  })

}

shinyApp(ui, server)
