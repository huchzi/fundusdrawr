library(shiny)
devtools::load_all()

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("eye", "Augenseite", choices = c("OD", "OS"), selected = "OD"),
      actionButton("add_tear", "+ Hufeisenriss"),
      actionButton("add_detachment", "+ Netzhautablösung"),
      hr(),
      downloadButton("download_word", "Skizze herunterladen")
    ),
    mainPanel(
      htmlOutput("svg_image"),
      br(),
      uiOutput("modify_selector")
    )
  )
)

modify_tear <- modalDialog(
  title = "Create tear",
      sliderInput("tear_clock", "Lokalisation (Uhrzeiten)",
                  min = 1, max = 12,
                  step = .5, value = 6),
      radioButtons("tear_size", "Größe",
                  choiceNames = c("groß", "mittel", "klein"),
                  choiceValues = c("large", "medium", "small"),
                  selected = "medium",
                  inline = TRUE),
      sliderInput("tear_ecc", label = "Exzentrizität", min = 0, max = 120, step = 5, value = 100),
  footer = tagList(actionButton("save_tear", "Speichern"),
                   actionButton("delete_tear", "Löschen"),
                   modalButton("Abbrechen"))
)

modify_detachment <- modalDialog(
  title = "Create detachment",
  plotOutput("detachment", click = "select_point"),
  actionButton("delete_point", "Undo"),
  footer = tagList(actionButton("save_detachment", "Speichern"),
                   actionButton("delete_detachment", "Löschen"),
                   modalButton("Abbrechen"))
)

server <- function(input, output, session) {

  raster <- reactiveVal(raster)

  fundus_items <- reactiveVal(list())

  new_fundus_item <- reactiveVal(list())

  remaining_fundus_items <- reactiveVal(list())

  new_detachment <- function() {
    new_fundus_item(list(type = "detachment",
                            inner_radii = rep(180, 12)))
  }

  new_tear <- function() {
    new_fundus_item(list(type = "tear",
                         clock = input$tear_clock,
                         eccentricity = input$tear_ecc,
                         size = input$tear_size))
  }

  observeEvent(input$save_tear, {
    fundus_items(
      sort_items(
      c(
        list(list(type = "tear",
                  clock = input$tear_clock,
                  eccentricity = input$tear_ecc,
                  size = input$tear_size)),
        remaining_fundus_items()
      ))
    )
    removeModal()
  })

  observeEvent(input$save_detachment, {
    fundus_items(
      sort_items(c(
        list(new_fundus_item()),
        remaining_fundus_items()
      ))
    )
    removeModal()
  })

  observeEvent(input$delete_detachment, {
    fundus_items(remaining_fundus_items())
    removeModal()
  })

  observeEvent(input$delete_tear, {
    fundus_items(remaining_fundus_items())
    removeModal()
  })

  observeEvent(input$add_tear, {
    remaining_fundus_items(fundus_items())
    new_tear()
    showModal(modify_tear)
  })

  observeEvent(input$add_detachment, {
    remaining_fundus_items(fundus_items())
    new_detachment()
    showModal(modify_detachment)
  })

  observeEvent(input$delete_point, {
    modify_item <- new_fundus_item()
    req(nrow(modify_item$path) > 0)
    modify_item$path <- modify_item$path[-nrow(modify_item$path), ]
    new_fundus_item(modify_item)
  })

  output$modify_selector <- renderUI({
    req(length(fundus_items()) > 0)
    bttns <- list()
    for(i in 1:length(fundus_items())) {
      bttns <- list(bttns, actionButton(glue::glue("modify{i}"),
                                        HTML(fundus_image(
                                          stringr::str_c(ifelse(input$eye == "OS",
                                                                left_eye(fundus_template),
                                                                fundus_template),
                                                         render_objects(fundus_items()[i])),
                                          scale_image = .2))))
    }
    bttns
  })

  observe({
    lapply(paste0("modify", 1:length(fundus_items())),
           function(id) {
             observeEvent(input[[id]], {
               i <- as.integer(sub("modify", "", id))
               new_fundus_item <- fundus_items()[i]
               remaining_fundus_items(fundus_items()[-i])
               if(fundus_items()[[i]][["type"]] == "detachment") {
                 showModal(modify_detachment)
               }
               if(fundus_items()[[i]][["type"]] == "tear") {
                 showModal(modify_tear)
               }
             }, ignoreInit = TRUE)
           })
  })

  output$svg_image <- renderUI({

    stringr::str_c(ifelse(input$eye == "OS", ora_clip_OS, ora_clip),
                   ifelse(input$eye == "OS", left_eye(fundus_template), fundus_template),
                   render_objects(fundus_items())) |>
      fundus_image(scale_image = 1.3) |>
    HTML()

   })

  # output$item_list <- renderText({
  #   req(!identical(fundus_items(), list()))
  #
  #   toJSON(fundus_items(), pretty = TRUE, flatten = TRUE)
  # })

  output$detachment <- renderPlot({
    background_image <-
      fundus_image(stringr::str_c(left_eye(fundus_template), closed_form(new_fundus_item()$path))) |>
      svg_to_grob()

    ggplot2::ggplot(raster(), ggplot2::aes(x = cx, y = cy)) +
      ggplot2::annotation_custom(background_image,
                                 xmin = 0, xmax = 400) +
      ggplot2::geom_point(alpha = .3, size = .5) +
      ggplot2::coord_equal() +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_void()
  })

  observeEvent(input$select_point, {
    tab <- nearPoints(raster(), input$select_point, xvar = "cx", yvar = "cy")
    new_obj <- new_fundus_item()
    new_obj$path <- rbind(new_obj$path, tab)
    new_fundus_item(new_obj)

    new_raster <- raster()
    new_raster[new_raster$N %in% tab$N, "selected"] <- TRUE
    raster(new_raster)
  })

  output$download_word <- downloadHandler(
    filename = function() {
      paste("fundus_image.png")
    },
    content = function(file) {
      stringr::str_c(ifelse(input$eye == "OS", ora_clip_OS, ora_clip),
                     ifelse(input$eye == "OS", left_eye(fundus_template), fundus_template),
                     render_objects(fundus_items())) |>
        fundus_image(scale_image = 1) |>
        charToRaw() |>
        rsvg::rsvg_png(file = file, width = 400, height = 400)
        # writeLines(con = file)
    }
  )

}

shinyApp(ui, server)
