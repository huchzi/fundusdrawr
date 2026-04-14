library(shiny)
library(jsonlite)
library(xml2)
devtools::load_all()

element_types <- c(
  "encirclingBand",
  "detachment",
  "lattice",
  "tear",
  "laser",
  "roundhole"
)

sort_items <- function(item_list) {
  ord <- sapply(item_list, function(x) x$type) |>
    factor(levels = element_types, ordered = TRUE) |>
    order()
  item_list[ord]
}

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("eye", "Augenseite", choices = c("OD", "OS"), selected = "OD"),
      hr(),
      textAreaInput("natural_language", "Fundusbeschreibung in natürlicher Sprache"),
      actionButton("from_natural_language", "Erstelle aus Sprache"),
      actionButton("from_json", "Lade aus JSON"),
      hr(),
      actionButton("add_encirclingBand", "+ Cerclage"),
      actionButton("add_tear", "+ Hufeisenriss"),
      actionButton("add_laser", "+ Laser"),
      actionButton("add_roundhole", "+ Rundforamen"),
      actionButton("add_detachment", "+ Netzhautablösung"),
      actionButton("add_lattice", "+ Gitterbeet"),
      hr(),
      downloadButton("download_word", "Skizze herunterladen"),
      actionButton("show_json", "JSON-Code zeigen")
    ),
    mainPanel(
      htmlOutput("svg_image"),
      br(),
      uiOutput("modify_selector")
    )
  )
)


# ModalDialogs ------------------------------------------------------------
modify_tear <- function(tear_item) {
  modalDialog(
    title = "Create tear",
    plotOutput("tear", click = "select_tear"),
    radioButtons("tear_size", "Größe",
      choiceNames = c("groß", "mittel", "klein"),
      choiceValues = c("large", "medium", "small"),
      selected = tear_item$size,
      inline = TRUE
    ),
    footer = tagList(
      actionButton("save_tear", "Speichern"),
      actionButton("delete_tear", "Löschen"),
      modalButton("Abbrechen")
    )
  )
}

modify_laser <- function(laser_item) {
  modalDialog(
    title = "Create laser lesions",
    plotOutput("laser", click = "select_laser"),
    footer = tagList(
      actionButton("save_laser", "Speichern"),
      actionButton("delete_laser", "Löschen"),
      modalButton("Abbrechen")
    )
  )
}

modify_roundhole <- function(roundhole_item) {
  modalDialog(
    title = "Create round hole",
    sliderInput("roundhole_clock", "Lokalisation (Uhrzeiten)",
      min = 1, max = 12,
      step = .5, value = roundhole_item$clock
    ),
    sliderInput("roundhole_ecc", label = "Exzentrizität", min = 0, max = 120, step = 5, value = roundhole_item$eccentricity),
    footer = tagList(
      actionButton("save_roundhole", "Speichern"),
      actionButton("delete_roundhole", "Löschen"),
      modalButton("Abbrechen")
    )
  )
}

modify_lattice <- function(lattice_item) {
  modalDialog(
    title = "Create lattice degeneration",
    sliderInput("from", "Von (Uhrzeiten)",
      min = 1, max = 12,
      step = .5, value = lattice_item$from
    ),
    sliderInput("to", "Bis (Uhrzeiten)",
      min = 1, max = 12,
      step = .5, value = lattice_item$to
    ),
    sliderInput("lattice_ecc", label = "Exzentrizität", min = 0, max = 120, step = 5, value = 100),
    footer = tagList(
      actionButton("save_lattice", "Speichern"),
      actionButton("delete_lattice", "Löschen"),
      modalButton("Abbrechen")
    )
  )
}

modify_detachment <- function(detachment_item) {
  modalDialog(
    title = "Create detachment",
    plotOutput("detachment", click = "select_point"),
    actionButton("delete_point", "Undo"),
    footer = tagList(
      actionButton("save_detachment", "Speichern"),
      actionButton("delete_detachment", "Löschen"),
      modalButton("Abbrechen")
    )
  )
}

modify_encirclingBand <- function(encirclingBand_item) {
  modalDialog(
    title = "Cerclage",
    footer = tagList(
      actionButton("save_detachment", "Vorhanden"),
      actionButton("delete_detachment", "Nicht vorhanden")
    )
  )
}

# Server ------------------------------------------------------------------


server <- function(input, output, session) {


  # Reactive vals -----------------------------------------------------------
  fundus_items <- reactiveVal(list())
  new_fundus_item <- reactiveVal(list())
  remaining_fundus_items <- reactiveVal(list())

  # Default element items -------------------------------------------------------
  default_tear <- list(
    type = "tear",
    clock = NULL,
    eccentricity = NULL,
    size = "medium"
  )

  default_laser <- list(
    type = "laser",
    path = data.frame(
      cx = numeric(0),
      cy = numeric(0),
      clock = numeric(0),
      eccentricity = numeric(0)
    )
  )

  default_roundhole <- list(
    type = "roundhole",
    clock = 6,
    eccentricity = 100
  )

  default_detachment <- list(
    type = "detachment",
    path = data.frame(cx = integer(0), cy = integer(0))
  )

  default_lattice <- list(
    type = "lattice",
    from = 11,
    to = 1,
    eccentricity = 95
  )

  default_encirclingBand <- list(type = "encirclingBand")

  # Create element -------------------------------------------------------------
  lapply(element_types, function(elem_type) {
    observeEvent(input[[paste0("add_", elem_type)]], {
      remaining_fundus_items(fundus_items())
      this_new_item <- get(paste0("default_", elem_type))
      new_fundus_item(this_new_item)
      modal <- get(paste0("modify_", elem_type), mode = "function")
      showModal(modal(this_new_item))
    })
  })

  # Add element to list ------------------------------------------------------------
  observeEvent(input$save_tear, {
    tear_item <- new_fundus_item()
    tear_item$size <- input$tear_size
    req(!is.null(tear_item$clock), !is.null(tear_item$eccentricity))

    fundus_items(
      sort_items(c(
        list(tear_item),
        remaining_fundus_items()
      ))
    )
    removeModal()
  })

  observeEvent(input$save_laser, {
    laser_item <- new_fundus_item()
    laser_item$path <- laser_path(laser_item)
    laser_item$clock <- NULL
    laser_item$ecc <- NULL
    laser_item$eccentricity <- NULL

    fundus_items(
      sort_items(c(
        list(laser_item),
        remaining_fundus_items()
      ))
    )
    removeModal()
  })

  observeEvent(input$save_roundhole, {
    fundus_items(
      sort_items(
        c(
          list(list(
            type = "roundhole",
            clock = input$roundhole_clock,
            eccentricity = input$roundhole_ecc
          )),
          remaining_fundus_items()
        )
      )
    )
    removeModal()
  })

  observeEvent(input$save_lattice, {
    fundus_items(
      sort_items(
        c(
          list(list(
            type = "lattice",
            from = input$from,
            to = input$to,
            eccentricity = input$lattice_ecc
          )),
          remaining_fundus_items()
        )
      )
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

  observeEvent(input$save_encirclingBand, {
    c(list(new_fundus_item()), remaining_fundus_items()) |>
      sort_items() |>
      fundus_items()
    removeModal()
  })


  # Delete element ----------------------------------------------------------
  lapply(element_types, function(btn) {
    observeEvent(input[[paste0("delete_", btn)]], {
      fundus_items(remaining_fundus_items())
      removeModal()
    })
  })

  # LLM ---------------------------------------------------------------------
  # Parse natural language to JSON
  observeEvent(input$from_natural_language, {
    token <- Sys.getenv("API_TOKEN") # oder GITHUB_PAT
    endpoint <- "https://models.github.ai/inference/chat/completions"
    # model <- "meta/Llama-4-Scout-17B-16E-Instruct"
    model <- "openai/gpt-4.1-mini"

    body <- list(
      messages = list(
        list(role = "system", content = role),
        list(role = "user", content = paste(prompt, input$natural_language))
      ),
      temperature = 1.0,
      top_p = 1.0,
      max_tokens = 1000,
      model = model
    )

    try(
      resp <- httr2::request(endpoint) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
          "Authorization" = paste("Bearer", token),
          "Accept" = "application/json",
          "Content-Type" = "application/json",
          "X-GitHub-Api-Version" = "2022-11-28"
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    )

    if (httr2::resp_status(resp) >= 400) {
      if (httr2::resp_status(resp) == 429) {
        showNotification(
          "Zu viele Anfragen an Server. Dieser Prototyp verwendet ein kostenloses LLM."
        )
      } else {
        showNotification(
          paste("Fehler bei Verbindung zum LLM. API Error: ", httr2::resp_status_desc(resp)),
          type = "error"
        )
      }
    } else {
      data <- httr2::resp_body_json(resp)
      data$choices[[1]]$message$content |>
        stringr::str_remove_all("^```json\\s*|\\s*```$") |>
        fromJSON(simplifyDataFrame = FALSE) |>
        fundus_items()
    }
  })


  # Thumbnails --------------------------------------------------------------
  output$modify_selector <- renderUI({
    req(length(fundus_items()) > 0)
    bttns <- list()
    for (i in 1:length(fundus_items())) {
      bttns <- list(bttns, actionButton(
        glue::glue("modify{i}"),
        fundus_image(input$eye, fundus_items()[i], clip = TRUE, scale_image = .2) |> HTML()
      ))
    }
    bttns
  })

  observe({
    lapply(
      paste0("modify", 1:length(fundus_items())),
      function(id) {
        observeEvent(input[[id]],
          {
            i <- as.integer(sub("modify", "", id))
            new_fundus_item(fundus_items()[[i]])
            remaining_fundus_items(fundus_items()[-i])
            modal <- get(paste0("modify_", new_fundus_item()$type),
              mode = "function"
            )
            showModal(modal(new_fundus_item()))
          },
          ignoreInit = TRUE
        )
      }
    )
  })


  # DetachmentPlot   -------------------------------------------------------
  output$detachment <- renderPlot({
    background_image <-
      fundus_image(input$eye, append(fundus_items(), list(new_fundus_item())), clip = FALSE, scale_image = 1)
    background_image <- background_image |>
      svg_to_grob()

    ggplot2::ggplot(new_fundus_item()$path, ggplot2::aes(x = cx, y = cy)) +
      ggplot2::annotation_custom(background_image,
        xmin = 0, xmax = 400
      ) +
      ggplot2::geom_point(size = 2, color = "blue") +
      ggplot2::coord_equal() +
      ggplot2::theme_void() +
      ggplot2::scale_x_continuous(limits = c(0, 400)) +
      ggplot2::scale_y_continuous(limits = c(0, 400), trans = "reverse")
  })

  observeEvent(input$select_point, {
    # tab <- nearPoints(raster, input$select_point, xvar = "cx", yvar = "cy")
    tab <- data.frame(cx = input$select_point$x, cy =  input$select_point$y)
    new_obj <- new_fundus_item()
    new_obj$path <- rbind(new_obj$path, tab)
    new_fundus_item(new_obj)
  })

  # removes last selection
  observeEvent(input$delete_point, {
    modify_item <- new_fundus_item()
    req(nrow(modify_item$path) > 0)
    modify_item$path <- modify_item$path[-nrow(modify_item$path), ]
    new_fundus_item(modify_item)
  })

  # TearPlot ----------------------------------------------------------------
  output$tear <- renderPlot({
    tear_item <- new_fundus_item()
    tear_item$size <- input$tear_size

    objects <- remaining_fundus_items()
    if (!is.null(tear_item$clock) && !is.null(tear_item$eccentricity)) {
      objects <- append(objects, list(tear_item))
    }

    background_image <-
      fundus_image(input$eye, objects, clip = FALSE, scale_image = 1)
    background_image <- background_image |>
      svg_to_grob()

    ggplot2::ggplot() +
      ggplot2::annotation_custom(background_image,
        xmin = 0, xmax = 400
      ) +
      ggplot2::coord_equal() +
      ggplot2::theme_void() +
      ggplot2::scale_x_continuous(limits = c(0, 400)) +
      ggplot2::scale_y_continuous(limits = c(0, 400), trans = "reverse")
  })

  observeEvent(input$select_tear, {
    modify_item <- new_fundus_item()
    click_coords <- xy_to_clock_eccentricity(input$select_tear$x, input$select_tear$y)

    modify_item$clock <- click_coords$clock
    modify_item$eccentricity <- click_coords$eccentricity
    modify_item$size <- input$tear_size
    new_fundus_item(modify_item)
  })

  # LaserPlot ---------------------------------------------------------------
  output$laser <- renderPlot({
    laser_item <- new_fundus_item()
    laser_item$path <- laser_path(laser_item)

    background_image <-
      fundus_image(input$eye, append(remaining_fundus_items(), list(laser_item)), clip = FALSE, scale_image = 1)
    background_image <- background_image |>
      svg_to_grob()

    ggplot2::ggplot() +
      ggplot2::annotation_custom(background_image,
        xmin = 0, xmax = 400
      ) +
      ggplot2::coord_equal() +
      ggplot2::theme_void() +
      ggplot2::scale_x_continuous(limits = c(0, 400)) +
      ggplot2::scale_y_continuous(limits = c(0, 400), trans = "reverse")
  })

  observeEvent(input$select_laser, {
    click <- data.frame(cx = input$select_laser$x, cy = input$select_laser$y)
    click_coords <- xy_to_clock_eccentricity(click$cx, click$cy)

    modify_item <- new_fundus_item()
    modify_item$path <- laser_path(modify_item)
    modify_item$clock <- NULL
    modify_item$ecc <- NULL
    modify_item$eccentricity <- NULL

    if (nrow(modify_item$path) > 0) {
      distances <- sqrt((modify_item$path$cx - click$cx)^2 + (modify_item$path$cy - click$cy)^2)
      nearest <- which.min(distances)

      if (distances[nearest] <= 12) {
        modify_item$path <- modify_item$path[-nearest, ]
        new_fundus_item(modify_item)
        return()
      }
    }

    new_laser <- data.frame(
      cx = click$cx,
      cy = click$cy,
      clock = click_coords$clock,
      eccentricity = click_coords$eccentricity
    )

    modify_item$path <- rbind(modify_item$path, new_laser)
    new_fundus_item(modify_item)
  })

  # JSON code ----------------------------------------------------------
  observeEvent(input$show_json, {
    showModal(modalDialog(p(toJSON(fundus_items(), pretty = TRUE, auto_unbox = TRUE))))
  })


  # Undo point --------------------------------------------------------------
  observeEvent(input$delete_point, {
    modify_item <- new_fundus_item()
    req(nrow(modify_item$path) > 0)
    modify_item$path <- modify_item$path[-nrow(modify_item$path), ]
    new_fundus_item(modify_item)
  })

  # Render image ------------------------------------------------------------
  output$svg_image <- renderUI({
    fundus_image(input$eye, fundus_items(), clip = TRUE, scale_image = 1.3) |> HTML()
  })

  # Download image ----------------------------------------------------------
  output$download_word <- downloadHandler(
    filename = function() {
      paste("fundus_image.png")
    },
    content = function(file) {
      fundus_image(input$eye, fundus_items(), clip = TRUE, scale_image = 1) |>
        charToRaw() |>
        rsvg::rsvg_png(file = file, width = 400, height = 400)
      # writeLines(con = file)
    }
  )
}

shinyApp(ui, server)
