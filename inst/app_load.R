library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("svg_file", "Upload JSON"),
    ),
    mainPanel(
      htmlOutput("svg_image")
    )
  )
)

server <- function(input, output, session) {

  output$svg_image <- renderUI({
    file <- input$svg_file
    req(file)

    image <- readLines(file$datapath)
    HTML(image)
  })

}

shinyApp(ui, server)
