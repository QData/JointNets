library(shiny)


ui <- fluidPage(


  titlePanel("JointNet GUI"),

  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      textInput("result","choose a graph to plot"),
      actionButton("confirm", "confirm"),
      selectInput("type", "choose the graph type:",
                  choices = c("task","share","taskspecific","neighbour")),
      conditionalPanel("input.type == 'neighbour'",
                       selectInput("neighbouroption", "choose a neighbour option:",
                                   choices = c("task","taskspecific"))
      ),
      conditionalPanel("input.type == 'neighbour'",
                       numericInput("index", "choose node id to zoom in", 1,
                                    1, 1000, 1)
      ),

      conditionalPanel("input.type != 'share'",
                       numericInput("subID", "choose a context id", -1,
                                    -1, 100, 1)
      )

      ##actionButton("update", "Plot")

    ),

    mainPanel(
      plotOutput("plot")
    )
  )

)

server <- function(input, output) {
  v <- reactiveValues()

  observeEvent(input$confirm, {
    v = 5
    #v = input$result
    #v = eval(as.name(input$result))
  })

  output$plot <- renderPlot({
      par(mfrow = c(1, 1))
    #layout = layout_nicely(returngraph(eval(as.name(input$result))), dim = 2)
      plot(v)

      plot(x = eval(as.name(v)),
           type = input$type,
           index = as.integer(input$index),
           subID = if (as.integer(input$subID) == -1) NULL else as.integer(input$subID)
      )
  })

}

app = shinyApp(ui = ui, server = server)

#' GUI version of plot
#' @export
plot_gui <- function(){
  runApp(app)
}

