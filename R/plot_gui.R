ui <- fluidPage(


  titlePanel("JointNet GUI"),

  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      textInput("result","choose a graph to plot"),

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
  output$plot <- shiny::renderPlot({
    #layout = layout_nicely(returngraph(eval(as.name(input$result))), dim = 2)
    par(mfrow = c(1, 1))
    if (input$result != ''){
    plot(x = eval(as.name(input$result)),
         type = input$type,
         index = as.integer(input$index),
         subID = if (as.integer(input$subID) == -1) NULL else as.integer(input$subID)
    )
    }
  })

}

app = shiny::shinyApp(ui = ui, server = server)


#' GUI of JointNets plot
#'
#' GUI version of JointNets plot (input from the global environment)
#' @export
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @details please refer to plot.simule, plot.wsimule and etc for details in plotting.
#' value -1 for subID and index corresponds to NUL value
#' @import shiny
#' @importFrom graphics par
#' @importFrom graphics plot
#' @examples
#' library(JointNets)
#' if(interactive()){
#' plot_gui()
#' }
plot_gui <- function(){
  shiny::runApp(app)
}

