# Yunting Chiu

library(shiny)
library(ggplot2)
# named the type of plot first
plotTypes <- c("Density Plot", "Histogram", "Frequency Polygon")

# Inputting to three different variables
ui <- fluidPage(
    
    varSelectInput("xVar", label = "X variable",
                   data = mtcars, selected = "mpg"),
    radioButtons(inputId = "plotType", label = "Choose a plot type",
                 choices = plotTypes),
    plotOutput("plot")
)

# Output
server <- function(input, output) {
    output$plot <- renderPlot({
        plotType <- switch(
            input$plotType,
            "Density Plot" = ggplot(mtcars, aes(x = !!input$xVar)) +
            geom_density(outline.type = "full") +
            theme_bw(),
            "Histogram" = ggplot(mtcars, aes(x = !!input$xVar)) +
                geom_histogram() +
                theme_bw(),
            "Frequency Polygon" = ggplot(mtcars, aes(x = !!input$xVar)) +
                geom_freqpoly() +
                theme_bw()
        )
        plot(plotType)
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
