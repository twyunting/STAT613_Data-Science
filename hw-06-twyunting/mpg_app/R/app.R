# Yunting Chiu

library(shiny)
library(tidyverse)

# extract mpg with chr types
mpg %>% select_if(is.character)  -> mpgChr

# Inputting to three different variables
ui <- fluidPage(
     
    varSelectInput("x", label = "X variable",
                   data = mpg, selected = "cty"),
    varSelectInput("y", label = "Y variable",
                   data = mpg, selected = "hwy"),
    varSelectInput("color", label = "Color variable (categorical)",
                   data = mpgChr, selected = "class"),
    plotOutput("plot")
)

# Define server logic required to draw a scatterrplot
server <- function(input, output) {
    output$plot <- renderPlot({
        ggplot(mpg, aes(x = !!input$x, y = !!input$y)) +
            geom_point(aes(color = !!input$color)) +
            theme_bw()
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
