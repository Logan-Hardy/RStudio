library(shiny)
library(tidyverse) # Add tidyverse and shinyWidgets packages
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Create a background color with setBackgroundColor(color = "#ebf4ff")
    setBackgroundColor(color='#ebf4ff'),
    
    # Application title
    titlePanel("Diamonds"), # Change the title of the Shiny App 

    # This is where we put the layout.
    sidebarLayout(
        sidebarPanel(
            # Here is where we put the inputs starting with a select input for us to choose a numeric variable for the histogram.
            selectInput(inputId = "hist_select", label="Select a variable for the histogram", 
                        choices = names(diamonds)),
            # Then a slider to select the number of bins. 
            sliderInput(inputId = "bins", label= "Number of bins: ", 
                        min=1, 
                        max=50, 
                        value=25),
            # Then another select input to pick the categorical variable for a bar chart. 
            selectInput(inputId = "bar_select", label="select variable for the bar graph", 
                        choices = list("cut", "color", "clarity"))
            ),
             

       
        mainPanel(
           # Here we'll render the plots we are creating. First the histogram...
           plotOutput("histPlot"), 
           # Then the bar chart. 
          plotOutput("barPlot")
        )
    )
)

# The server is where we write out the code for our outputs. 
server <- function(input, output) {
    
    # We need to write out our code for how to make the histogram. 
    output$histPlot <- renderPlot((
      ggplot(diamonds) +geom_histogram(aes_string(input$hist_select), fill="lightblue", bins=input$bins) + 
        theme_minimal()
    ))
    
    # We need to write out our code for how to make the histogram. 
    output$barPlot <- renderPlot({
      ggplot(diamonds) + geom_bar(aes_string(input$bar_select, fill=input$bar_select)) +
        theme_minimal()
    })
    # output$barPlot <- renderPlot({
    #   ggplot(diamonds, aes_string(input$bar_select, fill = input$bar_select)) + geom_bar() + theme_minimal()
    # })
   
}

shinyApp(ui = ui, server = server)
