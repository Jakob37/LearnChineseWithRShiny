#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         actionButton("iterate", "Click me")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         textOutput("text")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    text_list <- c("a", "b", "c", "d", "e")
    cur_ind <- 1
       
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$text <- renderText({paste("Current letter: ", text_list[cur_ind])})
   
   observeEvent(input$iterate, {
       print("Observed!")
       cur_ind <<- cur_ind + 1
       cur_ind <- (cur_ind - 1) %% length(text_list) + 1
       print(cur_ind)
       # output$text()
       
       output$text <- renderText({paste("Current letter: ", text_list[cur_ind])})
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

