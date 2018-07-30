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
            
            actionButton("iterate", "Click me"),
            # actionButton("iterate2", "Click me 2"),
            # actionButton("iterate3", "Click me 3"),
            uiOutput("my_button"),
            
            textInput("pinying", "Enter pinying"),
            textInput("english", "Enter english"),
            actionButton("enter", "Enter")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("text"),
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    text_list <- c("a", "b", "c", "d", "e")
    cur_ind <- 1
    
    dict <- list()
    dict[[1]] <- c("一", "yi1", "one")
    dict[[2]] <- c("丨", "gun3", "line")
    dict[[3]] <- c("丶", "zhu3", "dot")
    dict[[4]] <- c("丿	乀", "fu2", "slash")
    dict[[5]] <- c("乙	乚", "yin3", "second")
    dict[[6]] <- c("亅", "jue2", "hook")
    dict[[7]] <- c("二", "er4", "two")
    dict[[8]] <- c("亠", "tou2", "lid")
    dict[[9]] <- c("人	亻", "ren2", "man")
    dict[[10]] <- c("儿", "er2", "legs")

    output$my_button <- renderUI({
        actionButton("iterate", label=paste("text -", cur_ind))
    })
    
    output$distPlot <- renderPlot({
        x    <- faithful[, 2] 
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$text <- renderText({paste0('<div style="font-size:200px;">', dict[[cur_ind]][1], '</div>')})
    
        
    observeEvent(input$iterate, {
        
        print("Observed!")
        cur_ind <<- cur_ind + 1
        cur_ind <- (cur_ind - 1) %% length(dict) + 1
        print(cur_ind)
        
        output$text <- renderText({paste0('<div style="font-size:200px;">', dict[[cur_ind]][1], '</div>')})

        output$my_button <- renderUI({
            actionButton("iterate", label=paste("text -", cur_ind))
        })
    })
    
    
    
    observeEvent(input$iterate2, {
        print("Observed!")
        cur_ind <<- cur_ind + 1
        cur_ind <- (cur_ind - 1) %% length(text_list) + 1
        print(cur_ind)
        output$text <- renderText({paste("Current letter: 2")})
    })
    
    observeEvent(input$iterate3, {
        print("Observed!")
        cur_ind <<- cur_ind + 1
        cur_ind <- (cur_ind - 1) %% length(text_list) + 1
        print(cur_ind)
        output$text <- renderText({paste("Current letter: 3")})
    })
    
    observeEvent(input$iterate4, {
        print("Observed!")
        cur_ind <<- cur_ind + 1
        cur_ind <- (cur_ind - 1) %% length(text_list) + 1
        print(cur_ind)
        output$text <- renderText({paste("Current letter: 4")})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

