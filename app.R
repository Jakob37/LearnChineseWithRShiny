#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(shinyWidgets)

source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Learn chinese characters - with RShiny"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            # 
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            
            # actionButton("iterate", "Click me"),
            # actionButton("iterate2", "Click me 2"),
            # actionButton("iterate3", "Click me 3"),
            # uiOutput("my_button"),
            
            textInput("pinying", "Enter pinying"),
            textInput("english", "Enter english"),
            actionButton("iterate", "Enter")

            # checkboxGroupButtons(
            #     inputId = "somevalue", label = "Make a choice :", 
            #     choices = c("Choice A", "Choice B", " Choice C", "Choice D"), 
            #     justified = TRUE, status = "primary",
            #     checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            # )
            
                        
            # dropdownButton(
            #     selectInput(inputId = 'xcol', label = 'X Variable', choices = c("A", "B"))
            # )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("char_display"),
            htmlOutput("text"),
            htmlOutput("statistics"),
            htmlOutput("Hint:"),
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    cur_ind <- 1
    
    correct <- 0
    correct_pinying <- 0
    correct_english <- 0
    total <- 0
    
    dict <- list()
    dict[[1]] <- c("一", "yi1", "one")
    dict[[2]] <- c("丨", "gun3", "line")
    dict[[3]] <- c("丶", "zhu3", "dot")
    dict[[4]] <- c("丿	乀", "fu2", "slash")
    dict[[5]] <- c("乙 乚", "yin3", "second")
    dict[[6]] <- c("亅", "jue2", "hook")
    dict[[7]] <- c("二", "er4", "two")
    dict[[8]] <- c("亠", "tou2", "lid")
    dict[[9]] <- c("人 亻", "ren2", "man")
    dict[[10]] <- c("儿", "er2", "legs")

    output$my_button <- renderUI({
        actionButton("iterate", label=paste("text -", cur_ind))
    })
    
    output$char_display <- renderText({
        paste0("<div style='font-size:30px'>", get_char_string(dict), "</div>")
    })
    
    # output$distPlot <- renderPlot({
    #     x    <- faithful[, 2] 
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
    output$text <- renderText({paste0('<div style="font-size:200px;">', dict[[cur_ind]][1], '</div>')})

    observeEvent(input$iterate, {
        
        print(get_char_string(dict))

        print(check_correct(dict, cur_ind, input[["pinying"]], input[["english"]]))
                
        total <<- total + 1
        cur_ind <<- cur_ind + 1
        cur_ind <- (cur_ind - 1) %% length(dict) + 1
        print(paste("New index: ", cur_ind))
        
        output$text <- renderText({paste0('<div style="font-size:200px;">', dict[[cur_ind]][1], '</div>')})

        output$my_button <- renderUI({
            actionButton("iterate", label=paste("text -", cur_ind))
        })
        
        output$statistics <- renderText({
            paste(
                'Correct: ', correct, 
                '<br>Correct Pinying: ', correct_pinying, 
                '<br>Correct English: ', correct_english,
                '<br>Total: ', total)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

