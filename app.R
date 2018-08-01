library(shiny)
# library(shinyWidgets)

source("functions.R")

ui <- fluidPage(
    
    # Press return to trig check
    tags$script(HTML("$(function(){
      $(document).keyup(function(e) {
      if (e.which == 13) {
        $('#iterate').click()
      }
      });
      })")),
    
    
    # Application title
    titlePanel("Learn chinese characters - with RShiny"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = 'hint_level', 
                label = 'Hint level', 
                choices = c("None", "Tone", "Pinying", "English", "All")
            ),
            textInput("pinying", "Enter pinying"),
            textInput("english", "Enter english"),
            actionButton("iterate", "Enter"),
            htmlOutput("result")
        ),
        
        mainPanel(
            htmlOutput("char_display"),
            htmlOutput("text"),
            htmlOutput("statistics"),
            htmlOutput("hint"),
            plotOutput("distPlot")
        )
    )
    
    
)

server <- function(input, output, session) {
    
    cur_ind <- 1
    correct <- 0
    correct_pinying <- 0
    correct_english <- 0
    total <- 0
    
    result_display <- TRUE
    
    dict <- list()
    dict[[1]] <- c("一", "yi1", "one", 1)
    dict[[2]] <- c("丨", "gun3", "line", 3)
    dict[[3]] <- c("丶", "zhu3", "dot", 3)
    dict[[4]] <- c("丿	乀", "fu2", "slash", 2)
    dict[[5]] <- c("乙 乚", "yin3", "second", 3)
    dict[[6]] <- c("亅", "jue2", "hook", 2)
    dict[[7]] <- c("二", "er4", "two", 4)
    dict[[8]] <- c("亠", "tou2", "lid", 2)
    dict[[9]] <- c("人 亻", "ren2", "man", 2)
    dict[[10]] <- c("儿", "er2", "legs", 2)
    
    output$my_button <- renderUI({
        actionButton("iterate", label=paste("text -", cur_ind))
    })
    
    output$char_display <- renderText({
        paste0("<div style='font-size:30px'>", get_char_string(dict), "</div>")
    })
    
    output$hint <- renderText({
        paste0("<div style='font-size:30px'>", 
               get_hint_text(dict[[cur_ind]], input[["hint_level"]]),
               "</div>")
    })

    output$text <- renderText({paste0('<div style="font-size:200px;">', dict[[cur_ind]][1], '</div>')})

    observeEvent(input$iterate, {
        
        # print(get_char_string(dict))
        print("Iterate")
        print(result_display)
        # print(check_correct(dict, cur_ind, input[["pinying"]], input[["english"]]))

        if (result_display) {
            result_display <<- FALSE
            output$result <- renderText(get_result_string(dict, cur_ind))
        }
        else {
            # Iterate update
            result_display <<- TRUE
            output$result <- renderText("")
            total <<- total + 1
            cur_ind <<- cur_ind + 1
            cur_ind <- (cur_ind - 1) %% length(dict) + 1
            print(paste("New index: ", cur_ind))
            
            # Clear previous
            updateTextInput(session, "english", label = NULL, value = "")
            updateTextInput(session, "pinying", label = NULL, value = "")
            
            # input[["pinying"]] <<- ""
            # input[["english"]] <<- ""
            
            # Update character
            output$text <- renderText({paste0('<div style="font-size:200px;">', dict[[cur_ind]][1], '</div>')})
            
            output$my_button <- renderUI({
                actionButton("iterate", label=paste("text -", cur_ind))
            })

            # Hint
            output$hint <- renderText({
                paste0("<div style='font-size:30px'>", 
                       get_hint_text(dict[[cur_ind]], input[["hint_level"]]),
                       "</div>")
            })
            
            # Statistics
            output$statistics <- renderText({
                paste(
                    'Correct: ', correct, 
                    '<br>Correct Pinying: ', correct_pinying, 
                    '<br>Correct English: ', correct_english,
                    '<br>Total: ', total)
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

