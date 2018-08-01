library(shiny)
# library(shinyWidgets)

source("functions.R")
source("display.R")

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
            actionButton("iterate", "Enter")
        ),
        
        mainPanel(
            htmlOutput("char_display"),
            htmlOutput("text"),
            htmlOutput("statistics"),
            htmlOutput("hint"),
            htmlOutput("result"),
            plotOutput("distPlot")
        )
    )
)

server <- function(input, output, session) {

    global_stats <- list(
        "correct" = 0,
        "correct_pinying" = 0,
        "correct_english" = 0,
        "total" = 0
    )

    first <- TRUE
    result_display <- FALSE
    dict <- setup_character_dict()
    character_stats <- setup_character_stats(dict)
    render(session, dict, cur_ind, global_stats, character_stats, result_display)
    cur_ind <<- sample(seq_len(length(dict)), 1)
    
    observeEvent(input$iterate, {
        
        if (first) {
            first <<- FALSE
            result_display <<- TRUE
        }
        else if (!result_display) {
            result_display <<- TRUE
        }
        else {
            result_display <<- FALSE
            global_stats[["total"]] <<- global_stats[["total"]] + 1
            cur_ind <<- sample(seq_len(length(dict)), 1)
        }
        render(session, dict, cur_ind, global_stats, character_stats, result_display)
    })
}

shinyApp(ui = ui, server = server)

