library(shiny)
library(tidyverse)
# library(shinyWidgets)

source("functions.R")
source("display.R")
source("util.R")
source("algorithm.R")
source("classes/character_group.R")
source("db.R")

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
    fluidRow(
        
        column(12,
               tabsetPanel(
                   tabPanel("Typing",
                            fluidRow(
                                column(4,
                                       htmlOutput("text")
                                ),
                                column(8,
                                       htmlOutput("char_display")
                                )
                            ),
                            fluidRow(
                                column(12,
                                       htmlOutput("statistics"),
                                       htmlOutput("hint"),
                                       htmlOutput("result")
                                )
                            ),
                            hr(),
                            fluidRow(
                                column(12,
                                       textInput("pinying", "Enter pinying"),
                                       textInput("english", "Enter english"),
                                       actionButton("iterate", "Enter")
                                )
                                
                            )
                   ),
                   tabPanel("Settings",
                            selectInput(
                                inputId = 'hint_level', 
                                label = 'Hint level', 
                                choices = c("None", "Tone", "Pinying", "English", "All")
                            ),
                            selectInput(
                                inputId = 'practice_type', 
                                label = 'Practice type', 
                                choices = c("English", "Pinying", "Both")
                            ),
                            selectInput(
                                inputId = 'threshold', 
                                label = 'Done threshold', 
                                choices = c(1, 2, 3)
                            )
                   ),
                   tabPanel("Character groups",
                            fluidRow(
                                column(4,
                                       uiOutput("char_groups")
                                ),
                                column(4,
                                       textInput("new_group_name", "New group")
                                ),
                                column(4,
                                       actionButton("create_new_group", "Create new group")
                                ),
                                tags$style(type="text/css", "#create_new_group {margin-top: 25px}")
                            ),
                            fluidRow(
                                column(12,
                                       uiOutput("char_details")
                                )
                            ),
                            hr(),
                            fluidRow(
                                column(2, textInput("new_entry_char", "Character")),
                                column(2, textInput("new_entry_english", "English")),
                                column(2, textInput("new_entry_pinying", "Pinying")),
                                column(2, textInput("new_entry_comment", "Comment")),
                                column(4, actionButton("create_new_entry", "New Entry")),
                                tags$style(type="text/css", "#create_new_entry {margin-top: 25px}")
                            )
                   )
               )
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
    dict <- setup_dict_from_file("data/radicals.txt", select=0)
    character_stats <- setup_character_stats(dict)
    render(session, dict, cur_ind, global_stats, character_stats, result_display)
    cur_ind <<- sample(seq_len(length(dict)), 1)
    
    observeEvent(input$iterate, {
        
        if (first) {
            first <<- FALSE
            result_display <<- TRUE
            character_stats <<- update_character_stats(
                session, dict, character_stats, cur_ind)
        }
        else if (!result_display) {
            result_display <<- TRUE
            character_stats <<- update_character_stats(
                session, dict, character_stats, cur_ind)
        }
        else {
            result_display <<- FALSE
            global_stats[["total"]] <<- global_stats[["total"]] + 1
            cur_ind <<- select_character_index(
                dict, 
                character_stats, 
                as.numeric(input$threshold),
                slice_size=15)
        }
        render(session, dict, cur_ind, global_stats, 
               character_stats, result_display, as.numeric(input$threshold))
    })
    
    observeEvent(input$create_new_group, {
        if (input$new_group_name != "") {
            saveEntry(input$new_group_name, "", debug=TRUE)
        }
        updateTextInput(session, "new_group_name", label = NULL, value = "")
        render(session, dict, cur_ind, global_stats, 
               character_stats, result_display, as.numeric(input$threshold))    })
    
    observeEvent(input$create_new_entry, {
        
        if (input$new_entry_char != "" &&
            input$new_entry_english != "" &&
            input$new_entry_pinying != "") {
            
            saveWord(
                input$new_entry_char, 
                input$new_entry_english, 
                input$new_entry_pinying, 
                input$new_entry_comment,
                debug=TRUE)
            updateTextInput(session, "new_entry_char", label = NULL, value = "")
            updateTextInput(session, "new_entry_english", label = NULL, value = "")
            updateTextInput(session, "new_entry_pinying", label = NULL, value = "")
            updateTextInput(session, "new_entry_comment", label = NULL, value = "")
            message("New entry successfully created")
            render(session, dict, cur_ind, global_stats, 
                   character_stats, result_display, as.numeric(input$threshold))        
            }
        else {
            message("Unable to create new entry, at least one field is missing")
        }
    })
}

shinyApp(ui = ui, server = server)

