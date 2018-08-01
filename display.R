render <- function(session, dict, cur_ind, global_stats, character_stats, result_check=FALSE) {
    
    updateTextInput(session, "english", label = NULL, value = "")
    updateTextInput(session, "pinying", label = NULL, value = "")
    
    session$output$my_button <- renderUI({
        actionButton("iterate", label=paste("text -", cur_ind))
    })
    
    session$output$char_display <- renderText({
        paste0("<div style='font-size:30px'>", get_char_string(dict), "</div>")
    })
    
    session$output$hint <- renderText({
        paste0("<div style='font-size:30px'>", 
               get_hint_text(dict[[cur_ind]], session$input$hint_level),
               "</div>")
    })
    
    session$output$text <- renderText({paste0('<div style="font-size:200px;">', dict[[cur_ind]][1], '</div>')})
    
    session$output$statistics <- renderText({
        paste(
            'Correct: ', global_stats[["correct"]],
            '<br>Correct Pinying: ', global_stats[["correct_pinying"]],
            '<br>Correct English: ', global_stats[["correct_english"]],
            '<br>Total: ', global_stats[["total"]])
    })

    session$output$result <- renderText(get_result_string(dict, cur_ind, active=!result_check))
}

show_results <- function(session, dict, cur_ind) {
    print("show results")
    session$output$result <- renderText(get_result_string(dict, cur_ind))
}

character_color <- function() {
    
}



