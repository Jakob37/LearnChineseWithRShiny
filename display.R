render <- function(session, dict, cur_ind, global_stats, character_stats, result_check=FALSE) {
    
    updateTextInput(session, "english", label = NULL, value = "")
    updateTextInput(session, "pinying", label = NULL, value = "")
    
    session$output$my_button <- renderUI({
        actionButton("iterate", label=paste("text -", cur_ind))
    })
    
    green_thres <- 3
    
    session$output$char_display <- renderText({
        get_parsed_char_string(dict, character_stats, 20, green_threshold = green_thres)
    })
    
    session$output$hint <- renderText({
        html(paste(get_hint_text(dict[[cur_ind]], session$input$hint_level)), size=30)
    })
    
    session$output$text <- renderText({html(dict[[cur_ind]]$character, size=200)})
    
    session$output$statistics <- renderText({
        html(paste('Total tries:', global_stats[["total"]]), size=30)
    })

    session$output$result <- renderText(
        html(get_result_string(dict, cur_ind, active=result_check), size=30))
}

show_results <- function(session, dict, cur_ind) {
    session$output$result <- renderText(get_result_string(dict, cur_ind))
}

character_color <- function() {
    
}



