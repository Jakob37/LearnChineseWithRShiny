
get_groups <- function() {
    
    group_df <- loadGroups()
    
    character_entries <- list()
    groups <- list()

    for (i in seq_len(nrow(group_df))) {
        
        row <- group_df[i, ]
        group <- row$mygroup
        character <- row$mycharacter
        char_entry <- CharacterEntry$new(character, "ping", "eng", "comm")
        character_entries[[character]] <- char_entry
        
        if (!(group %in% names(groups))) {
            group_entry <- CharacterGroup$new(group)
            groups[[group]] <- group_entry
        }

        if (character != "") {
            groups[[group]]$add_character(char_entry)
        }
    }
    
    groups
}

render <- function(session, dict, cur_ind, global_stats, character_stats, 
                   result_check=FALSE, threshold=1) {

    groups <- get_groups()
    
    session$output$char_groups <- renderUI({
        selectInput("char_groups", "Select word group", names(groups))
    })
    
    session$output$char_details <- renderText({
        
        selected_group <- session$input$char_groups
        if (selected_group %in% names(groups)) {
            vapply(
                groups[[selected_group]]$get_char_list(), 
                function(word) {
                    paste0("<div>", word, "</div>")
                },
                ""
            )            
        }
        else {
            "No words to show"
        }
    })
    
    updateTextInput(session, "english", label = NULL, value = "")
    updateTextInput(session, "pinying", label = NULL, value = "")
    
    session$output$my_button <- renderUI({
        actionButton("iterate", label=paste("text -", cur_ind))
    })
    
    green_thres <- as.numeric(threshold)
    
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


