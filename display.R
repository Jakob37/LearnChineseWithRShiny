
get_groups <- function() {
    
    group_df <- loadTableToDf("groups")
    character_entries_table <- loadTableToDf("words")
    character_entries <- list()
    
    for (i in seq_len(nrow(character_entries_table))) {
        char_row <- character_entries_table[i, ]
        character <- char_row$mycharacter
        english <- char_row$myenglish
        pinying <- char_row$pinying
        note <- char_row$note
        
        char_entry <- CharacterEntry$new(character, english, pinying, note)
        character_entries[[character]] <- char_entry
    }
    
    groups <- list()
    groups[["All"]] <- CharacterGroup$new("All")
    
    # Next: Load the character entries from file
    # Then distribute these among the different groups
    
    for (i in seq_len(nrow(group_df))) {
        
        row <- group_df[i, ]
        group <- row$mygroup
        character <- row$mycharacter
        
        if (!(group %in% names(groups))) {
            group_entry <- CharacterGroup$new(group)
            groups[[group]] <- group_entry
        }

        if (character != "") {
            if (character %in% names(character_entries)) {
                groups[[group]]$add_character(character_entries[[character]])
            }
            else {
                warning("Character not available: ", character)
            }
        }
    }
    
    for (entry_name in names(character_entries)) {
        if (entry_name != "") {
            char_entry <- character_entries[[entry_name]]
            groups[["All"]]$add_character(char_entry)
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

    session$output$out_table <- renderTable({
        selected_group <- session$input$char_groups
        available_chars <- names(groups[[selected_group]]$characters)
        character_entries_table <- loadTableToDf("words") %>% filter(mycharacter %in% available_chars)
        character_entries_table
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


