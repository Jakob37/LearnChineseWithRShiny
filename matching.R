

update_character_stats <- function(session, dict, character_stats, cur_ind) {
    
    input_english <- session$input$english
    input_pinying <- session$input$pinying
    
    correct <- check_correct(
        dict[[cur_ind]], 
        input_pinying, 
        input_english, 
        type=session$input$practice_type)
    
    if (correct) {
        character_stats[[cur_ind]]$right <- character_stats[[cur_ind]]$right + 1
    }
    else {
        character_stats[[cur_ind]]$wrong <- character_stats[[cur_ind]]$wrong + 1
    }
    
    character_stats
}

select_character_index <- function(dict, character_stats, slice_size=10) {
    
    rank_vector <- c()
    
    for (ind in seq_len(length(dict))) {
        right <- character_stats[[ind]]$right
        wrong <- character_stats[[ind]]$wrong
        rank_vector[as.character(ind)] <- right / (wrong+1)
    }
    
    candidate_vector <- sort(rank_vector)[seq_len(slice_size)]
    pick <- as.numeric(sample(names(candidate_vector), 1))
    print(paste("The pick is:", pick))
    pick
}





