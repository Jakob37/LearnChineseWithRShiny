

update_character_stats <- function(session, dict, character_stats, cur_ind, debug=T) {
    
    input_english <- session$input$english
    input_pinying <- session$input$pinying
    
    correct <- check_correct(
        dict[[cur_ind]], 
        input_pinying, 
        input_english, 
        type=session$input$practice_type,
        debug=debug)
    
    if (correct) {
        character_stats[[cur_ind]]$right <- character_stats[[cur_ind]]$right + 1
    }
    else {
        character_stats[[cur_ind]]$wrong <- character_stats[[cur_ind]]$wrong + 1
    }
    
    if (debug) {
        print(paste("Correct: ", character_stats[[cur_ind]]$right, 
              "Wrong: ", character_stats[[cur_ind]]$wrong))
    }
    
    character_stats
}

check_correct <- function(entry, input_pinying, input_english, type, debug=F) {
    
    expected_pinying <- entry$pinying
    expected_english <- entry$english
    
    if (type == "Both") {
        result <- expected_pinying == input_pinying && expected_english == input_english
    }
    else if (type == "English") {
        result <- expected_english == input_english
    }
    else if (type == "Pinying") {
        result <- expected_english == input_pinying
    }
    else {
        stop(paste("Unknown type:", type))
    }
    
    print(paste0(
        result,
        ", Expected: '", expected_english, "', '", expected_pinying, "'",
        " Observed: '", input_english, "', '", input_pinying, "'"))
    
    result
}

select_character_index <- function(dict, character_stats, threshold, slice_size=10) {
    
    rank_vector <- c()
    
    calculate_weight <- function(right, wrong) {
        right / (wrong + 1) - wrong / 5
    }
    
    for (ind in seq_len(length(dict))) {
        
        right <- character_stats[[ind]]$right
        wrong <- character_stats[[ind]]$wrong
        
        if (right - wrong < threshold) {
            rank_vector[as.character(ind)] <- calculate_weight(right, wrong)
        }
    }
    
    if (length(rank_vector) == 0) {
        stop("YOU WIN")
    }
    
    rank_vector <- sample(rank_vector)
    # print(rank_vector)
    candidate_vector <- sort(rank_vector)[seq_len(slice_size)]
    # print(candidate_vector)
    pick <- as.numeric(sample(names(candidate_vector), 1))
    # print(pick)
    pick
}

