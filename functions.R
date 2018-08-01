setup_character_dict <- function() {
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
    
    dict
}

setup_character_stats <- function(dict) {
    
    char_stats <- list()
    
    for (char_ind in seq_len(length(dict))) {
        char_stats[[char_ind]] <- list(right=0, wrong=0)
    }
    
    char_stats
}

get_hint_text <- function(char_entry, hint_level) {
    
    if (hint_level == "None") {
        hint_string <- ""
    }
    else if (hint_level == "Tone") {
        
        tones <- c("á", "ā", "ǎ", "à","a")
        tone_level <- as.numeric(char_entry[4])
        print(paste("Tone level:", tone_level, "tone:", tones[tone_level]))
        hint_string <- paste("Tone:", tones[tone_level])
    }
    else if (hint_level == "Pinying") {
        hint_string <- paste("Pinying:", char_entry[2])
    }
    else if (hint_level == "English") {
        hint_string <- paste("English:", char_entry[3])
    }
    else if (hint_level == "All") {
        hint_string <- paste0(
            "English: ", char_entry[3], ", ", 
            "Pinying: ", char_entry[2])
    }
    else {
        stop(paste("Unknown hint level: ", hint_level))
    }
    
    hint_string
}

get_result_string <- function(char_list, index, active) {
    
    if (active) {
        expected_pinying <- char_list[[index]][2]
        expected_english <- char_list[[index]][3]
        paste("Correct:", expected_english, expected_pinying)
    }
    else {
        "Type your answer..."
    }
}

check_correct <- function(entry, input_pinying, input_english, type) {
    
    expected_pinying <- entry[2]
    expected_english <- entry[3]
    
    if (type == "Both") {
        expected_pinying == input_pinying && expected_english == input_english
    }
    else if (type == "English") {
        expected_english == input_english
    }
    else if (type == "Pinying") {
        expected_english == input_pinying
    }
    else {
        stop(paste("Unknown type:", type))
    }
}

get_char_string <- function(char_list) {
    
    char_vect <- unlist(lapply(
        char_list, 
        function(entry) {
            entry[1]
        }))
    paste(char_vect, collapse=", ")
}

get_parsed_char_string <- function(dict, character_stats) {
 
    gray_level <- 255
    char_gray_level <- 200
    font_size <- 40
    
    char_vect <- c(paste0("<div style='background-color:rgb(",
                   paste(rep(gray_level, 3), collapse=", "),
                   ");'>"))
    for (ind in seq_len(length(dict))) {
        
        entry <- dict[[ind]]
        
        char <- entry[1]
        right <- character_stats[[ind]]$right
        wrong <- character_stats[[ind]]$wrong
        
        print(right, wrong)
        
        new_char <- char
        if (right - wrong > 0) {
            char_vect <- c(char_vect, html(char, font_size, color=c(0, 200, 0)))
        }
        else if (wrong - right > 0) {
            char_vect <- c(char_vect, html(char, font_size, color=c(400, 0, 0)))
        }
        else {
            char_vect <- c(char_vect, html(
                char, 
                font_size,
                color=c(char_gray_level, char_gray_level, char_gray_level)))
        }
    }
    
    char_vect <- c(char_vect, "</div>")
    out <- paste(char_vect, collapse=" ")
    print(out)
    out
}

