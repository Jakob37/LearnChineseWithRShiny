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
        char_stats[[char_ind]] <- 0
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

get_char_string <- function(char_list) {
    
    char_vect <- unlist(lapply(
        char_list, 
        function(entry) {
            entry[1]
        }))
    paste(char_vect, collapse=", ")
}

check_correct <- function(char_list, index, input_pinying, input_english) {
    
    expected_pinying <- char_list[[index]][2]
    expected_english <- char_list[[index]][3]
    expected_pinying == input_pinying && expected_english == input_english
}

html <- function(string, size=NULL, color=NULL) {
    
    if (!is.null(size)) {
        paste0("<div style='font-size:", size, "px'>", string, "</div>")
    }
    else {
        paste0("<div>", string, "</div>")
    }
}









