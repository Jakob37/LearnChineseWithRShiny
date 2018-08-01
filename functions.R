test_print <- function() {
    print("Executing a test print")
}

get_hint_text <- function(char_entry, hint_level) {
    
    if (hint_level == "None") {
        hint_string <- ""
    }
    else if (hint_level == "Tone") {
        
        tones <- c("má", "mā", "mǎ", "mà","ma")
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

get_result_string <- function(char_list, index) {
    expected_pinying <- char_list[[index]][2]
    expected_english <- char_list[[index]][3]
    paste("Correct:", expected_english, expected_pinying)
}

get_char_string <- function(char_list) {
    
    char_vect <- unlist(lapply(
        char_list, 
        function(entry) {
            entry[1]
        }))
    paste(char_vect, collapse=", ")
}

check_correct <- function(char_list, index, input_pinying, input_english, debug=TRUE) {
    
    expected_pinying <- char_list[[index]][2]
    expected_english <- char_list[[index]][3]

    if (debug) {
        print(paste(
            "Input: ",
            input_pinying,
            input_english,
            "Correct: ",
            expected_pinying,
            expected_english
        ))
    }
        
    expected_pinying == input_pinying && expected_english == input_english
}