setup_character_dict <- function() {
    
    dict <- list()
    
    add_entry <- function(dict, ind, char, pin, eng, strokes) {
        
        dict[[ind]] <- list(
            "character" = char,
            "pinying" = pin,
            "english" = eng,
            "strokes" = strokes
        )
        
        dict
    }
    
    dict <- add_entry(dict, 1, "一", "yi1", "one", 1)
    dict <- add_entry(dict, 2, "丨", "gun3", "line", 3)
    dict <- add_entry(dict, 3, "丶", "zhu3", "dot", 3)
    dict <- add_entry(dict, 4, "丿	乀", "fu2", "slash", 2)
    dict <- add_entry(dict, 5, "乙 乚", "yin3", "second", 3)
    dict <- add_entry(dict, 6, "亅", "jue2", "hook", 2)
    dict <- add_entry(dict, 7, "二", "er4", "two", 4)
    dict <- add_entry(dict, 8, "亠", "tou2", "lid", 2)
    dict <- add_entry(dict, 9, "人 亻", "ren2", "man", 2)
    dict <- add_entry(dict, 10, "儿", "er2", "legs", 2)

    dict
}

setup_dict_from_file <- function(filepath, select=0) {
    
    df <- read_tsv(filepath)
    dict <- list()
    
    selected <- 0
    for (ind in seq_len(nrow(df))) {
        row <- df[ind, ]
        dict[[ind]] <- row
        dict[[ind]]$tone <- "Unspecified"
        
        selected <- selected + 1
        
        if (select > 0 && selected >= select) {
            return(dict)
        }
    }
    
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
        # print(paste("Tone level:", tone_level, "tone:", tones[tone_level]))
        hint_string <- paste("Tone:", tones[tone_level])
    }
    else if (hint_level == "Pinying") {
        hint_string <- paste("Pinying:", char_entry$pinying)
    }
    else if (hint_level == "English") {
        hint_string <- paste("English:", char_entry$english)
    }
    else if (hint_level == "All") {
        hint_string <- paste0(
            "English: ", char_entry$english, ", ", 
            "Pinying: ", char_entry$pinying)
    }
    else {
        stop(paste("Unknown hint level: ", hint_level))
    }
    
    hint_string
}

get_result_string <- function(char_list, index, active) {
    
    if (active) {
        expected_pinying <- char_list[[index]]$pinying
        expected_english <- char_list[[index]]$english
        paste("Correct:", expected_english, expected_pinying)
    }
    else {
        "Type your answer to the left..."
    }
}

get_parsed_char_string <- function(dict, character_stats, size, green_threshold) {
 
    gray_level <- 255
    char_gray_level <- 200
    font_size <- size
    
    char_vect <- c(paste0("<div style='background-color:rgb(",
                   paste(rep(gray_level, 3), collapse=", "),
                   ");'>"))
    
    for (ind in seq_len(length(dict))) {
        
        entry <- dict[[ind]]
        
        char <- entry$character
        right <- character_stats[[ind]]$right
        wrong <- character_stats[[ind]]$wrong
        
        green_c <- c(0, 200, 0)
        red_c <- c(400, 0, 0)
        black_c <- c(0, 0, 0)
        gray_c <- c(200, 200, 200)
        
        new_char <- char
        if (wrong + right == 0) {
            char_vect <- c(char_vect, html(
                char, 
                font_size,
                color=c(char_gray_level, char_gray_level, char_gray_level)))
        }
        else if (right - wrong >= green_threshold) {
            char_vect <- c(char_vect, html(char, font_size, color=green_c))
        }
        else if (wrong - right > 0) {
            char_vect <- c(char_vect, html(char, font_size, color=red_c))
        }
        else {
            char_vect <- c(char_vect, html(char, font_size, color=black_c))
        }
    }
    
    char_vect <- c(char_vect, "</div>")
    out <- paste(char_vect, collapse=" ")
    # print(out)
    out
}

