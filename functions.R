test_print <- function() {
    print("Executing a test print")
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