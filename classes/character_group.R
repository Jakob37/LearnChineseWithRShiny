library(R6)

CharacterGroup <- R6Class(
    "CharacterGroup",
    public = list(
        name = NULL,
        characters = list(),
        initialize = function(name, characters=NULL) {
            
            message("Initializing character group: ", name)
            self$name <- name
            if (!is.null(characters)) {
                self$characters <- characters
            }
        },
        add_character = function(character) {
            self$characters[[character$character]] <- character
        },
        setup_from_save = function() {
            
        },
        get_char_list = function() {
            vapply(
                self$characters,
                function(char_entry) {
                    char_entry$to_string()
                },
                ""
            )
        }
    ),
    private = list()
)

EntryStats <- R6Class(
    "GroupStats",
    public = list(
        character = NULL,
        sessions = c(),
        
        initialize = function(sessions=NULL) {
            if (!is.null(sessions)) {
                self$sessions <- sessions
            }
        }
    )
)

# Key issue: Identify the 'status'

EntrySession <- R6Class(
    "EntrySession",
    public = list(
        entries = c(),
        initialize = function(entries=NULL) {
            if (!is.null) {
                entries <- entries
            }
        },
        add_entry = function(status, english=NULL, pinying=NULL) {
            time <- format(Sys.time(), "%H%m")
            date <- format(Sys.time(), "%Y%m%d")
            entries <- c(entries, Entry$new(status, date, time, english=english, pinying=pinying))
        }
    )
)

Entry <- R6Class(
    "Entry",
    public = list(
        status = NULL,
        date = NULL,
        time = NULL,
        english = NULL,
        pinying = NULL,
        initialize = function(status, date, time, english=NULL, pinying=NULL) {
            self$status <- status
            self$date <- date
            self$time <- time
            self$english <- english
            self$pinying <- pinying
        }
    )
)

CharacterEntry <- R6Class(
    "CharacterEntry",
    public = list(
        character = NULL,
        pinying = NULL,
        english = NULL,
        comment = NULL,
        statistics = NULL,
        initialize = function(character, pinying, english, comment) {
            message("Initializing character: ", character)
            self$character <- character
            self$pinying <- pinying
            self$english <- english
            self$comment <- comment
        },
        to_string = function() {
            paste(self$character, self$pinying, self$english, self$comment)
        },
        to_html = function() {
            paste0(
                "<div class='row'><div class='col'>", 
                self$character, 
                "</div><div class='col'>", 
                self$pinying, 
                "</div><div class='col'>", 
                self$english, 
                "</div><div class='col'>", 
                self$comment, 
                "</div></div>")
        }
    )
)





