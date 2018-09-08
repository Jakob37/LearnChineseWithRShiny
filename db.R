library(RMySQL)

# Load options
source("db.options")

# Options format:
# options(mysql = list(
#     "host" = "127.0.0.1",
#     "port" = 3306,
#     "user" = "user",
#     "password" = "pass"
# ))


databaseName <- "learnchinese"
# table <- "responses"

category_table <- "groups"
word_table <- "words"

# Table scheme
# Table 1: Groups linked to each character
#  id group character
# Table 2: Character linked to its information
#  id character english pinying comment
# Table 3: Processing data (for a bit later)

getConn <- function() {
    conn <- dbConnect(
        MySQL(), 
        dbname = databaseName, 
        host = options()$mysql$host, 
        port = options()$mysql$port, 
        user = options()$mysql$user, 
        password = options()$mysql$password)
    
    conn
}

saveEntry <- function(group, character, debug=FALSE) {
    
    db <- getConn()
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
        "INSERT INTO %s (%s) VALUES (%s)",
        category_table, 
        "myid, mygroup, mycharacter",
        paste0('null, ', "'", group, "', ", "'", character, "'")
    )
    
    if (debug) {
        print(paste("Attempting query:", query))
    }

    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
}

saveWord <- function(characters, english, pinying, notes="", debug=FALSE) {
    
    # INSERT INTO words (myid, mycharacter, myenglish, pinying, note) VALUES (null, '力', 'force', 'li4', 'First comment!');
    
    db <- getConn()
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
        "INSERT INTO %s (%s) VALUES (%s)",
        word_table, 
        "myid, mycharacter, myenglish, pinying, note",
        paste0('null, ', "'", characters, "', '", english, "', '", pinying, "', '", notes, "'")
    )
    
    if (debug) {
        print(paste("Attempting query:", query))
    }
    
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
}

loadGroups <- function() {

    # Connect to the database
    db <- dbConnect(
        MySQL(), 
        dbname = databaseName, 
        host = options()$mysql$host, 
        port = options()$mysql$port, 
        user = options()$mysql$user, 
        password = options()$mysql$password)
    
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s ", category_table)
    # Submit the fetch query and disconnect

    dbSendQuery(db, "SET NAMES utf8");
        
    db_df <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    db_df
}

dbDisconnectAll <- function(){
    ile <- length(dbListConnections(MySQL())  )
    lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
    cat(sprintf("%s connection(s) closed.\n", ile))
}


loadGroups()