html <- function(string, size=NULL, color=NULL) {
    
    if (!is.null(size)) {
        size_part <- paste0("font-size:", size, "px;")
    }
    else {
        size_part <- ""
    }
    
    if (!is.null(color)) {
        color_part <- paste0("color:", color, ";")
    }
    else {
        color_part <- ""
    }
    
    if (!is.null(size) || !is.null(color)) {
        out <- paste0("<span style='", size_part, color_part, "'>", string, "</span>")
    }
    else {
        out <- paste0("<span>", string, "</span>")
    }
    
    # print(out)
    out
}

