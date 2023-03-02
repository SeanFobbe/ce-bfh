#+
#'## f.linkextract: Extract Links from HTML
#' This function extracts all links (i.e. href attributes of <a> tags) from an arbitrary HTML document. Returns "NA" if there is an error.
#'

#' @param URL A valid URL.

#library(rvest)

f.linkextract <- function(URL){
    tryCatch({
        
        temp <- rvest::read_html(URL)
        temp <- rvest::html_nodes(temp, "a")
        rvest::html_attr(temp, 'href')

    },
        error = function(cond) {
            return(NA)}
        )
}
