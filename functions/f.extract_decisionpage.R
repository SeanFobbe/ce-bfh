#' f.extract_decisionpage
#'
#' Vectorize extraction of Bundesfinanzhof (BFH) decision pages.
#' 
#' @param html A URL to a remote HTML file or path to a local HTML file.
#'
#' @return A data.table containing all relevant metadata and content.

f.extract_decisionpage <- function(x){

    list.result <- lapply(x, f.extract_decisionpage_single)
    dt.return <- data.table::rbindlist(list.result)
    
    return(dt.return)
}


#' f.extract_decisionpage_single
#'
#' Extract all metadata and content from a page containing a single Bundesfinanzhof (BFH) decision.
#'
#' @param x Path to a local HTML file or a URL to a remote HTML file.
#'
#' @return A data.table containing all relevant metadata and content.

#' Note: Individual parts of the decision are not reliably identifiable by name. Leitsätze can be inferred by position, but not all parts of a decision (Leitsätze, Tenor, Tatbestand, Gründe) are always present, so positional inference is risky for the latter parts of a decision.



f.extract_decisionpage_single <- function(x){
    
    bfh_id <- gsub("\\.html", "", basename(x))
    
    html <- rvest::read_html(x)

    ## ECLI
    ecli <- rvest::html_nodes(html, "[class='ecli highlighted']")
    ecli <- rvest::html_text(ecli, trim = TRUE)
    ecli <- ifelse(length(ecli) == 0, NA, ecli)


    ## Normen
    normen <- rvest::html_nodes(html, "[class='norms']")
    normen <- rvest::html_text(normen, trim = TRUE)
    normen <- ifelse(length(normen) == 0, NA, normen)

    ## Vorinstanz
    vorinstanz <- rvest::html_nodes(html, "[class='vorinstanz']")
    vorinstanz <- rvest::html_text(vorinstanz, trim = TRUE)
    vorinstanz <- ifelse(length(vorinstanz) == 0, NA, vorinstanz)

    ## URL PDF
    url.pdf <- rvest::html_nodes(html, "a")
    url.pdf <- rvest::html_attr(url.pdf, 'href')
    url.pdf <- grep("detail/pdf", url.pdf, value = TRUE)

    url.pdf <- ifelse(length(url.pdf) == 0,
                      NA,
                      url.pdf <- paste0("https://www.bundesfinanzhof.de", url.pdf))

    ## Text
    text <- rvest::html_nodes(html, "[class='m-decisions']") # gesamte Entscheidung
    text <- rvest::html_text(text, trim = TRUE)
    text <- ifelse(length(text) == 0, NA, text <- paste0(text, collapse = " "))

    ## Leitsatz
    text_leitsatz <- rvest::html_nodes(html, "[class='m-decisions']")[1] # leitsätze
    text_leitsatz <- rvest::html_text(text_leitsatz)
    text_leitsatz <- ifelse(length(text_leitsatz) == 0, NA, text_leitsatz)
    

    ## Create Return Table
    dt.return <- data.table::data.table(bfh_id = bfh_id,
                                        ecli = ecli,
                                        normen = normen,
                                        vorinstanz = vorinstanz,
                                        url_pdf = url.pdf,
                                        text = text,
                                        text_leitsatz = text_leitsatz)

    return(dt.return)
    
}
