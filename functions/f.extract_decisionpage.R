#' f.extract_decisionpage
#'
#' Vectorize extraction of Bundesfinanzhof (BFH) decision pages.
#' 
#' @param html A URL to a remote HTML file or path to a local HTML file.
#'
#' @return A data.table containing all relevant metadata and content.

f.extract_decisionpage <- function(html){



    list.result <- lapply(html, f.extract_decisionpage_single)
    dt.return <- data.table::rbindlist(list.result)


    
    return(dt.return)

    

}


#' f.extract_decisionpage_single
#'
#' Extract all metadata and content from a page containing a single Bundesfinanzhof (BFH) decision.
#'
#' @param html A URL to a remote HTML file or path to a local HTML file.
#'
#' @return A data.table containing all relevant metadata and content.

#' Note: Individual parts of the decision are not reliably identifiable by name. Leitsätze can be inferred by position, but not all parts of a decision (Leitsätze, Tenor, Tatbestand, Gründe) are always present, so positional inference is risky for the latter parts of a decision.



f.extract_decisionpage_single <- function(html){
    
    html <- rvest::read_html(html)
    
    ecli <- rvest::html_nodes(html, "[class='ecli highlighted']")
    ecli <- rvest::html_text(ecli, trim = TRUE)
    
    normen <- rvest::html_nodes(html, "[class='norms']")
    normen <- rvest::html_text(normen, trim = TRUE)
    
    vorinstanz <- rvest::html_nodes(html, "[class='vorinstanz']")
    vorinstanz <- rvest::html_text(vorinstanz, trim = TRUE)
    
    url.pdf <- rvest::html_nodes(html, "a")
    url.pdf <- rvest::html_attr(url.pdf, 'href')
    url.pdf <- grep("detail/pdf", url.pdf, value = TRUE)
    url.pdf <- paste0("https://www.bundesfinanzhof.de", url.pdf)

    text <- rvest::html_nodes(html, "[class='m-decisions']") # gesamte Entscheidung
    text <- rvest::html_text(text, trim = TRUE)
    text <- paste0(text, collapse = " ")

    leitsaetze <- rvest::html_nodes(html, "[class='m-decisions']")[1] # leitsätze
    leitsaetze <- rvest::html_text(leitsaetze)
    

    dt.return <- data.table::data.table(ecli = ecli,
                                        normen = normen,
                                        vorinstanz = vorinstanz,
                                        url_pdf = url.pdf,
                                        text = text,
                                        leitsaetze = leitsaetze)

    return(dt.return)
    
}
