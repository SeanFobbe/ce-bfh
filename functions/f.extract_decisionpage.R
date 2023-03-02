




f.extract_decisionpage <- function(url){



    list.result <- lapply(url, f.extract_decisionpage_single)
    dt.return <- data.table::rbindlist(list.result)


    
    return(dt.return)

    

}







f.extract_decisionpage_single <- function(url){

    
    html <- rvest::read_html(url)
    
    ecli <- rvest::html_nodes(html, "[class='ecli highlighted']")
    ecli <- rvest::html_text(ecli, trim = TRUE)
    
    norms <- rvest::html_nodes(html, "[class='norms']")
    norms <- rvest::html_text(norms, trim = TRUE)
    
    vorinstanz <- rvest::html_nodes(html, "[class='vorinstanz']")
    vorinstanz <- rvest::html_text(vorinstanz, trim = TRUE)
    
    url.pdf <- rvest::html_nodes(html, "a")
    url.pdf <- rvest::html_attr(url.pdf, 'href')
    url.pdf <- grep("detail/pdf", url.pdf, value = TRUE)


    dt.return <- data.table::data.table(ecli = ecli,
                                        normen = norms,
                                        vorinstanz = vorinstanz,
                                        url_pdf = url.pdf)

    return(dt.return)
    
}
