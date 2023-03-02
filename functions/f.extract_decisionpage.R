




f.extract_decisionpage <- function(url){



    list.result <- lapply(url, f.extract_decisionpage_single)
    dt.return <- rbind(list.result)


    
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
    
    url <- rvest::html_nodes(html, "a")
    url <- rvest::html_attr(url, 'href')
    url <- grep("detail/pdf", url, value = TRUE)


    dt.return <- data.table(ecli = ecli,
                            normen = norms,
                            vorinstanz = vorinstanz,
                            url_pdf = url)

    return(dt.return)
    
}
