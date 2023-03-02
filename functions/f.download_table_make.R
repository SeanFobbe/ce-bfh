



#' === Packages ===
#' rvest
#' 



f.download_table_make <- function(){


    html <- rvest::read_html("https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/")

    
https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/?tx_eossearch_eossearch[offset]=0#search-form


    

}



f.extract_meta_bfh <- function(html){


    release <- rvest::html_nodes(html, "[data-label='Veröffentlichung am']")
    release <- rvest::html_text(release, trim = TRUE)
    release <- as.Date(release, format = "%d.%m.%Y")

    slg <- rvest::html_nodes(html, "[data-label='V/NV']")
    slg <- rvest::html_text(slg, trim = TRUE)

    spruchkoerper_db <- rvest::html_nodes(html, "[data-label='Senat']")
    spruchkoerper_db <- rvest::html_text(spruchkoerper_db, trim = TRUE)

    datum <- rvest::html_nodes(html, "[data-label='Entscheidung am']")
    datum <- rvest::html_text(datum, trim = TRUE)

    az <- rvest::html_nodes(html, "[data-label='Aktenzeichen']")
    az <- rvest::html_text(az, trim = TRUE)

    titel <- rvest::html_nodes(html, "[data-label='Titel']")
    titel <- rvest::html_text(titel, trim = TRUE)

    url_html <- rvest::html_nodes(html, "[data-label='Titel']")
    url_html <- rvest::html_nodes(url_html, "a")
    url_html <- rvest::html_attr(url_html, name = "href")
    url_html <- paste0("https://www.bundesfinanzhof.de/", url_html)
    

    dt.return <- data.table::data.table(release = release,
                                        slg = slg,
                                        spruchkoerper_db = spruchkoerper_db,
                                        datum = datum,
                                        az = az,
                                        titel = titel,
                                        url_html = url_html)

    return(dt.return)

}






