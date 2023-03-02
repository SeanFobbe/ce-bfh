



#' === Packages ===
#' rvest
#' 



f.download_table_make <- function(sleep.min = 0,
                                  sleep.max = 1,
                                  debug = FALSE,
                                  debug.pages = 20){

    offset.url <- f.linkextract("https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/")
    offset.url <- grep("search-form", offset.url, value = TRUE)
    
    offset.raw <- gsub(".*Boffset%5D=([0-9]+)#search-form.*", "\\1", offset.url)        
    offset.max <- max(as.integer(offset.raw))
    offset.all <- seq(0, offset.max, 10)


    if(debug == TRUE){

    offset.all <- sort(sample(offset.all, debug.pages))
        
    }
    
    
    url.all <- paste0("https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/?tx_eossearch_eossearch[offset]=",
                      offset.all,
                      "#search-form")

    
    result.list <- lapply(url.all, f.extract_meta_bfh)
    result.dt <- rbindlist(result.list)




    
    release <- as.Date(release, format = "%d.%m.%Y")

    datum <- as.Date(datum, format = "%d.%m.%Y")

    bfh_id <- gsub(".*\\/(STR.*)\\/", "\\1", url_html)



    
}





f.extract_meta_bfh <- function(url,
                               sleep.min = 0,
                               sleep.max = 1,
                               verbose = FALSE){

    html <- rvest::read_html(url)
    
    release <- rvest::html_nodes(html, "[data-label='Veröffentlichung am']")
    release <- rvest::html_text(release, trim = TRUE)


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

    if(verbose == TRUE){
        
        message(paste("Page", url, "completed."))

    }

    
    Sys.sleep(runif(1, sleep.min, sleep.max))

    return(dt.return)

}






