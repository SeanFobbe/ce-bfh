


#' Note: offsets of 10000 and more yield errors. Need to reverse list direction to acquire additional decisions.



f.download_table_make <- function(sleep.min = 0.5,
                                  sleep.max = 2,
                                  debug.toggle = FALSE,
                                  debug.pages = 20){



    ## Calculate Offsets
    
    offset.url <- f.linkextract("https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/")
    offset.url <- grep("search-form", offset.url, value = TRUE)
    
    offset.raw <- gsub(".*Boffset%5D=([0-9]+)#search-form.*", "\\1", offset.url)        
    offset.max <- max(as.integer(offset.raw))
    
    offset.descending <- seq(0, 9900, 10)
    offset.ascending <- seq(0, offset.max - 10000 + 50, 10)
    

    if(debug.toggle == TRUE){

        offset.descending <- sort(sample(offset.descending, debug.pages))
        
    }


    
    ## Create URLs
    
    url.descending <- paste0("https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/?tx_eossearch_eossearch[offset]=",
                             offset.descending,
                             "#search-form")


    url.ascending <- paste0("https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/?tx_eossearch_eossearch%5BdateRange%5D%5Bend%5D=02.03.2023&tx_eossearch_eossearch%5BdateRange%5D%5Bstart%5D=01.01.2010&tx_eossearch_eossearch%5Boffset%5D=",
                            offset.ascending,                          "&tx_eossearch_eossearch%5BsearchTerms%5D%5Baktenzeichen%5D=&tx_eossearch_eossearch%5BsearchTerms%5D%5Becli%5D=&tx_eossearch_eossearch%5BsearchTerms%5D%5Bnorm%5D=&tx_eossearch_eossearch%5BsearchTerms%5D%5BsearchTerm%5D=&tx_eossearch_eossearch%5BsearchTerms%5D%5Bsorting%5D=publishingDateAsc&tx_eossearch_eossearch%5B__referrer%5D%5B%40action%5D=index&tx_eossearch_eossearch%5B__referrer%5D%5B%40controller%5D=Standard&tx_eossearch_eossearch%5B__referrer%5D%5B%40extension%5D=&tx_eossearch_eossearch%5B__referrer%5D%5B%40request%5D=%7B%22%40extension%22%3Anull%2C%22%40controller%22%3A%22Standard%22%2C%22%40action%22%3A%22index%22%7D1716fe92177ae4bddcb343e9b5aa7abccc8ba164&tx_eossearch_eossearch%5B__referrer%5D%5Barguments%5D=YTowOnt91322d93bbcf98b6ec3afd3d809b3d4993a5b1aa1&tx_eossearch_eossearch%5B__trustedProperties%5D=%7B%22searchTerms%22%3A%7B%22aktenzeichen%22%3A1%2C%22ecli%22%3A1%2C%22norm%22%3A1%2C%22searchTerm%22%3A1%2C%22sorting%22%3A1%7D%2C%22dateRange%22%3A%7B%22start%22%3A1%2C%22end%22%3A1%7D%7D3fb8b123566ceaf4e15c2270602193b86fb9f80c&cHash=2b3bc2a63c3ca7f099e90e5c9762ebe7#search-form")


    
    ## Run Extraction: Descending
    list.descending <- lapply(url.descending,
                              f.extract_meta_bfh,
                              sleep.min = sleep.min,
                              sleep.max = sleep.max)
    
    dt.descending <- data.table::rbindlist(list.descending)


    ## Run Extraction: Ascending
    list.ascending <- lapply(url.ascending,
                             f.extract_meta_bfh,
                             sleep.min = sleep.min,
                             sleep.max = sleep.max)
    
    dt.ascending <- data.table::rbindlist(list.ascending)


    ## Combine Results
    dt.return <- rbind(dt.descending,
                       dt.ascending)
    
    
    dt.return <- unique(dt.return)
    dt.return <- dt.return[!is.na(datum)]

    return(dt.return)


    
}






f.extract_meta_bfh <- function(url,
                               sleep.min = 0.5,
                               sleep.max = 2,
                               verbose = TRUE){

    tryCatch({
        
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

    },
    error = function(cond) {
        return(NA)}
    )

}






