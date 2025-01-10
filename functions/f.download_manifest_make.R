#' f.download_manifest_make
#'
#' Create raw download manifest for Bundesfinanzhof (BFH) decisions.
#'
#' @param sleep.min Numeric. Minimum time to sleep between requests.
#' @param sleep.max Numeric. Maximum time to sleep between requests.
#' @param verbose Logical. Whether to print a message after each request. Defaults to FALSE.
#' @param debug.toggle Logical. Whether to collect only a random subset of decisions.
#' @param debug.pages Integer. The number of pages to collect. Each page contains 10 decisions.


#' Note: offsets of 10000 and more yield errors. Need to reverse list direction to acquire additional decisions.



f.download_manifest_make <- function(sleep.min = 0.5,
                                     sleep.max = 2,
                                     verbose = FALSE,
                                     debug.toggle = FALSE,
                                     debug.pages = 20){



    ## Calculate Offsets
    
    page.url <- f.linkextract("https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/")
    page.url <- grep("search-form", page.url, value = TRUE)
    
    page.raw <- gsub(".*Bpage%5D=([0-9]+)#search-form.*", "\\1", page.url)        
    page.max <- max(as.integer(page.raw))
    page.all <- seq(page.max)
        

    if(debug.toggle == TRUE){

        page.all <- sort(sample(page.all, debug.pages))
        
    }


    ## Create URLs
    
    url <- paste0( "https://www.bundesfinanzhof.de/de/entscheidungen/entscheidungen-online/?tx_solr%5Bpage%5D=",
                  page.all,
                  "#search-form")


    ## Run Extraction
    list <- lapply(url,
                   f.extract_meta_bfh,
                   verbose = verbose,
                   sleep.min = sleep.min,
                   sleep.max = sleep.max)
    
    dt <- data.table::rbindlist(list)


    dt <- unique(dt)
    dt.return <- dt[!is.na(datum)]

    return(dt.return)


    
}






f.extract_meta_bfh <- function(url,
                               sleep.min = 0.5,
                               sleep.max = 2,
                               verbose = TRUE){

    tryCatch({
        
        html <- rvest::read_html(url)
        
        veroeffentlichung <- rvest::html_nodes(html, "[data-label='Veröffentlichung am']")
        veroeffentlichung <- rvest::html_text(veroeffentlichung, trim = TRUE)

        bfhe <- rvest::html_nodes(html, "[data-label='V/NV']")
        bfhe <- rvest::html_text(bfhe, trim = TRUE)

        spruchkoerper_db <- rvest::html_nodes(html, "[data-label='Senat']")
        spruchkoerper_db <- rvest::html_text(spruchkoerper_db, trim = TRUE)

        datum <- rvest::html_nodes(html, "[data-label='Entscheidung vom']")
        datum <- rvest::html_text(datum, trim = TRUE)


        aktenzeichen <- rvest::html_nodes(html, "[data-label='Aktenzeichen']")
        aktenzeichen <- rvest::html_text(aktenzeichen, trim = TRUE)

        titel <- rvest::html_nodes(html, "[data-label='Titel']")
        titel <- rvest::html_text(titel, trim = TRUE)

        url_html <- rvest::html_nodes(html, "[data-label='Titel']")
        url_html <- rvest::html_nodes(url_html, "a")
        url_html <- rvest::html_attr(url_html, name = "href")
        url_html <- paste0("https://www.bundesfinanzhof.de", url_html)
        

        dt.return <- data.table::data.table(veroeffentlichung = veroeffentlichung,
                                            bfhe = bfhe,
                                            spruchkoerper_db = spruchkoerper_db,
                                            datum = datum,
                                            aktenzeichen = aktenzeichen,
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




## DEBUGGING


# test <- f.download_manifest_make(verbose = TRUE)
