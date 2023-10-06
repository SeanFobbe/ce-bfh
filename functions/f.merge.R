#' f.merge
#'
#' Merge download manifest and decisionpage data.
#'
#' @param dt.download.manifest.final Data.table. Final download manifest created from pipeline.
#' @param dt.decisionpage Data.table. Extracted data from individual decisionpages




f.merge <- function(dt.download.manifest.final,
                    dt.decisionpage){


    ## Merge
    dt <- merge(dt.download.manifest.final,
                dt.decisionpage,
                by = "bfh_id")

    ## Create var "spruchkoerper_az"
    dt$spruchkoerper_az <- gsub("([IVXGrS]+).+", "\\1", dt$az)

    ## Create var "registerzeichen"
    registerzeichen <- gsub("[IVXGrS]+ ([A-Za-z-]+).*", "\\1", dt$az)
    registerzeichen <- gsub("GrS.*", "GrS", registerzeichen)
    dt$registerzeichen <- gsub("ER-S.*", "ER-S", registerzeichen)    

    ## Create var "eingangsnummer"
    dt$eingangsnummer <- as.integer(gsub("[IVXGrS]+ *[A-Za-z-]+ *([0-9]+)[-,/].*",
                                         "\\1",
                                         dt$az))

    ## Create var "eingangsjahr_az"
    dt$eingangsjahr_az <- as.integer(gsub(".*/([0-9]+).*",
                                         "\\1",
                                         dt$az))

    ## Create var "eingangsjahr_iso"
    dt$eingangsjahr_iso <- f.year.iso(dt$eingangsjahr_az)

    ## Create var "pkh"
    dt$pkh <- grepl("PKH", dt$az, ignore.case = TRUE)

    ## Create var "adv"
    dt$adv <- grepl("AdV", dt$az, ignore.case = TRUE)

    
    ## Create var "doc_id"
    dt$doc_id <- paste0("BFH_",
                        dt$datum,
                        "_",
                        dt$spruchkoerper_az,
                        "_",
                        dt$registerzeichen,
                        "_",
                        dt$eingangsnummer,
                        "_",
                        dt$eingangsjahr_az,
                        "_",
                        dt$bfh_id)


    ## Order by Date
    dt <- dt[order(datum)]

    
uniqueN(dt$bfh_id)
    

    
uniqueN(dt$doc_id)

    nrow(dt)

    dt$az[]

sum(duplicated(dt$doc_id))
    
str(dt[az == "IX S 17/21"])
    

grep(

    
}




    
## DEBUGGING CODE

## tar_load(dt.download.manifest.final)
## tar_load(dt.decisionpage)
