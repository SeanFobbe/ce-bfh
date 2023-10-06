#' f.merge
#'
#' Merge download manifest and decisionpage data.
#'
#' @param dt.download.manifest.final Data.table. Final download manifest created from pipeline.
#' @param dt.decisionpage Data.table. Extracted data from individual decisionpages




f.merge <- function(dt.download.manifest.final,
                    dt.decisionpage){


    dt <- merge(dt.download.manifest.final,
                dt.decisionpage,
                by = "bfh_id")

    dt$spruchkoerper_az <- gsub("([IVXGrS]+).+", "\\1", dt$az)

    registerzeichen <- gsub("[IVXGrS]+ ([A-Za-z-]+).*", "\\1", dt$az)
    registerzeichen <- gsub("GrS.*", "GrS", registerzeichen)
    dt$registerzeichen <- gsub("ER-S.*", "ER-S", registerzeichen)    
    
    dt$eingangsnummer <- as.integer(gsub("[IVXGrS]+ *[A-Za-z-]+ *([0-9]+)[-,/].*",
                                         "\\1",
                                         dt$az))



    
    dt$az[is.na(dt$eingangsnummer)]

    summary(as.integer(dt$eingangsnummer))
    

    pkh <- grepl("PKH", dt$az, ignore.case = TRUE)
    adv <- grepl("AdV", dt$az, ignore.case = TRUE)
    

    
    doc_id <- paste0("BFH_",
                     dt$datum,
                     

}




    
## DEBUGGING CODE

## tar_load(dt.download.manifest.final)
## tar_load(dt.decisionpage)
