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

    spruchkoerper_az <- gsub("([IVX])+.+", "\\1", dt$spruchkoerper_db)
    

    doc_id <- paste0("BFH_",
                     dt$datum,
                     

}




    
## DEBUGGING CODE

## tar_load(dt.download.manifest.final)
## tar_load(dt.decisionpage)
