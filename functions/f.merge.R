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


    ## Assign temporary doc_id (will be changed by f.final) otherwise lingstats will not work
    dt$doc_id <- dt$bfh_id
    
    ## Order by Date
    dt.final <- dt[order(datum)]


    ## Tests
    test_that("Class is correct.", {
        expect_s3_class(dt.final, "data.table")
    })
   
    test_that("BFH IDs are unique.", {
        expect_equal(sum(duplicated(dt.final$bf_id)),  0)
    })

    test_that("Dates are in ISO format.", {
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$veroeffentlichung)))
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$datum)))
    })



    return(dt.final)
    
}




    
## DEBUGGING CODE

## tar_load(dt.download.manifest.final)
## tar_load(dt.decisionpage)
