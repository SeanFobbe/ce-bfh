#' f.download_manifest_finalize
#'
#' Cleans up and tests the download manifest for the Bundesfinanzhof (BFH) database.
#'
#' @param x Data.table. A download manifest created by the f.download_manifest_make function.
#'
#' @return Data.table. The finalized download manifest.




f.download_manifest_finalize <- function(x){


    ## Copy table

    dt.final <- x

    ## Clean Dates
    dt.final$veroeffentlichung <- as.Date(dt.final$veroeffentlichung, format = "%d.%m.%Y")
    dt.final$datum <- as.Date(dt.final$datum, format = "%d.%m.%Y")

    ## Create BFH ID
    dt.final$bfh_id <- basename(dt.final$url_html)


 
    ## Tests
    test_that("Class is correct.", {
        expect_s3_class(dt.final, "data.table")
    })
   
    test_that("BFH IDs are unique.", {
        expect_equal(uniqueN(dt.final$bfh_id),  x[,.N])
    })

    test_that("Dates are in ISO format.", {
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$veroeffentlichung)))
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$datum)))
    })

    ## test_that("Docket numbers are schema-compliant.", {
    ##     test.docket <- grep("[IVX]{1,5} +[RSB] +[0-9]+/[0-9]+",
    ##                         dt.final$az,
    ##                         invert = TRUE,
    ##                         value = TRUE)
    ##     expect_length(test.docket, 0)
    ## })

    ## print(test.docket)

    ## if (length(regex.test) != 0){

    ##     warning("Folgende Aktenzeichen sind fehlerhaft:")
    ##     warning(test.docket)
    ## }

    
    

    return(dt.final)


}


## DEBUGGING CODE

# x <- tar_read(dt.download.manifest.raw)
