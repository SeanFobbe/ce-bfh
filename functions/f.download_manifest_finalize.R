#' f.download_manifest_finalize
#'
#' Cleans up and tests the download manifest for the Bundesfinanzhof (BFH) database.
#'
#' @param x Data.table. A download manifest created by the f.download_manifest_make function.
#'
#' @return Data.table. The finalized download manifest.




f.download_manifest_finalize <- function(x){


    ## Clean Results
    
    dt.return$release <- as.Date(dt.return$release, format = "%d.%m.%Y")

    dt.return$datum <- as.Date(dt.return$datum, format = "%d.%m.%Y")

    dt.return$bfh_id <- gsub(".*\\/(STR.*)\\/", "\\1", dt.return$url_html)



    ## Tests
    test_that("Class is correct.", {
        expect_s3_class(dt.final, "data.table")
    })
   
    test_that("BFH IDs are unique.", {
        expect_equal(uniqueN(bfh_id),  x[,.N])
    })

    test_that("Dates are in ISO format.", {
        expect_equal(uniqueN(bfh_id),  x[,.N])
    })

    

    return(dt.return)


}


## DEBUGGING CODE

# x <- tar_read(dt.download.manifest.raw)
