#' f.finalize
#'
#' Finalize the data set



f.finalize <- function(dt.intermediate){


        ## Create var "doc_id"
    dt$doc_id <- paste0("BFH_",
                        dt$slg,
                        "_",
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


    ## Tests
    test_that("Class is correct.", {
        expect_s3_class(dt.final, "data.table")
    })
   
    test_that("BFH IDs are unique.", {
        expect_equal(sum(duplicated(dt.final$bf_id)),  0)
    })

    test_that("Doc IDs are unique.", {
        expect_equal(sum(duplicated(dt.final$doc_id)),  0)
    })
    
    
    }
