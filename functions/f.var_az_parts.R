#' f.var_az_parts
#'
#' Extract docket number components from Bundesfinanzhof (BFH) docket number (Aktenzeichen).
#'
#' @param dt.intermediate Data.Table. The merger of download table and decision page content.
#'
#' @return Data.Table. A table consisting of the parts of the docket numbers.
#'
#' Note: if multiple docket numbers are given, this extracts only the information from the first docket number.



f.var_az_parts <- function(dt.intermediate){


    ## Create var "spruchkoerper_az"
    spruchkoerper_az  <-  gsub("([IVXGrS]+).+", "\\1", dt.intermediate$aktenzeichen)

    ## Create var "registerzeichen"
    registerzeichen <- gsub("[IVXGrS]+ ([A-Za-z-]+).*", "\\1", dt.intermediate$aktenzeichen)
    registerzeichen <- gsub("GrS.*", "GrS", registerzeichen)
    registerzeichen <- gsub("ER-S.*", "ER-S", registerzeichen)    

    ## Create var "eingangsnummer"
    eingangsnummer <- as.integer(gsub("[IVXGrS]+ *[A-Za-z-]+ *([0-9]+)[-,/].*",
                                         "\\1",
                                         dt.intermediate$aktenzeichen))

    ## Create var "eingangsjahr_az"
    eingangsjahr_az <- as.integer(gsub(".*/([0-9]+).*",
                                         "\\1",
                                         dt.intermediate$aktenzeichen))

    ## Create var "eingangsjahr_iso"
    eingangsjahr_iso <- f.year.iso(eingangsjahr_az)


    ## Return Value
    dt.final <- data.table(spruchkoerper_az,
                           registerzeichen,
                           eingangsnummer,
                           eingangsjahr_az,
                           eingangsjahr_iso)

    
    ## Tests
    test_that("var spruchkoerper_az contains only expected values.", {
        expect_length(setdiff(dt.final$spruchkoerper_az, c(as.character(as.roman(1:12)), "GrS")),
                      0)
    })

    test_that("var registerzeichen contains only expected values.", {
        expect_length(setdiff(dt.final$registerzeichen,
                              c("S", "R", "B", "E", "ER-S", "K", "GrS")),
                      0)
    })

    test_that("var eingangsnummer contains only expected values.", {
        expect_true(all(dt.final$eingangsnummer > 0))
        expect_true(all(dt.final$eingangsnummer < 1e5))
    })
   
    
    test_that("var eingangsjahr_az contains only expected values.", {
        expect_true(all(dt.final$eingangsjahr_az >= 0))
        expect_true(all(dt.final$eingangsjahr_az <= as.integer(format(Sys.Date(), "%y"))))
    })
    

     test_that("var eingangsjahr_iso contains only expected values.", {
        expect_true(all(dt.final$eingangsjahr_iso >= 2000))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date()))) 
    })

    return(dt.final)
    

}


## DEBUGGING CODE

##tar_load(dt.intermediate)
