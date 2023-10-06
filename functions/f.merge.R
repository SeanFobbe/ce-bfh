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

    ## Create var "entscheidungsjahr"
    dt$entscheidungsjahr <- year(dt$datum)

    ## Create var "veroeffentlichungsjahr"
    dt$veroeffentlichungsjahr <- year(dt$veroeffentlichung)

    ## Create var "pkh"
    dt$pkh <- grepl("PKH", dt$az, ignore.case = TRUE)

    ## Create var "adv"
    dt$adv <- grepl("AdV", dt$az, ignore.case = TRUE)

    
    ## Create var "doc_id"
    dt$doc_id <- paste0("BFH_",
                        slg,
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


    ## Order by Date
    dt.final <- dt[order(datum)]


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

    test_that("var spruchkoerper_az contains only expected values.", {
        expect_length(setdiff(dt.final$spruchkoerper_az, c(as.character(as.roman(1:12)), "GrS")),
                      0)
    })

    test_that("var registerzeichen contains only expected values.", {
        expect_length(setdiff(dt.final$registerzeichen,
                              c("S", "R", "B", "E", "ER-S", "K", "GrS")),
                      0)
    })

    test_that("var eingangsjahr_iso contains only expected values.", {
        expect_true(all(dt.final$eingangsjahr_iso >= 2000))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date()))) 
    })

    test_that("var eingangsjahr_az contains only expected values.", {
        expect_true(all(dt.final$eingangsjahr_az >= 0))
        expect_true(all(dt.final$eingangsjahr_az <= as.integer(format(Sys.Date(), "%y"))))
    })
    
    test_that("var entscheidungsjahr contains only expected values.", {
        expect_true(all(dt.final$entscheidungsjahr >= 2010))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date())))    
    })

    test_that("var veroeffentlichungsjahr contains only expected values.", {
        expect_true(all(dt.final$entscheidungsjahr >= 2010))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date())))    
    })
    
    test_that("var eingangsnummer contains only expected values.", {
        expect_true(all(dt.final$eingangsnummer > 0))
        expect_true(all(dt.final$eingangsnummer < 1e5))
    })

    test_that("var pkh contains only expected values.", {
      expect_type(dt.final$adv, "logical")   
    })
    
    test_that("var adv contains only expected values.", {
      expect_type(dt.final$adv, "logical")   
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
