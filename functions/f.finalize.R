#' f.finalize
#'
#' Finalize the data set



f.finalize <- function(dt.intermediate,
                       vars.additional,
                       varnames){


    ## Unit Test
    test_that("Arguments have correct classes", {
        expect_s3_class(x, "data.table")
        expect_s3_class(vars.additional, "data.table")
        expect_type(varnames, "character")
    })

    ## Bind additional vars
    dt.final <- cbind(dt.intermediate,
                      vars.additional)

    ## Create var "entscheidungsjahr"
    dt$entscheidungsjahr <- year(dt$datum)

    ## Create var "veroeffentlichungsjahr"
    dt$veroeffentlichungsjahr <- year(dt$veroeffentlichung)

    ## Create var "pkh"
    dt$pkh <- grepl("PKH", dt$aktenzeichen, ignore.case = TRUE)

    ## Create var "adv"
    dt$adv <- grepl("AdV", dt$aktenzeichen, ignore.case = TRUE)

    
    ## Create var "gericht"
    dt.final$gericht <- "BFH"
    
    ## Create var "doc_id"
    dt.final$doc_id <- paste0("BFH_",
                              dt$bfhe,
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

setdiff(varnames, names(dt.final))
    

    ## Unit Test: Check variables and set column order
    
    varnames <- gsub("\\\\", "", varnames) # Remove LaTeX escape characters
    data.table::setcolorder(dt.final, varnames)


    
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

    test_that("Dates are in ISO format.", {
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$veroeffentlichung)))
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$datum)))
    })
    

    test_that("var entscheidungsjahr contains only expected values.", {
        expect_true(all(dt.final$entscheidungsjahr >= 2010))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date())))    
    })

    test_that("var veroeffentlichungsjahr contains only expected values.", {
        expect_true(all(dt.final$entscheidungsjahr >= 2010))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date())))    
    })
    
    test_that("var pkh contains only expected values.", {
      expect_type(dt.final$adv, "logical")   
    })
    
    test_that("var adv contains only expected values.", {
      expect_type(dt.final$adv, "logical")   
    })
    

    

    return(dt.final)
    
    
}



## DEBUGGING CODE

library(data.table)
library(testthat)
tar_load(dt.intermediate)
vars.additional  <- tar_read(vars_additional)

variables.codebook <- fread("data/CE-BFH_Variables.csv")
varnames  <-  variables.codebook$varname
