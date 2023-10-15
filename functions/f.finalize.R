#' f.finalize
#'
#' Finalize the data set
#'
#' @param dt.intermediate Data.table. The intermediate result.
#' @param vars.additional Data.table. Additional variables calculated from the intermediate result.
#' @param varnames String. The variable names listed in the Codebook.
#'
#' @return Data.table. The finalized data set. 



f.finalize <- function(dt.intermediate,
                       vars.additional,
                       varnames){


    ## Unit Test
    test_that("Arguments have correct classes", {
        expect_s3_class(dt.intermediate, "data.table")
        expect_s3_class(vars.additional, "data.table")
        expect_type(varnames, "character")
    })

    ## Bind additional vars
    dt.final <- cbind(dt.intermediate,
                      vars.additional)

    ## Create var "entscheidungsjahr"
    dt.final$entscheidungsjahr <- year(dt.final$datum)

    ## Create var "veroeffentlichungsjahr"
    dt.final$veroeffentlichungsjahr <- year(dt.final$veroeffentlichung)

    ## Create var "pkh"
    dt.final$pkh <- grepl("PKH", dt.final$aktenzeichen, ignore.case = TRUE)

    ## Create var "adv"
    dt.final$adv <- grepl("AdV", dt.final$aktenzeichen, ignore.case = TRUE)

    
    ## Create var "gericht"
    dt.final$gericht <- "BFH"
    
    ## Create var "doc_id"
    dt.final$doc_id <- paste0("BFH_",
                              dt.final$bfhe,
                              "_",
                              dt.final$datum,
                              "_",
                              dt.final$spruchkoerper_az,
                              "_",
                              dt.final$registerzeichen,
                              "_",
                              dt.final$eingangsnummer,
                              "_",
                              dt.final$eingangsjahr_az,
                              "_",
                              dt.final$bfh_id)


    ## Set NA zeichen to 0
    dt.final$zeichen <- ifelse(is.na(dt.final$zeichen), 0, dt.final$zeichen)

    ## Clean var "normen"

    normen <- gsub(" *, *\n +", "|", dt.final$normen)
    normen <- gsub("\n", "", normen)    
    normen <- gsub(" +", " ", normen)
    normen <- gsub("\\| ", "|", normen)
    dt.final$normen <- normen

    ## Test: Check variables
    
    varnames <- gsub("\\\\", "", varnames) # Remove LaTeX escape characters

    test_that("Variables in data set are identical to those documented in Codebook", {
        expect_setequal(names(dt.final), varnames)
    })

    ## Set Column Order
    data.table::setcolorder(dt.final, varnames)

    ## setdiff(varnames, names(dt.final))
    ## setdiff(names(dt.final),varnames)
    

    
    ## TESTING ##

    ## Classes
    test_that("Class is correct.", {
        expect_s3_class(dt.final, "data.table")
    })

    ## Uniqueness of IDs
    test_that("BFH IDs are unique.", {
        expect_equal(sum(duplicated(dt.final$bf_id)),  0)
    })

    test_that("Doc IDs are unique.", {
        expect_equal(sum(duplicated(dt.final$doc_id)),  0)
    })

    ## Dates
    test_that("Dates are in ISO format.", {
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$veroeffentlichung)))
        expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", dt.final$datum)))
    })


    ## URLs
    test_that("URLs are valid", {
        expect_true(all(grepl("https://.+\\.de", dt.final$url_pdf)))
        expect_true(all(grepl("https://.+\\.de", dt.final$url_html)))
    })
    
    ## Sets
    test_that("var spruchkoerper_az contains only expected values.", {
        expect_length(setdiff(dt.final$spruchkoerper_az,
                              c(as.character(as.roman(1:12)), "GrS")),
                      0)
    })

    test_that("var registerzeichen contains only expected values.", {
        expect_length(setdiff(dt.final$registerzeichen,
                              c("S", "R", "B", "E", "ER-S", "K", "GrS")),
                      0)
    })

    test_that("var bfhe contains only expected values.", {
        expect_length(setdiff(dt.final$bfhe,
                              c("V", "NV")),
                      0)
    })
    

    ## Linguistic Variables
    test_that("var zeichen contains only expected values.", {
        expect_true(all(dt.final$zeichen >= 0))
        expect_true(all(dt.final$zeichen < 1e6))   
    })

    test_that("var tokens contains only expected values.", {
        expect_true(all(dt.final$tokens >= 0))
        expect_true(all(dt.final$tokens < 1e5))   
    })

    test_that("var typen contains only expected values.", {
        expect_true(all(dt.final$typen >= 0))
        expect_true(all(dt.final$typen < 1e4))   
    })
    
    test_that("var saetze contains only expected values.", {
        expect_true(all(dt.final$saetze >= 0))
        expect_true(all(dt.final$saetze < 1e3))   
    })

    ## Years
    test_that("var entscheidungsjahr contains only expected values.", {
        expect_true(all(dt.final$entscheidungsjahr >= 2010))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date())))    
    })

    test_that("var veroeffentlichungsjahr contains only expected values.", {
        expect_true(all(dt.final$entscheidungsjahr >= 2010))
        expect_true(all(dt.final$entscheidungsjahr <= year(Sys.Date())))    
    })

    ## Logical
    test_that("var pkh contains only expected values.", {
      expect_type(dt.final$adv, "logical")   
    })
    
    test_that("var adv contains only expected values.", {
      expect_type(dt.final$adv, "logical")   
    })
    



    return(dt.final)
    
    
}



## DEBUGGING CODE

## library(data.table)
## library(testthat)
## tar_load(dt.intermediate)
## vars.additional  <- tar_read(vars_additional)

## variables.codebook <- fread("data/CE-BFH_Variables.csv")
## varnames  <-  variables.codebook$varname
