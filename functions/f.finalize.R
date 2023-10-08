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


    

    return(dt.final)
    
    
}



## DEBUGGING CODE

library(data.table)
library(testthat)
tar_load(dt.intermediate)
vars.additional  <- tar_read(vars_additional)

variables.codebook <- fread("data/CE-BFH_Variables.csv")
varnames  <-  variables.codebook$varname
