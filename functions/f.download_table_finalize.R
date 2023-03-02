




f.download_table_finalize <- function(x){


        

    ## Clean Results
    
    dt.return$release <- as.Date(dt.return$release, format = "%d.%m.%Y")

    dt.return$datum <- as.Date(dt.return$datum, format = "%d.%m.%Y")

    dt.return$bfh_id <- gsub(".*\\/(STR.*)\\/", "\\1", dt.return$url_html)



    }
