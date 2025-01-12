#' f.citation_extraction
#'
#' Extract citations to decisions of the German Federal Court of Finance (BFH) and convert to igraph object.
#'
#' @param dt.final Data.table. The final data set.
#' @return Igraph object. All internal citations as a graph object.


# Draws and expands Coupette, Juristische Netzwerkforschung (Mohr Siebeck 2019), 241-244


#' Example citation blocks BFH
#' 
#' - "BVerfGE 79, 240 <243>; 149, 1 <10 Rn. 21>; 157, 223 <250 Rn. 70>),"
#' - "(vgl. BVerfGE 152, 345 <371 Rn. 65 f.> m.w.N.)"



f.citation_extraction_bfh <- function(dt.final){


    ## Create full Aktenzeichen search REGEX, example: "VII S 28/08"
    regex.az <- paste0("[IXV]{1,4}", # Senatsnummer
                       "\\s*",
                       "(AR|B|E|GrS|K|PKH|R|S)", # Registerzeichen
                       "\\s*",
                       "\\d{1,4}/", # Eingangsnummer
                       "\\d{2}") # Jahr

    
    ## Extract BVerfG citations to Aktenzeichen targets
    target.az <- stringi::stri_extract_all(dt.final$text,
                                           regex = regex.az)
    

    ## Extract BFHE citation blocks
    regex.bfhe.blocks <- "BFHE[\\s\\d\\[\\];,\\.<>Rnfu-]+"
    
    target.bfhe.blocks <- stringi::stri_extract_all(dt.final$text,
                                                            regex = regex.bfhe.blocks,
                                                            case_insensitive = TRUE)
    
    target.bfhe.blocks <- lapply(target.bfhe.blocks, paste0, collapse = " ")
    target.bfhe.blocks <- lapply(target.bfhe.blocks, # Fix case typos
                                    gsub,
                                    pattern = "BFH",
                                    replacement = "BFH",
                                    ignore.case = TRUE)

    ## Extract individual Bfhe citations from blocks
    regex.bfhe.cite <- paste0("(BFHE|;)\\s*", # hooks
                                 "\\d{1,3},\\s*", # Volume
                                 "\\d{1,3}") # Page


    target.bfhe <- stringi::stri_extract_all(target.bfhe.blocks,
                                                regex = regex.bfhe.cite)

    
    ## Define source Aktenzeichen
    source <- ifelse(is.na(dt.final$band),
                     dt.final$aktenzeichen,
                     paste0("BFHE ", dt.final$band, ", ", dt.final$seite))

       
    
    ## Combine source Aktenzeichen and target Aktenzeichen
    bind <- mapply(cbind, source, target.az)
    bind <- lapply(bind, as.data.table)
    dt.az <- rbindlist(bind)
    setnames(dt.az, new = c("source", "target"))


    ## Combine source Aktenzeichen and target BFHE
    bind <- mapply(cbind, source, target.bfhe)
    bind <- lapply(bind, as.data.table)
    dt.bfhe <- rbindlist(bind)
    setnames(dt.bfhe, new = c("source", "target"))

    ## Remove non-citations
    dt.az <- dt.az[!is.na(target)]
    dt.bfhe <- dt.bfhe[!is.na(target)]

    ## Clean BFHE hooks
    dt.bfhe$target <-  gsub(";", "BFHE", dt.bfhe$target)
    
    ## Combine Tables
    dt <- rbind(dt.az, dt.bfhe)    
    
    ## Clean whitespace
    dt$source <- gsub("\\s+", " ", dt$source)
    dt$target <- gsub("\\s+", " ", dt$target)

    dt$source <- trimws(dt$source)
    dt$target <- trimws(dt$target)

    ## Add whitespace if missing; example "1 BvL100/58"
    dt$source <- gsub("([A-Z])(\\d)", "\\1 \\2", dt$source)
    dt$target <- gsub("([A-Z])(\\d)", "\\1 \\2", dt$target)

    dt$source <- gsub("(\\d)B", "\\1 B", dt$source)
    dt$target <- gsub("(\\d)B", "\\1 B", dt$target)
    

    ## Remove self-citations    
    dt <- dt[!(dt$source == dt$target)]
    

    ## Create Graph Object
    g  <- igraph::graph_from_data_frame(dt,
                                        directed = TRUE)

    
    ## Convert Parallel Edges to Weights
    igraph::E(g)$weight <- 1
    g <- igraph::simplify(g, edge.attr.comb = list(weight = "sum"))



    ## Extract vertex names
    g.names <- igraph::vertex_attr(g, "name")

    ## Create limited metadata table
    dt.final$graphkey <-  ifelse(is.na(dt.final$band),
                                 dt.final$aktenzeichen,
                                 paste0("BFHE ", dt.final$band, ", ", dt.final$seite))
    
    
    dt.meta <- dt.final[,.(graphkey,
                           entscheidungsjahr,
                           spruchkoerper_typ,
                           spruchkoerper_az,
                           registerzeichen,
                           verfahrensart,
                           eingangsnummer,
                           eingangsjahr_az,
                           eingangsjahr_iso,
                           band,
                           aktenzeichen,
                           aktenzeichen_alle,
                           praesi,
                           v_praesi)]


    dt.meta <-  unique(dt.meta, by = "graphkey")

    
    ## Match metadata to graph
    match <- match(g.names, dt.meta$graphkey)
    dt.graphmeta <- dt.meta[match]

    
    ## Set Vertex Attributes (all)
    varnames <- names(dt.graphmeta)
    
    for(i in varnames){
    g <- igraph::set_vertex_attr(graph = g,
                                 name = i,
                                 value = unname(unlist(dt.graphmeta[, ..i])))

    }


    
    ## Add BFHE attribute
    g <- igraph::set_vertex_attr(graph = g,
                                 name = "bverfge",
                                 value = grepl("BFHE",
                                               igraph::vertex_attr(g, "name"))
                                 )
    


    return(g)
    

}



## DEBUGGING Code

## library(stringi)
## library(data.table)                     
## library(igraph)
## tar_load(dt.final)  

