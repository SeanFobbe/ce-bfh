#' f.citation_extraction
#'
#' Extract citations to decisions of the German Federal Court of Finance (BFH) and convert to igraph object.
#'
#' @param dt.final Data.table. The final data set.
#' @return Igraph object. All internal citations as a graph object.


# Draws and expands Coupette, Juristische Netzwerkforschung (Mohr Siebeck 2019), 241-244


#' TODO
#'
#' - ADD Letter Senates (e.g. GrS)


#' Example citation blocks BFH
#' 
#' (Senatsurteile vom 13.01.2015 - VII R 35/12, BFHE 248, 287, Rz 26 und 28 und vom 10.11.2015 - VII R 40/14, Rz 11; Senatsbeschlüsse vom 31.01.2019 - VII B 115/18, Rz 10 und vom 31.01.2019 - VII B 147/18, Rz 14). 



f.citation_extraction_bfh <- function(dt.final,
                                      az.brd){


    ## Create full Aktenzeichen search REGEX, example: "VII S 28/08"
    regex.az.number <- paste0("[IXV]{1,4}", # Senatsnummer
                              "\\s*",
                              "(AR|B|E|K|PKH|R|S|ER-S)", # Registerzeichen
                              "\\s*",
                              "\\d{1,4}/", # Eingangsnummer
                              "\\d{2}") # Jahr

    
    ## Extract BFH citations to Aktenzeichen targets
    target.az.number <- stringi::stri_extract_all(dt.final$text,
                                                  regex = regex.az.number)
    
    
    ## Create GrS Aktenzeichen search REGEX, example: "GrS 4/78"
    regex.az.letter <- paste0("GrS", # Registerzeichen
                              "\\s*",
                              "\\d{1,4}/", # Eingangsnummer
                              "\\d{2}") # Jahr

    
    ## Extract BFH citations to Aktenzeichen targets
    target.az.letter <- stringi::stri_extract_all(dt.final$text,
                                                  regex = regex.az.letter)

    

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
    source <- dt.final$aktenzeichen
       
    
    ## [Number Senates] Combine source Aktenzeichen and target Aktenzeichen
    bind <- mapply(cbind, source, target.az.number)
    bind <- lapply(bind, as.data.table)
    dt.az.number <- rbindlist(bind)
    setnames(dt.az.number, new = c("source", "target"))

    ## [Letter Senates] Combine source Aktenzeichen and target Aktenzeichen
    bind <- mapply(cbind, source, target.az.letter)
    bind <- lapply(bind, as.data.table)
    dt.az.letter <- rbindlist(bind)
    setnames(dt.az.letter, new = c("source", "target"))

    ## Combine source Aktenzeichen and target BFHE
    bind <- mapply(cbind, source, target.bfhe)
    bind <- lapply(bind, as.data.table)
    dt.bfhe <- rbindlist(bind)
    setnames(dt.bfhe, new = c("source", "target"))

   
    ## Clean BFHE hooks
    dt.bfhe$target <-  gsub(";", "BFHE", dt.bfhe$target)
    
    ## Combine Tables
    dt <- rbind(dt.az.number, dt.az.letter, dt.bfhe)    

    ## Remove non-citations
    dt <- dt[!is.na(target)]
    
    ## Clean Underscores
    dt$source <- gsub(" ", " ", dt$source)
    dt$target <- gsub(" ", " ", dt$target)
    
    ## Clean whitespace
    dt$source <- gsub("\\s+", " ", dt$source)
    dt$target <- gsub("\\s+", " ", dt$target)

    dt$source <- trimws(dt$source)
    dt$target <- trimws(dt$target)

    ## Add whitespace if missing; example "1 BvL100/58"
    dt$source <- gsub("([A-Z])(\\d)", "\\1 \\2", dt$source)
    dt$target <- gsub("([A-Z])(\\d)", "\\1 \\2", dt$target)


    ## Remove self-citations    
    dt <- dt[!(dt$source == dt$target)]


    ## Select Metadata    
    dt.meta <- dt.final[, !"text"]
    dt.meta <- dt.meta[!duplicated(dt.meta$aktenzeichen)]

    ## Select vertices without metadata
    edge.vertices <- unique(c(dt$source, dt$target))
    meta.vertices <- dt.meta$aktenzeichen
    nometa.vertices <- setdiff(edge.vertices, meta.vertices)
    dt.nometa <- data.table(aktenzeichen = nometa.vertices)

    ## Create table with all vertices with and without metadata
    dt.fullmeta <- rbind(dt.meta, dt.nometa, fill = TRUE)
    
    ## Create Graph Object
    g  <- igraph::graph_from_data_frame(dt,
                                        directed = TRUE,
                                        vertices = dt.fullmeta)

    
    ## Convert Parallel Edges to Weights
    igraph::E(g)$weight <- 1
    g <- igraph::simplify(g, edge.attr.comb = list(weight = "sum"))


    ## Add BFHE attribute
    g <- igraph::set_vertex_attr(graph = g,
                                 name = "bfhe",
                                 value = grepl("BFHE",
                                               igraph::vertex_attr(g, "name"))
                                 )
    


    ## Select vertex names
    g.names <- igraph::vertex_attr(g, "name")
    
    ## Extract Senate   
    g.senat <- stringi::stri_extract_all(g.names, regex = "^[IXV]{1,4}")
    g.senat <- unlist(g.senat)
    stopifnot(length(g.names) == length(g.senat))

    ## Extract Registerzeichen
    g.regz <- stringi::stri_match_first(g.names, regex = " (AR|B|E|GrS|K|PKH|R|S|ER-S) *[0-9]+")
    g.regz <- g.regz[,2]

    ## Code Verfahrensart
    g.verfahrensart <- f.var_verfahrensart(g.regz,
                                           az.brd = az.brd,
                                           gericht = "BFH")
    
    
    ## Extract BFHE
    g.bfhe <- grepl("BFHE", g.names, ignore.case = TRUE)

    ## Extract band
    g.band <- stringi::stri_match_all(g.names, regex = "BFHE ([0-9]+)\\s*,")
    g.band <- do.call(rbind, g.band)
    g.band <- as.integer(g.band[,2])

    ## Add Vertex Attributes
    g <- igraph::set_vertex_attr(g, "registerzeichen", index = igraph::V(g), g.regz)
    g <- igraph::set_vertex_attr(g, "verfahrensart", index = igraph::V(g), g.verfahrensart)
    g <- igraph::set_vertex_attr(g, "spruchkoerper_az", index = igraph::V(g), g.senat)
    g <- igraph::set_vertex_attr(g, "bfhe-alternative", index = igraph::V(g), g.bfhe)
    g <- igraph::set_vertex_attr(g, "band", index = igraph::V(g), g.band)
    

    return(g)
    

}



## DEBUGGING Code

## library(stringi)
## library(data.table)                     
## library(igraph)
## tar_load(dt.final)  




## Multiple AKtenzeichen
    ## test <- lapply(g.regz, length)
    ## test <- unlist(test)
    ## index <- which(test > 2)
    ## g.regz[index]

    ## g.names[10685]
    
