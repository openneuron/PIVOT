
# The functions swiped from STRINGdb, using the original stringdb package cause crash because of igraph incompatibility 

downloadAbsentFile <- function(urlStr, oD = tempdir()){
    
    fileName = tail(strsplit(urlStr, "/")[[1]], 1)
    temp <- paste(oD,"/", fileName, sep="")
    if(! file.exists(temp) || file.info(temp)$size==0) download.file(urlStr,temp)
    if(file.info(temp)$size==0) {
        unlink(temp)
        temp=NULL
        cat(paste("ERROR: failed to download ", fileName,".\nPlease check your internet connection and/or try again. " , 
                  "\nThen, if you still display this error message please contact us.",sep=""))
    }  
    return(temp)
}

stringdb_get_graph = function(species, version, score_threshold, backgroundV = NULL){
    '
    Description:
    Downloads and returns the STRING network (the network is set also in the graph variable of the STRING_db object).
    
    It makes use of the variables:
    "backgroundV"         vector containing STRING identifiers to be used as background 
    (i.e. the STRING network loaded will contain only the proteins that are present also in this vector)
    "score_threshold"     STRING combined score threshold (the network loaded contains only interactions having a combined score greater than this threshold)
    Author(s):
    Andrea Franceschini
    '
    
    
    temp = downloadAbsentFile(paste("http://string.uzh.ch/permanent/string/", version, "/protein_links/",
                                    species, "__protein_links.tsv.gz", sep=""))
    PPI <- read.table(temp, sep = " ", header=TRUE, stringsAsFactors=FALSE, fill = TRUE)
    
    PPIselected = PPI
    if(length(score_threshold)!=0) PPIselected <- PPI[PPI$combined_score >= score_threshold,]
    if(!is.null(backgroundV)){
        PPIselected <- PPIselected[PPIselected$protein1 %in% backgroundV,]
        PPIselected <- PPIselected[PPIselected$protein2 %in% backgroundV,]
    }
    
    myg = igraph::graph.data.frame(PPIselected,FALSE)
    return(myg)
}

