library(Biostrings)

findingMotifPresence <- function(motif, blastOutput, threshold) 
                        {
  
                          presence <- length(matchPattern(AAString(motif), AAString(blastOutput), fixed=TRUE, with.indels = TRUE, max.mismatch = threshold, min.mismatch=0))
                          return(presence)
                        
                          }
