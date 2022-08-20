

dataFrameGenerator <- function(df, filter_array) 
                      {
                        pieDf <- df %>% 
                          dplyr::filter(tfName %in% filter_array) %>%
                          dplyr::select(motifDescription) %>% 
                          dplyr::count(motifDescription) %>% 
                          dplyr::rename(Group = motifDescription, value = n)  
                        
                        return(pieDf)
                        }

