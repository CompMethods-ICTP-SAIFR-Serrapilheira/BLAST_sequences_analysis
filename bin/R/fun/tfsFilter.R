tfsFilter <- function(df = motifPresence, n = 1)
              { 
              
                if(n != "NA") {
                  dfFilter <- df %>% 
                                dplyr::filter(motifPresence100 == n)
                  } else {
                          dfFilter <- df %>% 
                                        dplyr::filter(is.na(motifPresence100))
                          }              

                  dfFilter <- dfFilter %>%
                                dplyr::select(tfName) %>%
                                unique() %>%
                                pull()
              return(dfFilter)
}