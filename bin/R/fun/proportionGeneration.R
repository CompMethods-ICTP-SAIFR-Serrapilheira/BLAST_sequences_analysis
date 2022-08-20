proportionGeneration <- function(df) 
                          {
                          dfProportion <- df %>% 
                            arrange(desc(Group)) %>%
                            mutate(prop = value / sum(df$value) *100) %>%
                            mutate(Group= paste(Group, "\n(", as.character(round(prop,2)), ")%", sep = ""))
 
                          return(dfProportion) 
                          }