pieplotSetting <- function(df_tmp= df, title= "TFs and motifs", unit = 1.5, legend_text_size = 15, legend_title_size = 20, plot_title_size = 30, vector=c("#999999", "#E69F00", "#56B4E9"), palete = FALSE)
        
                  {
  
                    ggobject <- ggplot(df_tmp, aes(x="", y=prop, fill=Group)) +
                      geom_bar(stat="identity", width=1, color="white") +
                      coord_polar("y", start=0) +
                      theme_void() +
                      ggtitle(title) +
                      theme(legend.key.size = unit(unit, 'cm'), legend.text = element_text(size=legend_text_size), legend.title = element_text(size= legend_title_size), plot.title = element_text(hjust = 0.5,  size = plot_title_size))
                    
                    if(palete) {
                      ggobject <- ggobject +	
                        scale_fill_brewer(palette="Dark2") 
                    }
                    
                    else {
                      ggobject <- ggobject +	
                        scale_fill_manual(values=vector)
                    }
                    
                    
                    return(ggobject)
                    
                  }