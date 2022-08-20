#!/usr/bin/env Rscript
#' ---
#' title: 03_pieplot.R
#'
#' author(s): Ericka Montserrat Hernandez Benitez
#'
#' date: August 18th, 2022
#'
#' version: 1
#'
#' description: 
#' The script performed the pie plot plotting from a tsv file.
#'
#' input:
#' ./data/processed/02_motifPresenceRelationship.tsv
#'
#' Output
#' pie plot graphs save as png files
#'
#' Dependencies from base R:
#' 1) tidyr
#' 2) dplyr
#' 3) ggplot2
#' 4) RColorBrewer
#' 5) ggpubr

# Importing libraries

library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
# Loading data and functions

source("./bin/R/fun/tfsFilter.R")
source("./bin/R/fun/proportionGeneration.R")
source("./bin/R/fun/dataFrameGenerator.R")
source("./bin/R/fun/pieplotSetting.R")
motifPresence <- read.table(file = "./data/processed/02_motifPresenceRelationship.tsv", header = TRUE , sep = "\t")

# Manipulating data

## First we need to set uo three categories:
### TFs with all motifs present in the BLAST output (allMotifs)
### TFs with at least one motif present BLAST output (atLeastOne)
### TFs that do not have a single motif present in the BLAST output (none0)
### TFs that do not have a single motif registrated in the Ecocyc database (noneNA)

tfsPresent <- tfsFilter()
tfsNotPresent <- tfsFilter(n = 0)
tfsNotRegister <- tfsFilter(n = "NA")

allMotifs   <- setdiff(tfsPresent, tfsNotPresent)
atLeastOne  <- intersect(tfsPresent, tfsNotPresent)
none0       <- setdiff(tfsNotPresent, tfsPresent)
noneNA      <- tfsNotRegister


# Creating the final data frames for plotting

dfTFs <- data.frame(
                  Group= c("All motifs", "At least one motif", "None motif (0s)", "None motif (NAs)"),
                  value=c(length(allMotifs), length(atLeastOne), length(none0), length(noneNA))
                )


dfTFs <- proportionGeneration(dfTFs)

dfAll         <- proportionGeneration(dataFrameGenerator(df = motifPresence, filter_array = allMotifs))
dfAtLeastOne  <- proportionGeneration(dataFrameGenerator(df = motifPresence, filter_array = atLeastOne))
dfNone0       <- proportionGeneration(dataFrameGenerator(df = motifPresence, filter_array = none0))

## Plotting using ggplot

p1 <- pieplotSetting(dfTFs,"Orthologous TFs and motifs", unit = 0.7, legend_text_size = 7, legend_title_size = 10, plot_title_size = 10, palete = TRUE)
p2 <- pieplotSetting(dfAll, "Motif description of \n 'All motifs' category",unit = 0.7, legend_text_size = 7, legend_title_size = 10, plot_title_size = 10, vector = c("#67001F", "#B2182B", "#D6604D",  "#F4A582"))
p3 <- pieplotSetting(dfAtLeastOne, "Motif description of 'At least one motif'\n(found and not found) category", unit = 0.7, legend_text_size = 7, legend_title_size = 10, plot_title_size = 10, vector = c( "#FDDBC7", "#67001F", "#B2182B" ,"#D6604D" ))
p4 <- pieplotSetting(dfNone0, "Motif description of \n 'None motif' (0s) category", unit = 0.7, legend_text_size = 7, legend_title_size = 10, plot_title_size = 10, vector = c("#67001F","#B2182B", "#905010"))


if (!dir.exists("./figs")) dir.create("./figs")

pngpath <- "./figs/pieplotMotifs.png"
png(pngpath, width=300*10, height=300*8, res=300, units="px")
ggpubr::ggarrange(p1,p2,p3,p4,
                  labels = c("A", "B","C","D"),
                  ncol = 2, nrow = 2)

dev.off()
