#!/usr/bin/env Rscript
#' ---
#' title: 01_mergeData.R
#'
#' author(s): Ericka Montserrat Hernandez Benitez
#'
#' date: August 18th, 2022
#'
#' version: 1
#'
#' description: 
#' The script performed the merging of several tables from databases of E.coli
#'
#' input parameters:
#' 1) ./data/raw/ecoliAnnotation.tsv 
#' 2) ./data/raw/geneAASeq.tsv
#' 3) ./data/raw/MotifsSeqRelation.tsv
#' 4) ./data/raw/Orthologous_ECaaq_RZaadb_blastN_b1_m8.tab
#' 5) ./data/raw/TFs_coli.txt

#'
#' Output
#' 1) tab-delimited database. 
#'
#' Dependencies from base R:
#' 1) dplyr
#' 2) tidyr


# Loading libraries

library(dplyr)
library(tidyr)

# Reading data

ecoliAnnotation   <- read.table(file = "./data/raw/ecoliAnnotation.tsv", header = TRUE , sep = "\t")
geneAASeq         <- read.table(file = "./data/raw/geneAASeq.tsv", header = FALSE , sep = "\t")
motifsSeqRelation <- read.table(file = "./data/raw/MotifsSeqRelation.tsv", header = FALSE , sep = "\t")
blastResult       <- read.table(file = "./data/raw/Orthologous_ECaaq_RZaadb_blastN_b1_m8.tab", header = TRUE , sep = "\t")
tfsEcoli          <- read.table(file = "./data/raw/TFs_coli.txt", header = FALSE , sep = "\t")

# Cleaning data

ecoliAnnotation <- ecoliAnnotation %>%
                    dplyr::select(ProteinID, Locus_tag, NCBI_name) %>%
                    dplyr::rename(proteinID = ProteinID, locusTag = Locus_tag, tfName = NCBI_name) %>%
                    as_tibble()


geneAASeq <- geneAASeq %>%
              dplyr::rename(proteinID = V1, proteinSeq = V2) %>%
              as_tibble()
  


motifsSeqRelation <- motifsSeqRelation %>%
                      dplyr::rename(tfName = V1, locusTag = V2, motifDescription = V3, mSS = V4, mSE = V5) %>%
                      dplyr::select(locusTag, motifDescription, mSS ,mSE) %>%
                      dplyr::mutate(locusTag = na_if(locusTag, "")) %>%
                      tidyr::fill(locusTag) %>%
                      as_tibble()
  
blastResult <- blastResult %>%
                dplyr::select(qName, qSS, qSE) %>%
                dplyr::mutate(qName = str_extract(qName, "[NY]P_.*")) %>%
                dplyr::rename(proteinID = qName) %>%
                as_tibble()


tfsEcoli  <- tfsEcoli %>%
              dplyr::rename(tfName= V1) %>%
              as_tibble()

# Merging the data

df <- blastResult %>%
        dplyr::left_join(ecoliAnnotation, by = "proteinID") %>%
        dplyr::inner_join(tfsEcoli, by = "tfName") %>%      
        dplyr::left_join(motifsSeqRelation, by = "locusTag") %>%
        dplyr::left_join(geneAASeq, by = "proteinID") %>%
        dplyr::select(tfName, locusTag, proteinID, qSS, qSE, motifDescription, mSS, mSE, proteinSeq)

# Writing the complete merging data in data/processed

if (!dir.exists("./data/processed")) dir.create("./data/processed")
write.table(x = df, file = "./data/processed/01_mergeDatabase.tsv", append = FALSE, quote = FALSE, sep = "\t", col.names =  TRUE, row.names = FALSE)
