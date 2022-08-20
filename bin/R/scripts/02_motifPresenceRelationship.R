#!/usr/bin/env Rscript
#' ---
#' title: 02_motifPresenceRelationship.R
#'
#' author(s): Ericka Montserrat Hernandez Benitez
#'
#' date: August 18th, 2022
#'
#' version: 1
#'
#' description: 
#' The script extracts both motifs and blast sequences and check wheter they aligned.
#'
#' input parameters:
#' ./data/processed/01_mergeDatabase.tsv
#'
#' Output
#' ./data/processed/02_motifPresenceRelationship.tsv: A tsv file containg a boolean column whether the motif aligned with the BLAST sequence
#'
#' Dependencies from base R:
#' 1) tidyr
#' 2) stringr
#' 3) dplyr
#''
#' Dependencies from Bioconductor:
#' 1) Biostrings

# Loading libraries
library(Biostrings)
library(tidyr)
library(stringr)
library(dplyr)

# Loading functions

source(file = "./bin/R/fun/findingMotifPresence.R")

# Reading data
database <- read.table(file = "./data/processed/01_mergeDatabase.tsv", header = TRUE , sep = "\t")


# Manipulating data

## Adding columns with the motif and blast output sequences
## Since the function matchPattern from Biostring do not deal with the NA's we are going to substitute
## the NAs to "Z" which are a letter that does not belong to the alphabet of aminoacids


database <- database %>%
              dplyr::mutate( blastSeq = str_sub(proteinSeq, qSS, qSE), 
                             motifSeq = str_sub(proteinSeq, mSS, mSE)) %>%
              dplyr::mutate(motifSeq = replace_na(motifSeq, "Z")) %>%
              dplyr::select(tfName, motifDescription, blastSeq, motifSeq)


# Perform the function findingMotifPresence

## We are going to check first if the 100% of the motif is present in the blast output sequence

database <- database %>% 
            mutate(motifPresence100 = as.vector(mapply(FUN = findingMotifPresence, database$motifSeq, database$blastSeq, threshold = 0)))


database <- database %>%
              dplyr::select(tfName, motifDescription, motifSeq, motifPresence100) %>%
              dplyr::mutate(motifPresence100 = ifelse(motifSeq == "Z", NA, motifPresence100),
                     motifSeq = ifelse(motifSeq == "Z", NA, motifSeq)
                     ) %>%
              dplyr::select(tfName, motifDescription, motifPresence100)

## Writing the final relationship
if (!dir.exists("./data/processed")) dir.create("./data/processed")
write.table(x = database, file = "./data/processed/02_motifPresenceRelationship.tsv", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
