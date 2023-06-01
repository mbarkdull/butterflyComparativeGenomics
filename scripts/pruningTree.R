# Load packages:
library(ape)
library(phytools)
library(tidyverse)
library(ggtree)
library(diversitree)
library(googlesheets4)
library(stringr)

# Read in a tree from Kawahara et al 2023:
kawahara2023Tree <- read.tree(file = "./KawaharaEtAl2023/Data.S21.MLtrees/tree7_AA154partitions_renamed.tre")
kawahara2023Tree[["tip.label"]]

# Download our list of species from google drive:
ourSpecies <- read_sheet("https://docs.google.com/spreadsheets/d/166mvT3i85SJO25s155Dm_SBkzlRPCRHsQDl4Sj9SOLA/edit?usp=sharing",
                         sheet = "Sheet4")

# Add underscores so they better match the tip labels in the Kawahara tree:
ourSpecies$species <- gsub(" ", "_", ourSpecies$species)

# Get a column for genus:
ourSpecies$genus <- sub("_.*", 
                        "", 
                        ourSpecies$species)

# Write a function to search for our species in the larger tree and map it over all our species:
findingMatchingSpecies <- function(i) {
  match <- str_subset(kawahara2023Tree[["tip.label"]], 
                      i)
  data <- c(i, match)
  return(data)
}

possiblyFindingMatchingSpecies <- possibly(findingMatchingSpecies,
                                           otherwise = "Error")

speciesLevelMatches <- purrr::map(ourSpecies$species,
                                  possiblyFindingMatchingSpecies)

speciesLevelMatches <- map_dfr(speciesLevelMatches, 
                               ~as_data_frame(t(.)))

# Check which species did not have a species-level match in the Kawahara phylogeny, then look for a genus-level match:
missingSpecies <- filter(speciesLevelMatches,
                         is.na(V2))

findingMatchingGenera <- function(i) {
  genus <- sub("_.*", 
               "", 
               i)
  genusMatches <- str_subset(kawahara2023Tree[["tip.label"]], 
                             genus)
  if (length(genusMatches) > 1) {
    match <- sample(genusMatches, 1)
  } else {
    match <- genusMatches
  }
  data <- c(i, match)
  return(data)
}

genusLevelMatches <- purrr::map(missingSpecies$V1,
                                findingMatchingGenera)
genusLevelMatches <- map_dfr(genusLevelMatches, 
                             ~as_data_frame(t(.)))

# Combine all the matches:
allMatches <- rbind(speciesLevelMatches,
                    genusLevelMatches) %>%
  filter(!is.na(V2))


# Trim the tree:
trimmedTree <- keep.tip(kawahara2023Tree, allMatches$V2)
ggtree(trimmedTree) +
  geom_tiplab(fontface = 'italic') +
  xlim(0, 0.8)

# Replace with the original species names:
tipLabelDictionary <- allMatches %>%
  distinct(V2,
           .keep_all = TRUE)

renamedTree <- trimmedTree
renamedTree[["tip.label"]] <- plyr::mapvalues(renamedTree[["tip.label"]], 
                                              from = tipLabelDictionary$V2, 
                                              to = tipLabelDictionary$V1)
ggtree(renamedTree) +
  geom_tiplab(fontface = 'italic') +
  xlim(0, 0.8)

# Still need to resolve the species that are missing from the tree:
allMatches$inTree <- allMatches$V1 %in% renamedTree[["tip.label"]]  
allMatches$genus <- sub("_.*", 
                        "", 
                        allMatches$V1)





