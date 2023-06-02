# Downloading genomes
library(googlesheets4)
library(tidyverse)
library(snakecase)
library(R.utils)

# Download our data from google drive:
genomeData <- read_sheet("https://docs.google.com/spreadsheets/d/166mvT3i85SJO25s155Dm_SBkzlRPCRHsQDl4Sj9SOLA/edit?usp=sharing",
                         sheet = "Sheet4")

# Download and unzip each genome:
dir.create(path = "./rawGenomeFiles/")
downloadGenomes <- function(url, species) {
  speciesCamel <- to_lower_camel_case(species)
  fileName <- paste("./rawGenomeFiles/",
                    speciesCamel,
                    ".fna.gz",
                    sep = "")
  download.file(url = url,
                destfile = fileName,
                method = "wget")
  gunzip(fileName)
}

possiblyDownloadGenomes <- possibly(downloadGenomes, 
                                    otherwise = "Error.")
map2(genomeData$downloadLink, 
     genomeData$species, 
     possiblyDownloadGenomes)
