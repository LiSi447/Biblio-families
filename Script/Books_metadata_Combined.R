#This script combines ID-ISBN and metadata files that were generated from national databases; the combined ISBN list is generated with Books-ISBNs_Combined.R
#ISBNs that appear in the ID-ISBN files have been standardised and deduplicated

library(tidyverse)

# Import data -------------------------------------------------------------

# Import ID-ISBN pairs
CROSBI <- read_csv("./Output/CROSBI_ID-ISBNs_2020-06-09.csv", col_types = cols(.default = "c"))
VABB_PR <- read_csv("./Output/VABB-PR_ID-ISBNs_2020-06-08.csv", col_types = cols(.default = "c"))
VABB_NPR <- read_csv("./Output/VABB-NPR_ID-ISBNs_2020-06-04.csv", col_types = cols(.default = "c"))
CRISTIN <- read_csv("./Output/CRISTIN_ID-ISBNs_2020-06-04.csv", col_types = cols(.default = "c"))
COBISS <- read_csv("./Output/COBISS_ID-ISBNs_2020-06-17.csv", col_types = cols(.default = "c"))

# Import metadata (includes ID but not ISBNs)

VABB_meta <- read_csv("./Output/VABB-PR-metadata_2020-06-08.csv", col_types = cols(.default = "c"))
VABB.npr_meta <- read_csv("./Output/VABB-NPR-metadata_2020-06-04.csv", col_types = cols(.default = "c"))
COBISS_meta <- read_csv("./Output/COBISS-metadata_2020-06-17.csv", col_types = cols(.default = "c"))
CROSBI_meta <- read_csv("./Output/CROSBI-metadata_2020-06-09.csv", col_types = cols(.default = "c"))
CRISTIN_meta <- read_csv("./Output/CRISTIN-metadata_2020-06-04.csv", col_types = cols(.default = "c"))

# Combine datasets -------------------------------------------------------

# Bind the ID-ISBN datasets (each row is a unique combination of an ISBN and a local ID) and remove inaccurate ISBNs
data <- rbind(COBISS, CRISTIN, CROSBI, VABB_PR, VABB_NPR) %>% 
  filter(!is.na(ISBN) & !ISBN %in% c("9780000000001","9780000000002"))

#Note: the cleaned ISBN file that was used in data prep scripts from national databases does not include all ISBNs. For this reason 5 CROSBI records here have missing ISBNs

# Bind the metadata datasets (each row is a unique local ID)
metadata <- rbind(VABB_meta, VABB.npr_meta, COBISS_meta, CROSBI_meta, CRISTIN_meta)

# Prepare datasets for cleaning the metadata ---------------------------------------------------

# Clean up info on publishers

publisher <- metadata %>%
  group_by(Publisher) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(publisherID = paste("publisher::", sprintf('%0.7d', 1:5342), sep="")) #previously n=5320

# Clean up info on language

language <- metadata %>% 
  group_by(Language) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(languageID = paste("language::", sprintf('%0.7d', 1:405), sep="")) #previously n=405

# Export datasets ---------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/ID-ISBNs-Combined_",currentDate,".csv")
csvFileName_2 <- paste0("./Output/ID-Metadata-Combined_",currentDate,".csv")
csvFileName_3 <- paste0("./Output/Language_",currentDate,".csv")
csvFileName_4 <- paste0("./Output/Publishers_",currentDate,".csv")

write_csv(data, csvFileName_1, na = "")
write_csv(metadata, csvFileName_2, na = "")
write_csv(publisher, csvFileName_3, na = "") #files that were generated before Publishers_20191204_ID.csv, Publishers_20191204.csv 
write_csv(language, csvFileName_4, na = "") #file that was generated before: Language_20191204_ID.csv
