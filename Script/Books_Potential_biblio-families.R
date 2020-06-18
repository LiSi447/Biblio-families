#This script generates a dataset of ID and ISBN pairs to be used to identify works (and their bibliographic families)
#Currently it includes only ISBNs that were identified in the national DBs. OCLC ISBNs yet to be added

# Call packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

data <- read_csv("./Output/ISBN-ID_all-and-OCLC_2020-06-17.csv", col_types = cols(.default = "c"))
ID_metadata <- read_csv("./Output/ID-Metadata-Combined_2020-06-17.csv", col_types = cols(.default = "c"))

#Note on COBISS: COBISS subset includes 103 extra local IDs and their ISBNs that were not included earlier (previous version of the cognitive classification code did not select records whose code include "Ž": "1A/Ž", "1 A/Ž", "908(497.4A/Ž)", "913(497.4A/Ž)", "94(497.4A/Ž)"))
#Note on CRISTIN: CRISTIN subset now no longer has 211 records that were included before (178 records from 2018 and 33 for which cognitive classification is missing); also duplicates have been removed
#Previously used files: CROSBI_ID_ISBNs_20190729.csv, VABB-LoiISBN13_20190729.csv, VABB_npr-LoiISBN13_20190806.csv, CRISTIN_IDISBNs_20190806.csv, COBISS-IDISBN13_20190729.csv  

# Prepare a dataset -------------------------------------------------------

# Transform long to wide (unique local ID and all their ISBNs per row)

data_wide <- data %>% group_by(localID) %>%
  mutate(counter.ID = row_number()) %>% 
  ungroup() %>% 
  spread(counter.ID, ISBN)

# Create a mini version of the long format dataset for ID-ISBN with ISBN as the first column

data_min <- data %>% select(ISBN, localID, Source) # this to be used as the basis for the ISBN file

# FINAL DATASET: Combine the mini version of the long format with the wide format of the ID-ISBN dataset
#Note: each row contains a unique pair of ISBN and local ID and a sequence of all ISBNs that are associated with that particular local ID

data_combined <- left_join(data_min, data_wide, by = c("localID", "Source"))

# Identify ISBNs that appear with multiple local IDs

ISBN_with_multiple_IDs <- data %>% 
  group_by(ISBN) %>% 
  mutate(
    counter.ISBN = row_number()
  ) %>% 
  ungroup() %>% 
  filter(counter.ISBN > 1) %>% 
  select(ISBN)

# Filter those records of the final dataset the ISBN of which appeared with multiple local IDs

data_ISBNs.with.multipleIDs <- data_combined %>% 
  filter (data_combined$ISBN %in% ISBN_with_multiple_IDs$ISBN) %>% 
  distinct(ISBN, localID, .keep_all = TRUE) # Doesn't change anything

# Select title, local ID, and source from the metatada file

titles <- ID_metadata %>% select(Title, localID, Source)

# Add title by matching local IDs

data_ISBNs.with.multipleIDs <- left_join(data_ISBNs.with.multipleIDs, titles, by = c("localID", "Source"))

# Deduplicate

data_ISBNs.with.multipleIDs <- data_ISBNs.with.multipleIDs %>% 
  distinct(ISBN, localID, Source, Title, .keep_all = TRUE)

# Export the dataset ------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/books_potential-work_",currentDate,".csv")

write_csv(data_ISBNs.with.multipleIDs, csvFileName_1, na = "")

#Note: Previous file: write_csv(data_ISBNs.with.multipleIDs, "H:/Folder from N drive/Book study/forworkIDs_20191118.csv", na = "") #cleaned up manually
