#This script generates datasets for analysis

# Load packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

ID_ISBNs <- read_csv("./Output/ID-ISBNs-Combined_2020-06-17.csv", col_types = cols(.default = "c"))
ID_ISBNs_ALL <- read_csv("./Output/ISBN-ID_all-and-OCLC_2020-06-17.csv", col_types = cols(.default = "c"))
ID_metadata <- read_csv("./Output/ID-Metadata-Combined_2020-06-17.csv", col_types = cols(.default = "c"))
workIDs <- read_csv("./Output/books_workIDs_2020-06-17.csv", col_types = cols(.default = "c"))

metadata_cleaned <- read_csv("./Output/books_metadata_cleaned_2020-06-17.csv", col_types = cols(.default = "c"))

GR <- read_csv("./Output/GR-cleaned_2020-06-17.csv", col_types = cols(.default = "c"))

worldcat_d1_cleaned <- read_csv2("./Manually cleaned data/manual_ISBN-OCLC_20190806_COMPLETE.csv", col_types = cols(.default = "c")) # a dataset that was cleaned manually in summer 2019
WC.2 <- read_csv("./Output/worldcat_d2_Cleaned_2020-06-17.csv", col_types = cols(.default = "c")) # this will identify additional ISBNs from WC


# Dataset A - ISBN as the unit of analysis --------------------------------

WC.1 <- worldcat_d1_cleaned %>% filter(worldcat_d1_cleaned$OCLC_correct_edited == "YES") %>% select (ISBN, ocln)

# Add workID

data_A <- left_join(ID_ISBNs_ALL, workIDs, by = c("localID", "Source"))

# Add info on COBISS authors (info on language, publishers and book categories also added)

data_A <- left_join(data_A, metadata_cleaned, by = c("localID", "Source"))

# Identify ISBNs that were in WorldCat and Goodreads

GR <- GR %>% filter(in.GR == TRUE)

data_A <- data_A %>% 
  mutate(
    in.WC = ISBN %in% WC.1$ISBN, #national DB isbns identified in WC
    from.WC = ISBN %in% WC.2$ISBN13_fin,
    in.GR = ISBN %in% GR$ISBN,
    ISBN.national = ISBN %in% ID_ISBNs$ISBN
  )

# Keep only COBISS records with researchers as authors

data_A1 <- data_A %>% 
  filter(COBISS_author == TRUE | is.na(COBISS_author))

# Calculate the number of ISBNs per workID
n.ISBNs.perWork <- data_A1 %>% 
  distinct(workID, ISBN, .keep_all = TRUE) %>% 
  group_by(workID) %>% 
  count()

names(n.ISBNs.perWork)[2] <- "n.ISBNs.perWork"

# Deduplicate ISBNs (this should be used for the tabular overview of ISBNs)
unique.ISBNs <- data_A1 %>% 
  distinct(ISBN, Source, .keep_all = TRUE) %>% 
  select(ISBN, Source, workID, in.WC, in.GR, from.WC, ISBN.national, COBISS_author)

# Dataset B - local ID as the unit of analysis ----------------------------

# Add workID

data_B <- left_join(ID_metadata, workIDs, by = c("localID", "Source"))

# Identify localIDs that were in WorldCat and Goodreads (use Dataset A)
#Below incorrect: overlap of CROSBI, COBISS and CRISTIN id

in.WC <- data_A1 %>% filter(in.WC == TRUE & ISBN.national == TRUE) %>% select(localID, Source, in.WC, workID)
in.GR <- data_A1 %>% filter(in.GR == TRUE) %>% select(in.GR, localID, Source, workID)

in.WC.GR <- full_join(in.WC, in.GR, by = c("localID", "Source", "workID"))

#data_B <- data_B %>% 
 # mutate(
  #  in.WC = localID %in% in.WC$localID,
   # in.GR = localID %in% in.GR$localID
  #)

data_B <- left_join(data_B, in.WC.GR, by = c("localID", "Source", "workID"))


# Add the cleaned metadata

data_B <- left_join(data_B, metadata_cleaned, by = c("localID", "Source"))

# Remove COBISS records that are not by researchers in the author role

data_B1 <- data_B %>%
  filter(COBISS_author == TRUE | is.na(COBISS_author))

# Deduplicate
data_B2 <- data_B1 %>% 
  distinct(localID, Source, .keep_all = TRUE)

# Dataset C - work ID as the unit of analysis -----------------------------

# Identify workIDs that were in WorldCat and in Goodreads (use Dataset B)

data_C <- workIDs %>% 
  mutate(
    in.WC = workID %in% in.WC$workID,
    in.GR = workID %in% in.GR$workID
  )

# Add cleaned metadata

data_C <- left_join(data_C, metadata_cleaned, by = c("localID", "Source"))

# Add info on number of ISBNs per workID (use Dataset A)

data_C <- left_join(data_C, n.ISBNs.perWork, by = "workID")

# Remove COBISS records that are not by researchers in the author role

data_C <- data_C %>%
  filter(COBISS_author == TRUE | is.na(COBISS_author))

# Deduplicate

data_C1 <- data_C %>% distinct(Source, workID, .keep_all = TRUE) %>% 
  select(-localID)

# Identify missing and correct existin book categories ----------------------------------------
# At this stage I checked for missing book categories and exported a file that was checked manually. Results were added to the cleaned metadata script and add workIDs script.

#manyISBNs.missing <- data_C %>% filter(n.ISBNs.perWork >= 6)
#manyISBNs.missing <- left_join(manyISBNs.missing, ID_metadata, by = c("localID", "Source"))

# Group 2 -----------------------------------------------------------------

Group2.all.localIDs <- data_C %>% 
  filter(n.ISBNs.perWork >= 6 & book_category == "multiISBN") %>% 
  left_join(ID_metadata, by = c("localID", "Source")) %>% 
  select(-Publisher, -Language)

Group2.one.localID <- data_C1 %>% 
  filter(n.ISBNs.perWork >= 6 & book_category == "multiISBN")


# Group 3 -----------------------------------------------------------------

Group3.all.localIDs <- data_C %>% 
  filter(n.ISBNs.perWork >= 6 & book_category == "Misc") %>% 
  left_join(ID_metadata, by = c("localID", "Source")) %>% 
  select(-Publisher, -Language)

Group3.one.localID <- data_C1 %>% 
  filter(n.ISBNs.perWork >= 6 & book_category == "Misc")

# Export data -------------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/books_multiISBN_",currentDate,".csv")
#write_csv(manyISBNs.missing, csvFileName_1, na = "") 

csvFileName_2 <- paste0("./Output/dataset_A_ISBN_",currentDate,".csv")
write_csv(unique.ISBNs, csvFileName_2, na = "")

csvFileName_3 <- paste0("./Output/dataset_B_localID_",currentDate,".csv")
write_csv(data_B2, csvFileName_3, na = "")

csvFileName_4 <- paste0("./Output/dataset_C_workID_",currentDate,".csv")
write_csv(data_C1, csvFileName_4, na = "")

csvFileName_5 <- paste0("./Output/Group2_all.localIDs_",currentDate,".csv")
write_csv(Group2.all.localIDs, csvFileName_5, na = "")

csvFileName_6 <- paste0("./Output/Group2_one.localID_",currentDate,".csv")
write_csv(Group2.one.localID, csvFileName_6, na = "")

csvFileName_7 <- paste0("./Output/Group3_all.localIDs_",currentDate,".csv")
write_csv(Group3.all.localIDs, csvFileName_7, na = "")

csvFileName_8 <- paste0("./Output/Group3_one.localID_",currentDate,".csv")
write_csv(Group3.one.localID, csvFileName_8, na = "")