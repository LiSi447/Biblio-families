#This script generates a list of ISBNs for GoodReads scrape and also a dataset with 6 and more ISBNs to be coded manually

# Call packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

data <- read_csv("./Output/ID-ISBNs-Combined_2020-06-17.csv", col_types = cols(.default = "c"))
ID_metadata <- read_csv("./Output/ID-Metadata-Combined_2020-06-17.csv", col_types = cols(.default = "c"))

worldcat_d1_cleaned <- read_csv2("./Manually cleaned data/manual_ISBN-OCLC_20190806_COMPLETE.csv", col_types = cols(.default = "c")) # a dataset that was cleaned manually in summer 2019
worldcat_d1_toclean <- read_csv("./Output/ocln-ISBN_tocheck_2020-06-17.csv", col_types = cols(.default = "c")) # the recently generated dataset

worldcat_d2 <- read_csv("./Output/worldcat_d2_Cleaned_2020-06-17.csv", col_types = cols(.default = "c"))

# Cross check the old and the new worldcat_d1 file

worldcat_d1_toclean$check <- worldcat_d1_toclean$ISBN %in% worldcat_d1_cleaned$ISBN

missing_validation <- worldcat_d1_toclean %>% 
  filter(check == "FALSE" & (is.na(OCLC_correct) | OCLC_correct == "TOCHECK"))

#760 records need to be checked again, at the moment I ignore them as it concern only 106 local IDs
                        
# Add ISBNs from WorldCat -------------------------------------------------

# Select only valid OCLN - ISBN links

worldcat_d1 <- worldcat_d1_cleaned %>% filter(worldcat_d1_cleaned$OCLC_correct_edited == "YES") %>% select (ISBN, ocln)

#Add OCLN numbers for ISBN
data_2 <- left_join(data, worldcat_d1, by = "ISBN")

#Add ISBNs for OCLN number
data_3 <- left_join(data_2, worldcat_d2, by = "ocln")

#Deduplicate (unique ISBN.x, localID, Source, ISBN.y)
data_4 <- data_3 %>% 
  distinct(ISBN, localID, Source, ISBN13_fin, .keep_all = TRUE) %>%
  select(ISBN13_fin, localID, Source) %>% 
  filter(!is.na(ISBN13_fin) & !ISBN13_fin %in% c("9780000000001", "9780000000002"))

names(data_4)[1] <- "ISBN"

#Combine with the ISBNs from OCLC with those that were already in the national DBs

data_5 <- rbind(data, data_4) 

#Deduplicate (unique ISBN, localID, Source)
data_5 <- data_5 %>% 
  distinct(ISBN, localID, Source, .keep_all = TRUE)

# Create a file for manually check that contains 6 or more ISBNs -----------

# Identify the records where the number of ISBN per ID is 6 or more
ISBN_per_ID <- data_5 %>% 
  count(localID, Source) %>% 
  filter(n >= 6)

# Transform to a wide form

many_ISBNs <- data_5 %>% 
  group_by(localID) %>% 
  mutate(
    n.ID = row_number()
  ) %>% 
  spread(n.ID, ISBN) %>% 
  filter(localID %in% ISBN_per_ID$localID)

# Add metadata

many_ISBNs <- left_join(many_ISBNs, ID_metadata, by = c("localID", "Source"))

# Export the dataset ------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/ISBN-ID_all-and-OCLC_",currentDate,".csv")
csvFileName_2 <- paste0("./Output/many_ISBNs_per_localID_tocheck_",currentDate,".csv")

write_csv(data_5, csvFileName_1, na = "")
write_csv(many_ISBNs, csvFileName_2, na = "")