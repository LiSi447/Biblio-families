#This script generates workIDs on the basis of the file where the overlap of ISBNs was checked manually (includes both duplicates and bibliographic families)

# Call packages -----------------------------------------------------------

library(tidyverse)
library(readxl)


# Import data -------------------------------------------------------------

# Import the local ID and metadata dataset

data <- read_csv("./Output/ID-Metadata-Combined_2020-06-17.csv", col_types = cols(.default = "c"))

# Import the most recent manually validated work / bibliographic family file

workIDsRAW <- read_excel("./Manually cleaned data/Identify_work_families_20200609.xlsx",
                         sheet = "books_potential-work_2020-06-09",
                         col_types = "text")

#Note: Each row corresponds to a unique combination of an ISBN and a local ID along with all the ISBNs (also from OCLC) associated with the local ID
#Records with a number in the column ID have been validated

# Generate work IDs for the dataset with multiple local ids per work -------------------------------------------------------

#Create a smaller working dataset

workIDsRAW_mini <- workIDsRAW %>% 
  select(ID_edited,localID, Source)  %>% 
  filter(str_detect(ID_edited,  "[:digit:]+" ))

names(workIDsRAW_mini)[1] <- "ID"

# Rearrange the dataset so that each row corresponds to a unique combination of a preliminary work ID (column 'ID'), local ID and the Source (overlap of local IDs across databases)

workIDs_distinct <- workIDsRAW_mini %>% group_by(ID, localID, Source) %>% count()

# Transform the ID column from character to integer vector
workIDs_distinct$ID <- as.integer(workIDs_distinct$ID)

# Generate final identifiers
workIDs_final <- workIDsRAW_mini %>% 
  distinct(ID) %>% 
  mutate(workID = paste("books::", sprintf('%0.7d', 1:1276), sep=""))

# Transform from long to wide format and identifiers to works
workIDs_distinct_wide <- workIDs_distinct %>% # with Source
  group_by(ID) %>% 
  mutate(counter_ID = row_number()) %>% 
  ungroup() %>% 
  select(ID, localID, counter_ID, Source) %>% 
  spread(counter_ID, localID)  %>% 
  mutate(workID = workIDs_final$workID[match(ID, workIDs_final$ID)])

# Transform back to long and remove the raw ID

workIDs_final_long <- workIDs_distinct_wide %>% 
  gather(ID, localID, `1`:`13`) %>% 
  select(localID, Source, workID) %>% 
  filter(!is.na(localID))

# Generate work IDs for the remaining local IDs ---------------------------

# Add work IDs to the complete ID / metadata file

workIDs_complete <- left_join(data, workIDs_final_long, by = c("localID", "Source"))

# Generate work IDs for records that don't have it

workIDs_complete2 <- workIDs_complete %>% 
  mutate(workID = ifelse(is.na(workID), paste0("books::", sprintf('%0.7d', 1277:20608)), workID)) %>% 
  select(localID, Source, workID)

#Note for rows with non-missing workID, the counting of the workID skips one (e.g. from 202 to 204)

# Correct records that were identified as wrong manually (when checking missing book categories)

workIDs_complete2$workID[workIDs_complete2$workID == "books::0000256" & workIDs_complete2$localID == "479885"] <- "books::0020609"
workIDs_complete2$workID[workIDs_complete2$workID == "books::0000786" & workIDs_complete2$localID == "255218432"] <- "books::0020610"
workIDs_complete2$workID[workIDs_complete2$workID == "books::0000831" & workIDs_complete2$localID == "255218176"] <- "books::0020610"

# Export data -------------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/books_workIDs_",currentDate,".csv")

write_csv(workIDs_complete2, csvFileName_1, na = "")
