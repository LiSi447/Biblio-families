#This script combines all the retrieved and manually validated Goodreads data


# Call packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

# Import data retrieved from Goodreads
gr_missing <- read_csv("./Raw data/Goodreads/ALL_missing_GR_20190806.csv",
                       col_types = cols(.default = "c"))
gr_cobiss <- read_csv("./Raw data//Goodreads/COBISS_GR_20190805_FIN.csv",
                      col_types = cols(.default = "c"))
gr_crosbi <- read_csv("./Raw data//Goodreads/CROSBI_GR_20190805_FIN.csv",
                      col_types = cols(.default = "c"))
gr_cristin <- read_csv("./Raw data//Goodreads/CRISTIN_GR_20190805_FIN.csv",
                       col_types = cols(.default = "c"))
gr_vabb <- read_csv("./Raw data//Goodreads/VABB_GR_20190805_FIN.csv",
                    col_types = cols(.default = "c"))
gr_missing2 <- read_csv2("./Raw data//Goodreads/missing_GR_20191203_FIN.csv",
                         col_types = cols(.default = "c"))

goodreads.RAW <- read_csv2("./Manually cleaned data/GRcleaned_20191210_raw.csv", col_types = cols(.default = "c")) 
#Note: This file contains ISBNs and their metadata for which I manually checked whether the title in Goodreads match the title in national dbs. Use the column 'FINAL_STATUS'

# Import combined ISBN / ID file

data <- read_csv("./Output/ISBN-ID_all-and-OCLC_2020-06-17.csv", col_types = cols(.default = "c"))

# Combine Goodreads data --------------------------------------------------

gr_combined <- rbind(gr_missing, gr_cobiss, gr_crosbi, gr_cristin, gr_vabb, gr_missing2) %>% 
  distinct(ISBN, .keep_all = TRUE)

# Cross check
data <- data %>% 
  mutate(test = ISBN %in% gr_combined$ISBN) %>% #All included
  select(-test) 

# Combine the data on Goodreads with the ID-ISBN dataset ---------------------

data_GR <- left_join(data, gr_combined, by = "ISBN")

# Add info on validation

validated <- goodreads.RAW %>% 
  filter(FINAL_STATUS == "VALIDATED")

data_GR <- data_GR %>% 
  mutate(
    GRdata_validated = ISBN %in% validated$ISBN
  ) %>% filter(
    in.GR == TRUE
  )

# Export data -------------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/GR-cleaned_",currentDate,".csv")

write_csv(data_GR, csvFileName_1, na = "")