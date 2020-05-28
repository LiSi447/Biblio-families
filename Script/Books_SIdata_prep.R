#! This generates datasets for monographs from COBISS

# call package

library(tidyverse)

# import data

COBISSdata <- read_csv2("./Raw data/COBISS/COBISS-raw.csv",
                        locale = readr::locale(encoding = 'UTF-8'),
                        col_types = cols(.default = "c"))

COBISSdata_ISBNs <- read_csv2("./Raw data/COBISS/COBISS-ISBNs_cleaned_20190710_2.csv",
                              col_types = cols(.default = "c")) # ISBNS extracted from XML and cleaned manually

UDC <- read_csv2("./Raw data/COBISS/COBISS-UDC_ssh.csv",
                 col_types = cols(.default = "c")) # UDC codes classified manually

cleaned_ISBNs <- read_csv("./Raw data/forAnalysis_ISBNs_20190729.csv",
                          col_types = cols(.default = "c"))

# add ISBNs from the manually cleaned file

COBISS_ISBNs <- COBISSdata_ISBNs %>% select(`COBISS.SI-ID`, ISBN_edited)

names(COBISS_ISBNs)[2] <- "ISBN"

COBISSdata2 <- left_join(COBISSdata, COBISS_ISBNs, by = "COBISS.SI-ID")

# delineate records by year and remove records with missing ISBN

years <- c(2000:2017)
books <- filter(COBISSdata2, Year %in% years & !is.na(ISBN))

# clean up ISBNs

books_ISBNs <- books %>% 
  select(`COBISS.SI-ID`, ISBN) %>% 
  separate(
    ISBN, paste0("ISBN", 1:max(str_count(books$ISBN, ";") + 1)), sep = ";"
  ) %>% 
  gather(
    key = "ISBN_nr", value = "ISBN", ISBN1:ISBN12) %>% 
  filter(
    !is.na(ISBN)
  ) %>% 
  mutate(
    ISBN2 = str_replace_all(ISBN, "-", ""), 
    ISBN13 = ifelse(str_detect(ISBN2, "^97[:alnum:]{11}$"), "YES", "NO"), #this and below was used to generate the file of ISBNs that need cleaning
    ISBN10 = ifelse(str_detect(ISBN2, "^[:alnum:]{10}$"), "YES", "NO"),
    ISBN13_cleaned = cleaned_ISBNs$ISBN13_fin[match(ISBN2, cleaned_ISBNs$ISBN)]) %>% 
  select(`COBISS.SI-ID`, ISBN_nr, ISBN13_cleaned) %>% 
  spread(ISBN_nr, ISBN13_cleaned)

books2 <- left_join(books, books_ISBNs, by = "COBISS.SI-ID")
  
# identify SSH publications

SSH_codes <- UDC %>% # this is a spreadsheet where I have manually marked those codes that belong to SSH
  filter(UDC$is.SSH_manual == TRUE) %>% 
  select(code) %>% 
  distinct()

difficult_SSH_codes <- c("1A/Ž", "1 A/Ž", "908(497.4A/Ž)", "913(497.4A/Ž)", "94(497.4A/Ž)")

classification <- books %>% select(`COBISS.SI-ID`, UDC) %>% 
  separate(UDC, paste0("d", 1:6), sep = "; ") %>% 
  gather(
    key = "d_nr", "UDC", d1:d6) %>% 
  filter(
    !is.na(UDC)
  ) %>% 
  separate(UDC, c("UDC_code", "UDC_category"), sep = "  - ", remove = FALSE) %>% 
  mutate(is.SSH = ifelse(UDC_code %in% SSH_codes$code | UDC_code %in% difficult_SSH_codes, TRUE, FALSE)) %>% 
  filter(is.SSH == TRUE)

# classification$check <- classification$UDC_code %in% UDC$code # this was used to identify the difficult SSH codes

books3 <- books2 %>% 
  filter(`COBISS.SI-ID` %in% classification$`COBISS.SI-ID`)

# extract ISBNs

ISBN.vars <- paste0("ISBN", 1:12)

listISBNs <- books3 %>% 
  select(`COBISS.SI-ID`, ISBN.vars) %>%
  gather("N-ISBN", "ISBN", all_of(ISBN.vars)) %>% 
  select(ISBN) %>% 
  distinct(ISBN)

# prepare the dataset for analysis (v.1) ID and ISBNs

ID_ISBN <- books3 %>% 
  select(`COBISS.SI-ID`, ISBN.vars) %>% 
  gather("N-ISBN", "ISBN", all_of(ISBN.vars)) %>% 
  select(-`N-ISBN`)

# prepare the dataset for analysis (v.2)

books_metadata <- books3 %>% 
  select(`COBISS.SI-ID`, Year, Title, Publisher, Country, language, `Last name, First name / researcher's code`)

# export datasets

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/COBISS-metadata_",currentDate,".csv")
csvFileName_2 <- paste0("./Output/COBISS_ID-ISBNs_",currentDate,".csv")
csvFileName_3 <- paste0("./Output/COBISS-ISBNs_",currentDate,".csv")

write_csv(books_metadata, csvFileName_1, na = "")
write_csv(ID_ISBN, csvFileName_2, na = "")
write_csv(listISBNs, csvFileName_3, na = "")
