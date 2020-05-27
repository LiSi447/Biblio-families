#! This generates a dataset for monographs from CROSBI

## call package

library(tidyverse)
#library(rvest)
#library(RCurl)

# import data
CROSBIdata_FULL <- read_csv2("./Raw data/CROSBI/CROSBI_books.csv",
                             col_types = cols(.default = "c"),
                             locale = readr::locale(encoding = "UTF-8"))
CROSBItoOECD <- read_csv2("./Raw data/CROSBI/CROSBI to OECD.csv",
                          col_types = cols(.default = "c")) # concordance table was made using the CROSBI data file and the column 'DISCIPLINE'
cleaned_ISBNs <- read_csv("./Raw data/forAnalysis_ISBNs_20190729.csv",
                          col_types = cols(.default = "c"))
CROSBI_language <- read_csv("./Output/CROSBI-ID-language_2020-05-15.csv",
                            col_types = cols(.default = "c"))
# assign CROSBI identifier

CROSBIdata_FULL$crosbiID <- paste("crosbi::", sprintf('%0.7d', 1:length(CROSBIdata_FULL$`TITLE (ORIGINAL)`)), sep="")

# extract CROSBI internal identifier

CROSBIdata_FULL$crosbiID.local <- str_replace_all(CROSBIdata_FULL$URL, "https://www.bib.irb.hr/", "")

# add data on language

CROSBIdata_FULL <- left_join(CROSBIdata_FULL, CROSBI_language, by = "crosbiID.local")

# delineate monographs and time span

years <- c(2000:2017)
books <- CROSBIdata_FULL %>% filter(`BOOK TYPE` %in% c("monograph", "monography") & 
                                      YEAR %in% years)

# remove records with missing information on discipline or ISBN

books2 <- books %>% filter_at(c("DISCIPLINE", "_ISBN"), all_vars(!is.na(.)))

# assign cognitive classification

count <- max(str_count(books2$DISCIPLINE, ";")) + 1

classification <- books2 %>% select(crosbiID.local, DISCIPLINE) %>% 
  separate(DISCIPLINE, paste0("d", 1:count), sep = "; ") %>% 
  gather(n, DISCIPLINE, d1:d5) %>% 
  filter(!is.na(DISCIPLINE)) %>% 
  mutate(FOS = CROSBItoOECD$FOS[match(DISCIPLINE, CROSBItoOECD$CROSBI_disc)]) %>% 
  group_by(crosbiID.local) %>% 
  mutate(counter_ID = paste0("FOS",row_number())) %>% 
  ungroup() %>% 
  select(crosbiID.local, counter_ID, FOS) %>% 
  spread(counter_ID, FOS)

books2 <- left_join(books2, classification, by = "crosbiID.local")

cog_vars_SSH.CROSBI <- paste0("FOS", 1:5)
cog_vars_SSH.FOS <- c(paste0("5_", 0:9),
                      paste0("6_", 0:5),
                      paste0("6_1_", 1:2),
                      paste0("6_2_", 1:2),
                      paste0("6_3_", 1:2))

books2$cog.SSH <- NA
for (i in 1:nrow(books2)) {
  for (var in cog_vars_SSH.CROSBI) {
    if (books2$cog.SSH[i] %in% TRUE) {
      break
    } else {
      books2$cog.SSH[i] <- books2[var][i, ] %in% cog_vars_SSH.FOS
    }
  }
}

books3 <- books2 %>%
  filter(cog.SSH == TRUE) # 401 records removed

#validate ISBN structure and filter records with valid ISBNs

pats <- c("-| ")
books3$ISBN_cleaned <- str_replace_all(books3$`_ISBN`, pats, "")

books3 <- books3 %>% 
  mutate(
    ISBN13 = str_detect(books3$ISBN_cleaned, "^97[:alnum:]{11}$"),
    ISBN10 = str_detect(books3$ISBN_cleaned, "^[:alnum:]{10}$")
  )

books4 <- books3 %>% 
  filter(books3$ISBN13 == TRUE | books3$ISBN10 == TRUE) # 103 records removed

#extract ISBNs

listISBNs <- as.tibble(books4$ISBN_cleaned)
names(list_ISBNs) <- "ISBN"
listISBNs <- distinct(list_ISBNs)

#prepare the dataset for analysis (v.1) IDs and ISBNs

ID_ISBN <- books4 %>% 
  select(crosbiID, crosbiID.local, ISBN_cleaned) %>% 
  mutate(
    ISBN13 = cleaned_ISBNs$ISBN13_fin[match(ISBN_cleaned, cleaned_ISBNs$ISBN)]
  ) %>% 
  select(crosbiID, crosbiID.local, ISBN13) %>% 
  distinct(crosbiID.local, ISBN13, .keep_all = TRUE)

names(ID_ISBN)[2] <- "ISBN"

#prepare the dataset for analysis (v.2) descriptive metadata

books_metadata <- books4 %>% 
  select(crosbiID, crosbiID.local, AUTHORS, `TITLE (ORIGINAL)`, `TITLE (IN ENGLISH)`, YEAR,
                                        PUBLISHER, language)
#export datasets

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/CROSBI-metadata_",currentDate,".csv")
csvFileName_2 <- paste0("./Output/CROSBI_ID-ISBNs_",currentDate,".csv")
csvFileName_3 <- paste0("./Output/CROSBI-ISBNs_",currentDate,".csv")

write_csv(books_metadata, csvFileName_1, na = "")
write_csv(ID_ISBN, csvFileName_2, na = "")
write_csv(listISBNs, csvFileName_3, na = "")