#! This generates a dataset for monographs from CRISTIN

# Call packages

library(tidyverse)
library(foreign)

# Import data

CRISTINdata.FULL <- as.tibble(read.dta("./Raw data/CRISTIN/Cristin_2005_2017.dta")) # complete CRISTIN dataset

CRISTINdata.raw <- read_csv2("./Raw data/CRISTIN//Vitenskaplige_monografier_2011_2018_NVI.csv",
                             col_types = cols(.default = "c")) # additional CRISTIN dataset on monographs, NVI subset with cognitive classification

manual.cog <- read_csv2("./Raw data/CRISTIN/CRISTIN_Books_missingcog_20190805_COMPLETE.csv",
                        col_types = cols(.default = "c")) # dataset with monographs with manually added cognitive classification
cleaned_ISBNs <- read_csv("./Raw data/forAnalysis_ISBNs_20190729.csv",
                          col_types = cols(.default = "c"))

# Change column names to upper case (for easier matching with the full CRISTIN dataset)
names(CRISTINdata.FULL) <- toupper(names(CRISTINdata.FULL))

# Delineate year and publication type and transform the institution variable

books <- CRISTINdata.FULL %>% 
  filter(ARSTALL %in% c(2005:2017) & VARBEIDUNDERKATKODE == "MONOGRAFI") %>% 
  mutate(INSTITUSJONSNR.ed = str_remove(INSTITUSJONSNR, "^0+"))
booksA <- filter(CRISTINdata.raw, ARSTALL %in% c(2005:2017))

# Omit records with missing ISBN

books <- filter(books, ISBN != "")
booksA <- filter(booksA, ISBN != "")

# Delineate institutions

NORW.inst <- c("184",
                "185",
                "186",
                "192",
                "194",
                "201",
                "204",
                "217")

books2 <- filter(books, INSTITUSJONSNR.ed %in% NORW.inst)
books2A <- filter(booksA, INSTITUSJONSNR %in% NORW.inst)


# Combine the two datasets ------------------------------------------------

# Create smaller working datasets

books_mini <- books2 %>% select(VARBEIDLOPENR, KVALITETSNIVAKODE, ISBN)
books2A_mini <- books2A %>% select(VARBEIDLOPENR, KVALITETSNIVAKODE, fagomrade, ISBN)

# Transform the CRISTIN id variable into a character variable
books_mini$VARBEIDLOPENR <- as.character(books_mini$VARBEIDLOPENR)

# Join the two datasets

books_mini2 <- full_join(books_mini, books2A_mini, by = c("VARBEIDLOPENR", "ISBN", "KVALITETSNIVAKODE"))

# Validate ISBNs ----------------------------------------------

# Add missing ISBNs 

books_mini2 <- books_mini2 %>% 
  mutate(
    ISBN = ifelse(ISBN == "NULL", books$ISBN[match(VARBEIDLOPENR, books$VARBEIDLOPENR)], ISBN)
  )

# Correct erroneous ISBNs (corrected only ISBNs that actually are already present in the dataset, but have some extra characters)

books_mini2$ISBN[which(books_mini2$VARBEIDLOPENR == "936346")] <- "9780415590389"
books_mini2$ISBN[which(books_mini2$VARBEIDLOPENR == "993382")] <- "9788251924061"
books_mini2$ISBN[which(books_mini2$VARBEIDLOPENR == "1025694")] <- "9788202412777"
books_mini2$ISBN[which(books_mini2$VARBEIDLOPENR == "1040745")] <- "9788215021119"
books_mini2$ISBN[which(books_mini2$VARBEIDLOPENR == "1512100")] <- "9780198779865"

# Validate ISBN structure and filter records with valid ISBNs

pats <- c("-| ")
books_mini2$ISBN_cleaned <- str_replace_all(books_mini2$ISBN, pats, "")

books_mini2 <- books_mini2 %>% 
  mutate(
    ISBN13 = str_detect(ISBN_cleaned, "^97[:alnum:]{11}$"),
    ISBN10 = str_detect(ISBN_cleaned, "^[:alnum:]{10}$")
  )

books_mini3 <- books_mini2 %>% 
  filter(ISBN13 == TRUE | ISBN10 == TRUE) %>% # 53 records (including duplicate IDs) removed
  distinct(VARBEIDLOPENR, .keep_all = TRUE)

# Identify peer-reviewed SSH ---------------------------------------------

# Add missing cognitive classification , delineate SSH and select unique CRISTIN IDs
books4 <- books_mini3 %>% 
  mutate(fagomrade = ifelse(is.na(fagomrade), booksA$fagomrade[match(VARBEIDLOPENR, booksA$VARBEIDLOPENR)], fagomrade),
         SSH = ifelse(fagomrade %in% c("Humaniora", "Samfunnsvitenskap", "Samfunnsvitnskap"), TRUE,
                                       ifelse(! fagomrade %in% c("Medisin og helsefag", "Realfag og teknologi"), FALSE, NA)),
         SSH.manual = manual.cog$SSH.MANUAL[match(VARBEIDLOPENR, manual.cog$VARBEIDLOPENR)]) %>% 
  filter(SSH == TRUE | SSH.manual == TRUE)
  
#Note: Cognitive classification to records where this info is missing was identified manually checking metadata (in Excel)

# Prep data for analysis --------------------------------------------------

# Extract ISBNs

listISBNs <- as.tibble(books4$ISBN_cleaned)
names(listISBNs) <- "ISBN"
listISBNs <- distinct(listISBNs)

# Prepare the dataset for analysis (v.1) IDs and ISBNs
ID_ISBN <- books4 %>%
  select(VARBEIDLOPENR, ISBN_cleaned) %>% 
  mutate(ISBN13 = cleaned_ISBNs$ISBN13_fin[match(ISBN_cleaned, cleaned_ISBNs$ISBN)]) %>% 
  filter(!is.na(ISBN13)) %>% 
  select(ISBN13, VARBEIDLOPENR) 

names(ID_ISBN) <- c("ISBN", "localID")

ID_ISBN$Source <- "CRISTIN"

# Prepare the dataset for analysis (v.2) descriptive metadata

books_MINI2 <- books4 %>% select(VARBEIDLOPENR)

CRISTINdata.FULL$VARBEIDLOPENR <- as.character(CRISTINdata.FULL$VARBEIDLOPENR)

CRISTINdata.raw$ARSTALL <- as.integer(CRISTINdata.raw$ARSTALL)

books_MINI2A <- inner_join(books_MINI2, CRISTINdata.FULL, by = "VARBEIDLOPENR") %>% 
  distinct(VARBEIDLOPENR, .keep_all = TRUE) %>% 
  select(VARBEIDLOPENR, TITTELTEKST_ORIGINAL, ARSTALL, UTGIVERNAVN, ETTERNAVN, SPRAKKODE_ORIGINAL)
names(books_MINI2A)[6] <- "SPRAKKODE"

books_MINI2B <- inner_join(books_MINI2, CRISTINdata.raw, by = "VARBEIDLOPENR") %>% 
  distinct(VARBEIDLOPENR, .keep_all = TRUE) %>%
  select(VARBEIDLOPENR, TITTELTEKST_ORIGINAL, ARSTALL, UTGIVERNAVN, ETTERNAVN, SPRAKKODE)

books_metadata <- rbind(books_MINI2A, books_MINI2B) %>% 
  distinct(VARBEIDLOPENR, .keep_all = TRUE)

books_metadata$Source <- "CRISTIN"
names(books_metadata) <- c("localID", "Title", "Year", "Publisher", "Author", "Language", "Source")
books_metadata <- books_metadata %>% select(localID, Title, Author, Year, Publisher, Language, Source)


# Export datasets ---------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/CRISTIN-metadata_",currentDate,".csv")
csvFileName_2 <- paste0("./Output/CRISTIN_ID-ISBNs_",currentDate,".csv")
csvFileName_3 <- paste0("./Output/CRISTIN-ISBNs_",currentDate,".csv")

write_csv(books_metadata, csvFileName_1, na = "")
write_csv(ID_ISBN, csvFileName_2, na = "")
write_csv(listISBNs, csvFileName_3, na = "")