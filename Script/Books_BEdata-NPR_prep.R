#! This generates datasets for the non peer-reviewed monographs from VABB

## call package

library(tidyverse)

# import data
VABBdata_FULL <- read_csv2("./Raw data/VABB/190521_overzichtsrapport.csv", 
                           col_types = cols(.default = "c")) # VABB 9

VABBdata_numbers <- read_csv2("./Raw data/VABB/vabb9_isbn.csv",
                              col_types = cols(.default = "c"))

cleaned_ISBNs <- read_csv("./Raw data/forAnalysis_ISBNs_20190729.csv",
                          col_types = cols(.default = "c"))


#Delineate time span and select only monographs

years <- c(2011:2017)
books <- filter(VABBdata_FULL, pubyear %in% years & `VABB-publicatietype` %in% "VABB-2")

#Add ISBN numbers

names(VABBdata_numbers)[1] <- "Loi"

pats <- c("-| ")
VABBdata_numbers$ISBN <- str_replace_all(VABBdata_numbers$number_nr, pats, "")

VABBdata_numbers <- VABBdata_numbers %>% 
  mutate(
    ISBN13 = ifelse(str_detect(VABBdata_numbers$ISBN, "^97[:alnum:]{11}$"), "YES", "NO"),
    ISBN10 = ifelse(str_detect(VABBdata_numbers$ISBN, "^[:alnum:]{10}$"), "YES", "NO")
  )

VABBdata_numbers_cleaned <- VABBdata_numbers %>%
  filter(VABBdata_numbers$ISBN13 == "YES" | VABBdata_numbers$ISBN10 == "YES") %>% # removes 65 entries that don't have a valid ISBN
  select(Loi, ISBN)

VABBdata_numbers_cleaned <- VABBdata_numbers_cleaned %>% 
  group_by(Loi) %>% 
  mutate(counter_Loi = row_number()) %>% 
  ungroup()

VABBdata_numbers_wide <- spread(VABBdata_numbers_cleaned, counter_Loi, ISBN)

new.names <- c("Loi", paste0("ISBN", 1:max(VABBdata_numbers_cleaned$counter_Loi)))
names(VABBdata_numbers_wide) <- new.names

books <- left_join(books, VABBdata_numbers_wide, by = "Loi")

#Remove records with missing ISBNs
books <- filter(books, !is.na(books$ISBN1)) # 2477 records removed

#Delineate non peer-reviewed publications

PRstatus_variables <- rev(names(books)[8:17])

books$peer.reviewed <- NA

for (i in 1:nrow(books)) {
  
  for (var in PRstatus_variables) {
    if (!is.na(books$peer.reviewed[i])) {
      break
    }
    if (is.na(books[var][i,])) {
      next
    }
    if (grepl("1,*", books[var][i,])) {
      books$peer.reviewed[i] <- "YES"
    } else if (grepl("in WOS \\(bof\\)", books[var][i,])) {
      books$peer.reviewed[i] <- "YES"
    } else {
      books$peer.reviewed[i] <- "NO"
    }
  }
}

books2 <- books %>%  filter(peer.reviewed == "NO")


#Delineate SSH using organisational classification

org_vars_SSH <- names(books)[c(18:31, 33:36)] # select org clas vars that refer to SSH; irc(a::irc.85) is ommitted
notSSH.org <- books2 %>% filter_at(org_vars_SSH, all_vars(is.na(.)))
books2$ORG_SSH <- !(books2$Loi %in% notSSH.org$Loi)

books3 <- books2 %>% filter(ORG_SSH == TRUE) # 1 record removed

#select data from universities

books4 <- filter(books3,                       `lm(vabb-ua)` %in% "Ja" |
                   `lm(vabb-kul)` %in% "Ja" |
                   `lm(vabb-ug)` %in% "Ja" |
                   `lm(vabb-uh)` %in% "Ja" |
                   `lm(vabb-vub)` %in% "Ja")

#extract ISBNs
ISBN.vars <- paste0("ISBN", 1:11)
listISBNs <- books4 %>% 
  select(Loi, ISBN.vars) %>%
  gather("N-ISBN", "ISBN", ISBN.vars) %>% 
  select(ISBN) %>% 
  distinct(ISBN)

#prepare the dataset for analysis (v.1) Loi and ISBNs
ID_ISBN <- books4 %>% 
  select(Loi, ISBN.vars) %>% 
  gather("N-ISBN", "ISBN.raw", ISBN.vars, na.rm = TRUE)
ID_ISBN$ISBN13 <- cleaned_ISBNs$ISBN13_fin[match(ID_ISBN$ISBN.raw, cleaned_ISBNs$ISBN)] #assigned ISBN13 acquired by transforming ISBN10


ID_ISBN <- ID_ISBN %>% select(ISBN13, Loi)

names(ID_ISBN) <- c("ISBN", "localID")

ID_ISBN$Source <- "VABB_NPR"

#prepare the dataset for analysis (v.2)

books_authors <- books4 %>% 
  select(Loi, Omschrijving) %>% 
  separate(Omschrijving, c("title", "author"), sep = " / ")

books_metadata <- books4 %>% 
  select(Loi, pubyear, utitle, lg)
books_metadata <- left_join(books_metadata, books_authors, by = "Loi")

books_metadata <- books_metadata %>% mutate(Source = "VABB_NPR")

names(books_metadata) <- c("localID", "Year", "Publisher", "Language", "Title", "Author", "Source")

books_metadata <- books_metadata %>% select(localID, Title, Author, Year, Publisher, Language, Source)

#export datasets

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/VABB-NPR-metadata_",currentDate,".csv")
csvFileName_2 <- paste0("./Output/VABB-NPR_ID-ISBNs_",currentDate,".csv")
csvFileName_3 <- paste0("./Output/VABB-NPR-ISBNs_",currentDate,".csv")

write_csv(books_metadata, csvFileName_1, na = "")
write_csv(ID_ISBN, csvFileName_2, na = "")
write_csv(listISBNs, csvFileName_3, na = "")