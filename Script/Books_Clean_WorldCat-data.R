#This file cleans up the WorldCat datasets and generates a dataset with OCLN and ISBN pairs to be checked manually


# Call packages -----------------------------------------------------------

library(tidyverse)
library(stringdist)

# Import data -------------------------------------------------------------

n_col <- max(count.fields("./Raw data/WorldCat/dr38_ds1.txt", sep = "\t"))

worldcat_d1 <- as_tibble(read.table("./Raw data/WorldCat/dr38_ds1.txt",
                                    sep="\t",
                                    fill=TRUE,
                                    header = F,
                                    col.names = c("ISBN", 1:n_col),
                                    colClasses = "character",
                                    fileEncoding = "UTF-8"))

worldcat_d2 <- read_delim("./Raw data/WorldCat/dr38_ds2ed.txt",
                          delim = "\t",
                          col_types = cols(.default = "c"),
                          col_names = FALSE,
                          locale = readr::locale(encoding = "UTF-8"))

ID_ISBN <- read_csv("./Output/ID-ISBNs-Combined_2020-06-17.csv", col_types = cols(.default = "c"))
ID_metadata <- read_csv("./Output/ID-Metadata-Combined_2020-06-17.csv", col_types = cols(.default = "c"))

# Clean up the dataset d2 -------------------------------------------------

# Adjust the WorldCat metadata names

names(worldcat_d2) <- c("ocln", "ISBN", "Title", "Author", "Publisher", "Year", "Edition", "Format", "Language", "Type", "Description")

# 
pats <- c("[:punctuation:]|[:space:]")

ocln_ISBNs <- worldcat_d2 %>% 
  select(ocln, ISBN) %>% 
  filter(!is.na(ISBN)) %>% 
  mutate(N_ISBNs = str_count(ISBN, ";") + 1) %>% 
  separate(ISBN, paste0("ISBN", 1:161)  ,sep = ";") %>% 
  gather(ISBN_NR, ISBN, ISBN1:ISBN161) %>% 
  filter(!is.na(ISBN)) %>% 
  distinct(ocln, ISBN) %>% 
  mutate(
    ISBN = str_remove_all(ISBN, "^[:space:]") 
  ) %>% 
  separate(ISBN, c("ISBN", "ISBN_note"), sep = " ") %>% 
  mutate(
    ISBN = str_remove_all(ISBN, pats),
    ISBN13_check = str_detect(ISBN, "^97[:alnum:]{11}$"),
    ISBN10_check = str_detect(ISBN, "^[:alnum:]{10}$")
  ) %>% 
  filter(
    ISBN10_check == TRUE | ISBN13_check == TRUE
  )

# Transform ISBN10 to ISBN13
ISBN10 <- ocln_ISBNs %>% 
  filter(ISBN10_check == TRUE) %>% 
  select(ISBN, ocln)

ISBN10 <- ISBN10 %>% 
  mutate(
    digit1 = 9,
    digit2 = 7,
    digit3 = 8
  )

for (i in 1:length(ISBN10$ISBN)) {
  out <- as.integer(unlist(strsplit(ISBN10$ISBN[i], "")))
  ISBN10$digit4[i] <-  out[1]
  ISBN10$digit5[i] <-  out[2]
  ISBN10$digit6[i] <-  out[3]
  ISBN10$digit7[i] <-  out[4]
  ISBN10$digit8[i] <-  out[5]
  ISBN10$digit9[i] <-  out[6]
  ISBN10$digit10[i] <-  out[7]
  ISBN10$digit11[i] <-  out[8]
  ISBN10$digit12[i] <-  out[9]
}

ISBN10 <- ISBN10 %>% 
  mutate(
    controldigit = (10 - (digit1 + 3 * digit2 + digit3 + 3 * digit4 + digit5 + 3 * digit6 + digit7 + 3 * digit8 +
                            digit9 + 3 * digit10 + digit11 + 3 *digit12) %% 10) %% 10,
    ISBN13 = paste0(digit1, digit2, digit3, digit4, digit5,
                    digit6, digit7, digit8, digit9, digit10,
                    digit11, digit12, controldigit)
  )

ISBN10 <- ISBN10 %>%
  select(ocln, ISBN, ISBN13)

#Combine the transformed ISBNs to the rest of the OCLN-ISBN list (d2) and deduplicate

ocln_ISBNs_2 <- left_join(ocln_ISBNs, ISBN10, by = c("ocln", "ISBN"))

ocln_ISBN_cleaned <- ocln_ISBNs_2 %>%
  mutate(
    ISBN13_fin = ifelse(!is.na(ISBN13), ISBN13, ISBN)
  ) %>% 
  filter(!is.na(ISBN13_fin)) %>% 
  select(ocln, ISBN13_fin) %>% 
  distinct(ocln, ISBN13_fin)

# Clean up the dataset d1 ---------------------------------------------------

# Create a function that paste only values from non-empty cells
paste_noNA <- function(x, sep =", ") {
  gsub(", " , sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) ) }

sep = ";"

# Gather all ocln codes in one cell

worldcat_d1A <- worldcat_d1 %>% 
  mutate(
    ocln = apply( worldcat_d1[ , c(2:2145) ] , 1 , paste_noNA , sep=sep)
  ) %>% 
  filter(ocln != "") %>% 
  select(ISBN, ocln)

# Rearrange the data file so that each row corresponds to a unique ISBN

worldcat_d1A_wide <- worldcat_d1A %>% 
  group_by(ISBN) %>% 
  mutate(counter.ISBN = row_number()) %>% 
  ungroup() %>% 
  spread(counter.ISBN, ocln) 

# Gather all oclns in one cell

worldcat_d1AA_wide <- worldcat_d1A_wide %>% 
  mutate(
    ocln = apply ( worldcat_d1A_wide[ , c(2:47) ] , 1,  paste_noNA , sep=sep)
  ) %>% 
  select(ISBN, ocln)

# Rearrange 

worldcat_d1B <- worldcat_d1AA_wide %>% 
  separate(ocln, c(paste0("N",1:2144)), sep = ";")

worldcat_d1C<- worldcat_d1B %>% 
  gather (ocln_nr, ocln, N1:N2144, na.rm = TRUE) %>%
  distinct(ISBN, ocln, .keep_all = TRUE) %>% 
  select(-ocln_nr)

# Prepare a dataset for checking ocln-ISBN links ------------------------------

# Add local IDs by ISBN
ISBN_OCLC_localIDs <- left_join(worldcat_d1C, ID_ISBN, by = "ISBN")

ISBN_OCLC_localIDs <- ISBN_OCLC_localIDs %>% 
  filter(!is.na(localID)) %>% 
  group_by(ISBN) %>% 
  mutate(
    nISBNS = row_number() #identify ISBNs that appear with multiple local IDs
  ) %>% 
  ungroup()

# Adjust column names
names(ID_metadata)[2:6] <- paste0("National_",names(ID_metadata)[2:6])
names(worldcat_d2)[3:11] <- paste0("OCLC_",names(worldcat_d2)[3:11])

# Add national DB metadata

check_ISBN_OCLC <- left_join(ISBN_OCLC_localIDs, ID_metadata, by = c("localID", "Source"))

# Add WorldCat metadata

worldcat_d2 <- worldcat_d2 %>% select(-ISBN)
check_ISBN_OCLC2 <- left_join(check_ISBN_OCLC, worldcat_d2, by = "ocln")

# Clean up the metadata, calculate Jaccard distances, and identify correct ocln-ISBN pairs

check_ISBN_OCLC2 <- check_ISBN_OCLC2 %>% 
  mutate(
    National_Title = tolower(gsub("[^0-9A-Za-z///' ]", "", National_Title)),
    National_Author = tolower(gsub("[^0-9A-Za-z///' ]", "", National_Author)),
    National_Year = tolower(gsub("[^0-9A-Za-z///' ]", "", National_Year)),
    National_Publisher = tolower(gsub("[^0-9A-Za-z///' ]", "", National_Publisher)),
    National_Language = tolower(gsub("[^0-9A-Za-z///' ]", "", National_Language)),
    
    OCLC_Title = tolower(gsub("[^0-9A-Za-z///' ]", "", OCLC_Title)),
    OCLC_Author = tolower(gsub("[^0-9A-Za-z///' ]", "", OCLC_Author)),
    OCLC_Year = tolower(gsub("[^0-9A-Za-z///' ]", "", OCLC_Year)),
    OCLC_Publisher = tolower(gsub("[^0-9A-Za-z///' ]", "", OCLC_Publisher)),
    OCLC_Language = tolower(gsub("[^0-9A-Za-z///' ]", "", OCLC_Language)),
    
    Year_distance = stringdist(OCLC_Year, National_Year, method = "jaccard"),
    Title_distance = stringdist(OCLC_Title, National_Title, method = "jaccard"),
    Author_distance = stringdist(OCLC_Author, National_Author, method = "jaccard"),
    Publisher_distance = stringdist(OCLC_Publisher, National_Publisher, method = "jaccard"),
    Language_distance = stringdist(OCLC_Language, National_Language, method = "jaccard"),
    
    OCLC_correct = ifelse(Title_distance <= 0.5, "YES",
                          ifelse(Title_distance > 0.5 & Author_distance >= 0.8, "NO", "TOCHECK"))
  )

# Rearrange columns

check_ISBN_OCLC2 <- check_ISBN_OCLC2 %>% 
  select(OCLC_correct,
         ISBN, ocln, localID, Source, nISBNS,
         National_Title, OCLC_Title,
         National_Author, OCLC_Author,
         National_Year, OCLC_Year,
         National_Publisher, OCLC_Publisher,
         National_Language, OCLC_Language,
         OCLC_Edition, OCLC_Format, OCLC_Type, OCLC_Description,
         Year_distance, Title_distance, Author_distance, Publisher_distance, Language_distance)
                             
# Export datasets ---------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/worldcat_d1_Cleaned_",currentDate,".csv")
csvFileName_2 <- paste0("./Output/ocln-ISBN_tocheck_",currentDate,".csv")
csvFileName_3 <- paste0("./Output/worldcat_d2_Cleaned_",currentDate,".csv")

write_csv(worldcat_d1C, csvFileName_1, na = "") #this was not exported before
write_csv(check_ISBN_OCLC2, csvFileName_2, na = "") #file that was generated earlier: ISBN-OCLCtocheck_20190730.csv
write_csv(ocln_ISBN_cleaned, csvFileName_3, na = "") #file before: worldcat_d2_cleaned_20190807.csv