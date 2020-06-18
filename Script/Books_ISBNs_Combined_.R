#This generates a combined list of standardised ISBNs (ISBN10 transformed to ISBN13)

# Call package

library(tidyverse)

# Import data -------------------------------------------------------------

#Note: file names that were used to generate the list of ISBNS sent to OCLC: "COBISS_ISBNs_20190712.csv"",
#"CROSBI_ISBNs_20190704.csv", "VABB_ISBNs_20190710.csv", "VABB-notPR_ISBNs_20190712.csv", "CRISTIN_ISBNs_20190712.csv"

COBISS <- read_csv("./Output/COBISS-ISBNs_2020-06-17.csv", col_types = cols(.default = "c"))

CROSBI <- read_csv("./Output/CROSBI-ISBNs_2020-06-09.csv", col_types = cols(.default = "c"))

VABB <- read_csv("./Output/VABB-PR-ISBNs_2020-06-08.csv", col_types = cols(.default = "c"))

VABB_npr <- read_csv("./Output/VABB-NPR-ISBNs_2020-06-04.csv", col_types = cols(.default = "c"))

CRISTIN <- read_csv("./Output/CRISTIN-ISBNs_2020-06-04.csv", col_types = cols(.default = "c")) 

# Combine and clean-up lists -----------------------------------------------------------

names(CRISTIN)[1] <- "ISBN"
ISBNs <- rbind(COBISS, CROSBI, VABB, VABB_npr, CRISTIN)

pats <- c("-| ")
ISBNs$ISBN <- str_replace_all(ISBNs$ISBN, pats, "")

ISBNs <- ISBNs %>% 
  mutate(
    ISBN13 = ifelse(str_detect(ISBNs$ISBN, "^97[:alnum:]{11}$"), "YES", "NO"),
    ISBN10 = ifelse(str_detect(ISBNs$ISBN, "^[:alnum:]{10}$"), "YES", "NO")
  ) %>% 
  distinct(ISBN, .keep_all = TRUE)

# Transform ISBN10 to ISBN13 ----------------------------------------------

ISBN10data <- ISBNs %>% 
  filter(ISBN10 == "YES") %>%
  select(ISBN)

names(ISBN10data)[1] <- "ISBN10"

ISBN10data <- ISBN10data %>% 
  mutate(
    digit1 = 9,
    digit2 = 7,
    digit3 = 8
  )

for (i in 1:4399) {
  out <- as.integer(unlist(strsplit(ISBN10data$ISBN10[i], "")))
  ISBN10data$digit4[i] <-  out[1]
  ISBN10data$digit5[i] <-  out[2]
  ISBN10data$digit6[i] <-  out[3]
  ISBN10data$digit7[i] <-  out[4]
  ISBN10data$digit8[i] <-  out[5]
  ISBN10data$digit9[i] <-  out[6]
  ISBN10data$digit10[i] <-  out[7]
  ISBN10data$digit11[i] <-  out[8]
  ISBN10data$digit12[i] <-  out[9]
}

ISBN10data <- ISBN10data %>% 
  mutate(
    controldigit = (10 - (ISBN10data$digit1 + 3 * ISBN10data$digit2 + ISBN10data$digit3 + 3 * ISBN10data$digit4 + ISBN10data$digit5 + 3 * ISBN10data$digit6 + ISBN10data$digit7 + 3 * ISBN10data$digit8 +
                         ISBN10data$digit9 + 3 * ISBN10data$digit10 + ISBN10data$digit11 + 3 *ISBN10data$digit12) %% 10) %% 10,
    ISBN13 = paste0(digit1, digit2, digit3, digit4, digit5,
                   digit6, digit7, digit8, digit9, digit10,
                   digit11, digit12, controldigit)
  ) %>%
  select(ISBN10, ISBN13)

# Generate a list of unique ISBN13s ---------------------------------------

ISBNs$ISBN13_cleaned <- ISBN10data$ISBN13[match(ISBNs$ISBN, ISBN10data$ISBN10)]

ISBNs$ISBN13_fin <- ifelse(!is.na(ISBNs$ISBN13_cleaned),
                                       ISBNs$ISBN13_cleaned,
                                       ISBNs$ISBN)

ISBNs_distinct <- ISBNs %>% 
  distinct(ISBN13_fin) %>% 
  select(ISBN13_fin)


# Remove inaccurate ISBNs -------------------------------------------------

ISBNs_distinct <- ISBNs_distinct %>% filter(!ISBN13_fin %in% c("9780000000001","9780000000002"))

# Export the list of ISBNs ------------------------------------------------

currentDate <- Sys.Date()
csvFileName_1 <- paste0("./Output/ISBNs_",currentDate,".csv")
write_csv(ISBNs_distinct, csvFileName_1, na = "")

#Note: file that was sent to OCLC: "Sile_forOCLC_ISBNs_20190712.csv"