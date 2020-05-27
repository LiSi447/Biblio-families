# This adds from web info on language to CROSBI data

## call package

library(tidyverse)
library(rvest)
library(RCurl)

# import data
data <- read_csv2("./Raw data/CROSBI/CROSBI_books.csv",
                             col_types = cols(.default = "c"),
                             locale = readr::locale(encoding = "UTF-8"))

# extract CROSBI internal identifier

data$crosbiID.local <- str_replace_all(data$URL, "https://www.bib.irb.hr/", "")

# create a working dataset

crosbi.ID <- data %>% 
  select(crosbiID.local, URL) %>% 
  filter(!is.na(crosbiID.local))


# Retrieve information on language ----------------------------------------

# Validate URLs
URL <- crosbi.ID$URL

crosbi.ID$valid.url <- NA
for (i in 1:nrow(crosbi.ID)) { 
  crosbi.ID$valid.url[i] <- url.exists(URL[i]) # validate URLs
  Sys.sleep(1)
}

# Select only valid URLs

crosbi.ID2 <- crosbi.ID %>% filter(valid.url == TRUE) 

# Retrieve language info (takes about 5 hours)

crosbi.ID$language <- NA
for (i in 1:nrow(crosbi.ID)) {
  Sys.sleep(runif(1, min=1, max=3))
  webpage <- read_html(URL[i])
  data_html <- html_nodes(webpage,'p.item-label')
  data.temp <- str_replace_all(html_text(data_html), "[\\n]", "")
  crosbi.ID$language[i] <- str_replace_all(data.temp[11], "        Izvorni jezik                ", "")
  crosbi.ID$language[i] <- str_replace_all(crosbi.ID$language[i], " ", "")
}

crosbi.ID <- crosbi.ID %>% select(crosbiID.local, language)

# export to csv

currentDate <- Sys.Date()
csvFileName <- paste0("./Output/CROSBI-ID-language_",currentDate,".csv")
write_csv(crosbi.ID, csvFileName, na = "")