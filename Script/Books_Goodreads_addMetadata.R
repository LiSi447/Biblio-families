#This script retrieves metadata from Goodreads on the basis of ISBN

# Call packages -----------------------------------------------------------

library(rvest)
library(tidyverse)

# Import data -------------------------------------------------------------

data <- read_csv("./Output/ISBN-ID_all-and-OCLC_2020-06-17.csv", col_types = cols(.default = "c"))

# Retrieve metadata from Goodreads ----------------------------------------

# Prepare the base file

GRdata <- as.tibble(data$ISBN)
names(GRdata)[1] <- "ISBN"
GRdata$language <- ""
GRdata$edition_raw <- ""
GRdata$year.publisher_raw <- ""

# Define arguments

base_url <- "https://www.goodreads.com/search?q="
isbns <- GRdata$ISBN

# Retrieve metadata (the code below was adjusted for every set of ISBNs)

for (i in 1:1000) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 1")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/nissing_GR_20191203_1.csv", na = "")

Sys.time()
for (i in 1001:2000) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 2")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/missing_GR_20191203_2.csv", na = "")
Sys.time()
for (i in 2001:2227) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 3")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/missing_GR_20191203_3.csv", na = "")
Sys.time()

for (i in 3001:4000) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 4")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/VABB_GR_20190804_4.csv", na = "")
Sys.time()
for (i in 4001:5000) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 5")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/VABB_GR_20190804_5.csv", na = "")
Sys.time()
for (i in 5001:6000) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 6")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/VABB_GR_20190804_6.csv", na = "")
Sys.time()
for (i in 6001:7000) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 7")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/VABB_GR_20190804_7.csv", na = "")
Sys.time()
for (i in 7001:8000) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 8")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/VABB_GR_20190804_8.csv", na = "")
Sys.time()
for (i in 8001:8043) {
  Sys.sleep(runif(1, min=1, max=3))
  url <- paste0(base_url, isbns[i])
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'div.infoBoxRowItem')
  data_html2 <- html_nodes(webpage,'div.row')
  data <- html_text(data_html)
  data2 <- html_text(data_html2)
  GRdata$language[i] <- html_text(data_html)[3]
  GRdata$edition_raw[i] <- html_text(data_html2)[1]
  GRdata$year.publisher_raw[i] <- gsub("\n", "", html_text(data_html2)[2])
}
print("done 9")
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/VABB_GR_20190804_9.csv", na = "")
Sys.time()
GRdata$in.GR <- ifelse(!is.na(GRdata$language), TRUE,
                       ifelse(!is.na(GRdata$edition_raw), TRUE,
                              ifelse(!is.na(GRdata$year.publisher_raw), TRUE, FALSE)))
#write_csv(GRdata, "N:/ADOC/Dienst Onderzoek/ECOOM/Linda/Book study/VABB_GR_20190805_FIN.csv", na = "")


