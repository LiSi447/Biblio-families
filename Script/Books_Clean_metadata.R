#This script cleans up the fields 'publisher' and 'language'
# Call packages -----------------------------------------------------------

library(tidyverse)
library(readxl)

# Import data -------------------------------------------------------------

# Import the local ID and metadata dataset

data <- read_csv("./Output/ID-Metadata-Combined_2020-06-17.csv", col_types = cols(.default = "c"))

# Create a smaller working file

data_mini <- data %>% select(localID, Source, Publisher, Language)

# Import the manually cleaned publisher file

publishers <- read_csv2("./Manually cleaned data/Publishers_20191204_cleaned.csv", col_types = cols(.default = "c"))

# Import the manually cleaned language file

languages <- read_csv2("./Manually cleaned data/Language_20191204_cleaned.csv", col_types = cols(.default = "c"))

# Import the manually validated files with records that contain 6 or more ISBNs

book_categories_1 <- read_csv2("./Manually cleaned data/manyISBNs_coded_20190820.csv", col_types = cols(.default = "c"))
book_categories_2 <- read_csv2("./Manually cleaned data/missing-info_20191204_edited.csv", col_types = cols(.default = "c"))
missing_bookcats <- read_excel("./Manually cleaned data/Missing_bookcats_20200615.xlsx",
                               sheet = "Sheet3",
                               col_types = "text")

# Add info on COBISS authors

cobiss_authors <- read_csv("./Raw data/COBISS/cobiss_data_authors_fromRaf.csv", col_types = cols(.default = "c"))

# Clean up the publisher names --------------------------------------------

#Generate a file with raw publisher titles

publishers_raw <- data %>% 
  group_by(Publisher) %>% 
  count() %>% 
  ungroup() #%>% 
  #mutate(publisherID = paste("publisher::", sprintf('%0.7d', 1:5320), sep=""))

# Combine with the manually cleaned file
  
publishers_cleaned <- left_join(publishers_raw, publishers, by = "Publisher")

#24 publisher have missing info -- all with 2 localIDs --> assign to "Other"

publishers_cleaned <- publishers_cleaned %>% 
  mutate(
    Publisher_cleaned = ifelse(is.na(Publisher_cleaned) & n.x < 5, "Other", Publisher_cleaned)
  )

# Adjust publisher names that due to encoding errors have been wrongly imported

publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID == "publisher::0000016")] <- "Casnik Finance"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000038", 
                                                         "publisher::0000039",
                                                         "publisher::0000045"))] <- "Skolska knjiga"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000135"))] <- "Adami"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000251", 
                                                         "publisher::0000252"))] <- "Andragoski center Republike Slovenije"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000260"))] <- "Antibarbarus"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000286",
                                                         "publisher::0000296"))] <- "Arheoloski muzej"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000290", 
                                                         "publisher::0000291",
                                                         "publisher::0000294"))] <- "Arheoloski muzej u Zagrebu"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000338"))] <- "Artresor naklada"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000558",
                                                         "publisher::0000559"))] <- "Cankarjeva zalozba"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000589"))] <- "Centar za zenske studije"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000608",
                                                         "publisher::0000609"))] <- "Centar za politoloska istrazivanja"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000723"))] <- "Columbia University Press"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000790"))] <- "De Gruyter"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000881"))] <- "Disput"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000961"))] <- "Drustvo povjesnicara umjetnosti Hrvatske"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001210"))] <- "Encore"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001285",
                                                         "publisher::0001285",
                                                         "publisher::0001286",
                                                         "publisher::0001287",
                                                         "publisher::0001288",
                                                         "publisher::0001289",
                                                         "publisher::0001290"))] <- "Ex Ante Akademisk forlag"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001292"))] <- "Ex Libris"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001358"))] <- "Fakultet politickih znanosti"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001384"))] <- "Fakulteta za druzbene vede"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001470",
                                                         "publisher::0001473",
                                                         "publisher::0001479"))] <- "FF Press"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001529"))] <- "Filozofski fakultet Sveucilista u Splitu"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001563",
                                                         "publisher::0001566"))] <- "Filozofsko-teoloski institut Druzbe Isusove"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001692"))] <- "Glas Koncila"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001707",
                                                         "publisher::0001710",
                                                         "publisher::0001715",
                                                         "publisher::0001716"))] <- "Golden Marketing"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001799"))] <- "GV zalozba"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001947",
                                                         "publisher::0001948"))] <- "Hrvatska sveucilisna naklada"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001971",
                                                         "publisher::0001972"))] <- "Hrvatska zajednica racunovoda i financijskih djelatnika"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0001978"))] <- "Hrvatski drzavni arhiv"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002045",
                                                         "publisher::0002046",
                                                         "publisher::0002047"))] <- "Hrvatsko filolosko drustvo"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002048",
                                                         "publisher::0002050"))] <- "Hrvatsko filozofsko drustvo"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002051",
                                                         "publisher::0002052",
                                                         "publisher::0002053"))] <- "Hrvatsko futurolosko drustvo"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002061"))] <-"Hrvatsko muzikolosko drustvo"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002109",
                                                         "publisher::0002112",
                                                         "publisher::0002053"))] <- "Ibis grafika"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002181",
                                                         "publisher::0002183"))] <- "Institut drustvenih znanosti Ivo Pilar"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002222"))] <- "Institut za drustvena istrazivanja"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002209"))] <- "Institut za arheologiju"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002224",
                                                         "publisher::0002225"))] <- "Institut za drustvena istrazivanja u Zagrebu"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002257",
                                                         "publisher::0002258"))] <- "Institut za medunarodne odnose"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002254"))] <- "Institut za lokalno samoupravo in javna narocila"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002346",
                                                         "publisher::0002347",
                                                         "publisher::0002348",
                                                         "publisher::0002349"))] <- "Institutum Studiorum Humanitatis - Fakulteta za podiplomski humanisticni studij"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002442",
                                                         "publisher::0002443",
                                                         "publisher::0002444"))] <- "IUS Software, GV zalozba"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002458"))] <- "Izdavacki centar Rijeka"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002603",
                                                         "publisher::0002605"))] <- "Knjizevni krug"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002612"))] <- "Knjizevni krug Split"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002663",
                                                         "publisher::0002664",
                                                         "publisher::0002665"))] <- "Krscanska sadasnjost"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002679"))] <- "KruZak"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002687"))] <- "KUD Logos"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002738"))] <- "Laika"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002743",
                                                         "publisher::0002752",
                                                         "publisher::0002753"))] <- "Lambert Academic Publishing"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002797"))] <- "Leykam"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002828",
                                                         "publisher::0002831"))] <- "Lit"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002876"))] <- "Magyar Nemzetis?gi Muvelod?si Int?zet"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002933",
                                                         "publisher::0002934",
                                                         "publisher::0002936",
                                                         "publisher::0002939",
                                                         "publisher::0002947"))] <- "Matica Hrvatska"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0002990",
                                                         "publisher::0002991"))] <- "Meandarmedia"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003042",
                                                         "publisher::0003043"))] <- "Meritum"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003050"))] <- "Mestno gledalisce ljubljansko"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003196",
                                                         "publisher::0003197",
                                                         "publisher::0003198",
                                                         "publisher::0003200"))] <- "Muzej hrvatskih arheoloskih spomenika"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003260",
                                                         "publisher::0003262",
                                                         "publisher::0003264"))] <- "Naklada Boskovic"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003266"))] <- "Naklada Breza"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003273",
                                                         "publisher::0003275"))] <-"Naklada Jurcic"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003286"))] <-"Naklada Slap"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003293"))] <-"Nakladni zavod Globus"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003387"))] <-"Novi informator"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003391"))] <-"Novus Forlag"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003511"))] <- "Orion art"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003577"))] <- "Pearson Education"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003688"))] <-  "Ponte alle grazie"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003741"))] <-  "Pravni fakultet Sveucilista u Rijeci"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003744",
                                                         "publisher::0003753"))] <-  "Pravni fakultet Sveucilista u Zagrebu"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003752"))] <-  "Pravni fakultet u Splitu"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0003951"))] <-  "Sage"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004068",
                                                         "publisher::0004071"))] <-  "Slavisticno drustvo"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004161"))] <-  "Sola za ravnatelje"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004255"))] <-  "Studia Humanitatis"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004251"))] <-  "Studentska zalozba"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004260",
                                                         "publisher::0004262"))] <-  "Studijski center za narodno spravo"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004293",
                                                         "publisher::0004294"))] <- "Sveucilisna knjizara"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004312",
                                                         "publisher::0004313",
                                                         "publisher::0004314"))] <- "Sveuciliste Jurja Dobrile"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004321"))] <- "Sveuciliste Sjever"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004344"))] <- "Sveuciliste Zadru"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004394"))] <- "T & T Clark"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004449"))] <- "MIT Press"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004473",
                                                         "publisher::0004474",
                                                         "publisher::0004478"))] <- "TIM press"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004548"))] <- "Uciteljski fakultet Sveucilista u Zagrebu"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004601"))] <- "Umjetniski paviljon"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004842",
                                                         "publisher::0004843",
                                                         "publisher::0004844",
                                                         "publisher::0004845",
                                                         "publisher::0004846"))] <- "VDM Verlag Dr. Muller"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004854"))] <- "Vega"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004879"))] <- "Verlag Dr. Kovac"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0004954",
                                                         "publisher::0004955",
                                                         "publisher::0004956",
                                                         "publisher::0004957",
                                                         "publisher::0004958",
                                                         "publisher::0004959"))] <- "Vlastita naklada / self published"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0005048",
                                                         "publisher::0005049"))] <- "Wydawnictwo krytyki politycznej"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0005102"))] <- "Zalozba ZRC SAZU"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0005226",
                                                         "publisher::0005245",
                                                         "publisher::0005246",
                                                         "publisher::0005247"))] <- "Znanstvena zalozba Filozofske fakultete"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0005271"))] <- "Znanstveno in publicisticno sredisce"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0005298"))] <- "Zupnija"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0005305"))] <- "Zveza drustev Slavisticno drustvo Slovenije"
publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0005310",
                                                         "publisher::0005312"))] <- "Zveza racunovodij, financnikov in revizorjev Slovenije"

publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000328", 
                                                         "publisher::0000332",
                                                         "publisher::0000442",
                                                         "publisher::0000583",
                                                         "publisher::0000877",
                                                         "publisher::0001171",
                                                         "publisher::0001193",
                                                         "publisher::0001227",
                                                         "publisher::0001304",
                                                         "publisher::0001590",
                                                         "publisher::0001993",
                                                         "publisher::0002010",
                                                         "publisher::0002138",
                                                         "publisher::0002553",
                                                         "publisher::0002725",
                                                         "publisher::0002971",
                                                         "publisher::0003202",
                                                         "publisher::0003401",
                                                         "publisher::0003475",
                                                         "publisher::0003552",
                                                         "publisher::0003798",
                                                         "publisher::0003807",
                                                         "publisher::0003813",
                                                         "publisher::0003888",
                                                         "publisher::0003892",
                                                         "publisher::0003934",
                                                         "publisher::0004061",
                                                         "publisher::0004153",
                                                         "publisher::0004210",
                                                         "publisher::0004232",
                                                         "publisher::0004284",
                                                         "publisher::0004462",
                                                         "publisher::0004488",
                                                         "publisher::0004606"))] <- "Other"


publishers_cleaned$Publisher_cleaned[which(publishers_cleaned$ID %in% c("publisher::0000445",
                                                         "publisher::0003118"))] <- "Multiple publishers"

# Add local IDs and Source

publishers_withLocalID  <- left_join(publishers_cleaned, data_mini, by = "Publisher")

publishers_withLocalID <- publishers_withLocalID %>% 
  select(localID, Source, Publisher_cleaned)

# Clean up language -------------------------------------------------------

#Generate a file with raw publisher titles

languages_raw <- data %>% 
  group_by(Language) %>% 
  count() %>% 
  ungroup()

# Combine with the manually cleaned file

languages_cleaned <- left_join(languages_raw, languages, by = "Language")

# 4 language fields have missing info --> add info

languages_cleaned$Cleaned[which(languages_cleaned$Language =="per - novoperzijski")] <- "Persian"
languages_cleaned$Cleaned[which(languages_cleaned$Language =="MarkoStevanovic(autor)")] <- "missing"
languages_cleaned$Cleaned[which(languages_cleaned$Language =="VesnaVašicek(autor)")] <- "missing"

languages_cleaned$Cleaned <- ifelse(is.na(languages_cleaned$Cleaned), "missing", languages_cleaned$Cleaned)

# Add local IDs and Source

languages_withLocalID <- left_join(languages_cleaned, data_mini, by = "Language")

languages_withLocalID <- languages_withLocalID %>% 
  select(localID, Source, Cleaned)

names(languages_withLocalID)[3] <- "Language_cleaned"


# Add info on book categories ---------------------------------------------

# Prepare dataset 1
book_categories_1A <- book_categories_1 %>% 
  select(-(15:726))

book_categories_1_mini <- book_categories_1A %>% 
  select(book_category, title, author, cobissID, crosbiID, cristinID, vabbID, vabb_pr) %>% 
  gather(Source, localID, cobissID:vabbID, na.rm = TRUE) %>% 
  mutate(
    Source = fct_recode(as.factor(Source), COBISS = "cobissID", CRISTIN = "cristinID", CROSBI = "crosbiID", VABB = "vabbID"),
    Source_cleaned = ifelse(is.na(vabb_pr), as.character(Source),
                            ifelse((vabb_pr == "YES" & Source == "VABB"), "VABB_PR",
                                   ifelse((vabb_pr == "NO" & Source == "VABB"), "VABB_NPR", as.character(Source))))
) %>% 
  select(localID, Source_cleaned, book_category)

names(book_categories_1_mini)[2] <- "Source"

# Prepare dataset 2
book_categories_2_mini <- book_categories_2 %>%
  select(localID, Source, book_category)

# Combine both datasets

book_categories.OLD <- rbind(book_categories_1_mini, book_categories_2_mini)

book_categories.OLD <- book_categories.OLD %>% 
  filter(!is.na(book_category)) %>% 
  distinct(localID, Source, .keep_all = TRUE)

# Prepare dataset 3
book_categories <- missing_bookcats %>% 
  select(localID, Source, book_category_FIN)

names(book_categories)[3] <- "book_category"

# Add info on COBISS authors ----------------------------------------------

cobiss_authors_select <- cobiss_authors %>% 
  filter(!is.na(res_code) & contrib_code == "070") %>% 
  select(cobiss_id)

# Combine in one dataset --------------------------------------------------

metadata_cleaned <- left_join(languages_withLocalID, publishers_withLocalID, by = c("Source", "localID"))
metadata_cleaned <- left_join(metadata_cleaned, book_categories, by = c("Source", "localID"))
metadata_cleaned <- metadata_cleaned %>% 
  mutate(COBISS_author = ifelse(Source == "COBISS", localID %in% cobiss_authors_select$cobiss_id, NA))

metadata_cleaned <- metadata_cleaned  %>% 
  distinct(localID, Source, Language_cleaned, Publisher_cleaned, book_category, .keep_all = TRUE)

# Export data -------------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/books_metadata_cleaned_",currentDate,".csv")

write_csv(metadata_cleaned, csvFileName_1, na = "")
