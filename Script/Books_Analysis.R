#This script generates all the analysis necessary for tables in the manuscript

# Load packages -----------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(extrafont)

# Import data -------------------------------------------------------------

data_ISBN <- read_csv("./Output/dataset_A_ISBN_2020-06-17.csv", col_types = cols(.default = "c"))
data_localID <- read_csv("./Output/dataset_B_localID_2020-06-17.csv", col_types = cols(.default = "c"))
data_workID <- read_csv("./Output/dataset_C_workID_2020-06-17.csv", col_types = cols(.default = "c"))

# 1: Overview of the datasets and metadata enrichment exercise results --------

# Records in total
records.in.national.DB <- data_localID %>% 
  distinct(localID, Source, .keep_all = TRUE) %>% 
  group_by(Source) %>% 
  count() %>% 
  mutate(localID.n = n) %>% 
  select(-n)

ISBNs.in.national.DB <- data_ISBN %>% 
  distinct(ISBN, Source, .keep_all = TRUE) %>% 
  filter(ISBN.national == TRUE) %>% 
  group_by(Source) %>% count() %>% 
  mutate(ISBN.n = n) %>% 
  select(-n)

workIDs.in.national.DB <- data_workID %>% 
  distinct(workID, .keep_all = TRUE) %>% 
  group_by(Source) %>% 
  count() %>% 
  mutate(workID.n = n) %>% 
  select(-n)

total.in.national.DB <- cbind(records.in.national.DB, ISBNs.in.national.DB, workIDs.in.national.DB)

total.in.national.DB <- total.in.national.DB %>% select(-`Source...3`, -`Source...5`)

a <- count(data_localID %>% distinct(localID, .keep_all = TRUE)) 
b <- count(data_ISBN %>% distinct(ISBN, .keep_all = TRUE) %>% filter(ISBN.national == TRUE))
c <- count(data_workID %>% distinct(workID, .keep_all = TRUE))

total.in.national.DB[6,] <- c("Total", a, b, c)
  
# Records in WorldCat

records.in.WC<- data_localID %>% 
  filter(in.WC == TRUE) %>% 
  group_by(Source) %>% 
  count() %>% 
  mutate(localID.n = n) %>% 
  select(-n)

ISBNs.in.WC <- data_ISBN %>% 
  filter(in.WC == TRUE & ISBN.national == TRUE) %>% 
  group_by(Source) %>% count() %>% 
  mutate(ISBN.n = n) %>% 
  select(-n)

workIDs.in.WC<- data_workID %>% 
  filter(in.WC == TRUE) %>% 
  group_by(Source) %>% 
  count() %>% 
  mutate(workID.n = n) %>% 
  select(-n)

total.in.WC<- cbind(records.in.WC, ISBNs.in.WC, workIDs.in.WC)

total.in.WC <- total.in.WC %>% select(-`Source...3`, -`Source...5`)

a.WC <- count(data_localID %>% distinct(localID, .keep_all = TRUE) %>% filter(in.WC == TRUE))
b.WC <- count(data_ISBN %>% distinct(ISBN, .keep_all = TRUE) %>% filter(in.WC == TRUE & ISBN.national == TRUE))
c.WC <- count(data_workID %>% distinct(workID, .keep_all = TRUE) %>% filter(in.WC == TRUE))

total.in.WC[6,] <- c("Total", a.WC, b.WC, c.WC)

# Additional ISBNs in WC

extra.ISBNs.in.WC <- data_ISBN %>% 
  filter(from.WC == TRUE & ISBN.national == FALSE) %>% 
  group_by(Source) %>% count() %>% 
  mutate(ISBN.n = n) %>% 
  select(-n)

a.WC.extra <- count(data_ISBN %>% distinct(ISBN, .keep_all = TRUE) %>% filter(from.WC == TRUE & ISBN.national == FALSE))

extra.ISBNs.in.WC[6,] <- c("Total", a.WC.extra)

# ISBNs in total

ISBNs.in.total <- data_ISBN %>% 
  group_by(Source) %>% count() %>% 
  mutate(ISBN.n = n) %>% 
  select(-n)
  
a.ISBN <- count(data_ISBN %>% distinct(ISBN, .keep_all = TRUE))

ISBNs.in.total[6,] <- c("Total", a.ISBN)

# In Goodreads
records.in.GR<- data_localID %>% 
  filter(in.GR == TRUE) %>% 
  group_by(Source) %>% 
  count() %>% 
  mutate(localID.n = n) %>% 
  select(-n)

ISBNs.in.GR <- data_ISBN %>% 
  filter(in.GR == TRUE) %>% 
  group_by(Source) %>% count() %>% 
  mutate(ISBN.n = n) %>% 
  select(-n)

workIDs.in.GR <- data_workID %>% 
  filter(in.GR == TRUE) %>% 
  group_by(Source) %>% 
  count() %>% 
  mutate(workID.n = n) %>% 
  select(-n)

total.in.GR<- cbind(records.in.GR, ISBNs.in.GR, workIDs.in.GR)

total.in.GR <- total.in.GR %>% select(-`Source...3`, -`Source...5`)

a.GR <- count(data_localID %>% distinct(localID, .keep_all = TRUE) %>% filter(in.GR == TRUE))
b.GR <- count(data_ISBN %>% distinct(ISBN, .keep_all = TRUE) %>% filter(in.GR == TRUE))
c.GR <- count(data_workID %>% distinct(workID, .keep_all = TRUE) %>% filter(in.GR == TRUE))

total.in.GR[6,] <- c("Total", a.GR, b.GR, c.GR)

# 2: Descriptive statistics for the number of ISBNs per work by data source --------

data_workID$n.ISBNs.perWork <- as.integer(data_workID$n.ISBNs.perWork)

n.ISBNs.bySource <- data_workID %>% 
  filter(!is.na(n.ISBNs.perWork)) %>% #Note: 49 records have missing info on the number of ISBNs per work. Checked the previous code but cannot find why it is the case
  group_by(Source) %>% 
  summarise(
    mean = mean(n.ISBNs.perWork),
    median = median(n.ISBNs.perWork),
    sd = sd(n.ISBNs.perWork),
    min = min(n.ISBNs.perWork),
    max = max(n.ISBNs.perWork),
    q75 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.75, 0.75, by = 0.01))),
    q95 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.95, 0.95, by = 0.01))),
    q99 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.99, 0.99, by = 0.01)))
  )

n.ISBNs.bySource[6,] <- data_workID %>% 
  filter(!is.na(n.ISBNs.perWork)) %>% #
  summarise(
    Source = "Total",
    mean = mean(n.ISBNs.perWork),
    median = median(n.ISBNs.perWork),
    sd = sd(n.ISBNs.perWork),
    min = min(n.ISBNs.perWork),
    max = max(n.ISBNs.perWork),
    q75 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.75, 0.75, by = 0.01))),
    q95 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.95, 0.95, by = 0.01))),
    q99 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.99, 0.99, by = 0.01)))
  )


# Generate group labels ---------------------------------------------------


data_workID$book_category <- ifelse(data_workID$workID == "books::0017407", "Misc", data_workID$book_category)
data_localID$book_category <- ifelse(data_localID$workID == "books::0017407", "Misc", data_localID$book_category)

data_workID <- data_workID %>% 
  mutate(
    analysis.group = case_when(
      n.ISBNs.perWork >= 6 & book_category == "multiISBN" ~ "GV-ME",
      n.ISBNs.perWork >= 6 & book_category == "Misc" ~ "Misc",
      in.WC == TRUE & n.ISBNs.perWork < 6 ~ "GV-SE"
    ),
    analysis.group = ifelse(is.na(analysis.group), "GI", analysis.group)
  )

data_localID <- data_localID %>% 
  mutate(
    n.ISBNs.perWork = data_workID$n.ISBNs.perWork[match(workID, data_workID$workID)],
    analysis.group = case_when(
      n.ISBNs.perWork >= 6 & book_category == "multiISBN" ~ "GV-ME",
      n.ISBNs.perWork >= 6 & book_category == "Misc" ~ "Misc",
      in.WC == TRUE & n.ISBNs.perWork < 6 ~ "GV-SE"
    ),
    analysis.group = ifelse(is.na(analysis.group), "GI", analysis.group)
  )

# 3: Number of works per data source by group -----------------------------

  
n.Works.byGroup.bySource <- data_workID %>% 
  group_by(Source, analysis.group) %>% 
  count() %>% 
  spread(analysis.group, n)

totals <- tibble(
  Source = "Total",
  GI = sum(n.Works.byGroup.bySource$GI),
  `GV-ME`= sum(n.Works.byGroup.bySource$`GV-ME`),
  `GV-SE` = sum(n.Works.byGroup.bySource$`GV-SE`),
  Misc = sum(n.Works.byGroup.bySource$Misc)
  )

n.Works.byGroup.bySource2 <- rbind(n.Works.byGroup.bySource, totals)

# 4: Descriptive statistics for the number of ISBNs per work by group --------

n.ISBNs.byGroup <- data_workID %>% 
  filter(!is.na(n.ISBNs.perWork)) %>% #Note: 49 records have missing info on the number of ISBNs per work. Checked the previous code but cannot find why it is the case
  group_by(analysis.group) %>% 
  summarise(
    mean = mean(n.ISBNs.perWork),
    median = median(n.ISBNs.perWork),
    sd = sd(n.ISBNs.perWork),
    min = min(n.ISBNs.perWork),
    max = max(n.ISBNs.perWork),
    q75 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.75, 0.75, by = 0.01))),
    q95 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.95, 0.95, by = 0.01))),
    q99 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.99, 0.99, by = 0.01)))
  )


totals_2 <- data_workID %>% 
  filter(!is.na(n.ISBNs.perWork)) %>% #
  summarise(
    analysis.group = "Total",
    mean = mean(n.ISBNs.perWork),
    median = median(n.ISBNs.perWork),
    sd = sd(n.ISBNs.perWork),
    min = min(n.ISBNs.perWork),
    max = max(n.ISBNs.perWork),
    q75 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.75, 0.75, by = 0.01))),
    q95 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.95, 0.95, by = 0.01))),
    q99 = as.numeric(quantile(n.ISBNs.perWork, probs = seq(0.99, 0.99, by = 0.01)))
  )

n.ISBNs.byGroup <- rbind(n.ISBNs.byGroup, totals_2)

# Create a dataset with only the oldest localID for each workID -----------------------------

data_localID$Year <- as.integer(data_localID$Year)

oldest.localID <- data_localID %>% 
  group_by(workID) %>% 
  arrange(Year) %>% 
  mutate(
    localID.nr = row_number()
  ) %>% 
  ungroup() %>% 
  filter(localID.nr == 1)

# 5: Overview of language use in scholarly monographs by group ------------

Language.byGroup <- oldest.localID %>% 
  group_by(Language_cleaned, analysis.group) %>% 
  count() %>% 
  spread(analysis.group, n)

Language.All <- oldest.localID %>% 
  group_by(Language_cleaned) %>% 
  count()

sum(Language.All$n)

# Overview of publishers by group -----------------------------------------

Publisher.byGroup <- oldest.localID %>% 
  group_by(Publisher_cleaned, analysis.group) %>% 
  count() %>% 
  spread(analysis.group, n) 

Publishers_GV.SE <- Publisher.byGroup %>% 
  arrange(desc(`GV-SE`)) %>% 
  head(10) %>% 
  select(Publisher_cleaned, `GV-SE`)

Publishers_GV.ME <- Publisher.byGroup %>% 
  arrange(desc(`GV-ME`)) %>% 
  #head(10) %>% 
  select(Publisher_cleaned, `GV-ME`) %>% 
  filter(!is.na(`GV-ME`))

Publishers_GI <- Publisher.byGroup %>% 
  arrange(desc(`GI`)) %>% 
  head(10) %>% 
  select(Publisher_cleaned, `GI`)

# Explore records with the most ISBNs ------------------------------------

most.ISBNs.bySource <- data_workID %>% 
  filter(analysis.group == "GV-ME") %>% 
  group_by(Source) %>% 
  arrange(desc(n.ISBNs.perWork)) %>%
  mutate(
    n = row_number()
  ) %>% 
  filter(
    n %in% c(1:4)
  )

most.ISBNs.bySource <- most.ISBNs.bySource %>% left_join(oldest.localID, by = "workID")

# Add info on the share of ISBNs identified in GR

GR.n.ISBNs <- data_ISBN %>% filter(in.GR == TRUE) %>% group_by(workID) %>% count()

most.ISBNs.bySource <- most.ISBNs.bySource %>% 
  mutate(
    n.ISBNs.GR = GR.n.ISBNs$n[match(workID, GR.n.ISBNs$workID)],
    share.ISBNs.GR = n.ISBNs.GR / n.ISBNs.perWork.y
  )

test <- most.ISBNs.bySource %>% select(workID, localID, Source.x, n.ISBNs.GR, n.ISBNs.perWork.y, share.ISBNs.GR)

#Note: These were created manually:
# Group 2 works with the largest number of ISBNs, three works per data source
# Sample list of related ISBNs for the work with the largest number of ISBNs (American Civilization: an Introduction. By David Mauk and John Oakland)


# Plots -------------------------------------------------------------------

theme_academic_ls <- function() {
  theme_classic() +
    theme(
      legend.position = "none",
      text = element_text(family = "Times New Roman"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "grey70"),
      axis.text = element_text(size = 10),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 12, color = "grey30"),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(),
      plot.margin = margin(10, 10, 10, 10, "mm")
    )
}

# ISBNS per work by Source

data_localID$Source <- as.factor(data_localID$Source)
data_localID$Source <- factor(data_localID$Source, levels = c("COBISS", "CRISTIN", "CROSBI", "VABB_PR", "VABB_NPR"))

plot.source <- ggplot(data_localID, aes(x = Source, y = n.ISBNs.perWork)) +
  geom_boxplot(alpha = 0.3) +
  scale_y_continuous(name = "Number of ISBNs per work", 
                     breaks = c(1, 5, 10, 15, 20, 40, 60), limits = c(1, 70)) +
  scale_x_discrete(name = "\n(a) Data source",
                   labels = c("COBISS" = "COBISS", "CRISTIN = CRISTIN", "CROSBI" = "CROSBI", "VABB_NPR" = "VABB not pr", "VABB_PR" = "VABB pr")) +
  theme_academic_ls()

# ISBNs per work by Group

data_localID$analysis.group <- as.factor(data_localID$analysis.group)
data_localID$analysis.group <- factor(data_localID$analysis.group, levels = c("GV-SE", "GV-ME", "Misc", "GI"))

plot.group <- ggplot(data_localID, aes(x = analysis.group, y = n.ISBNs.perWork)) +
  geom_boxplot(alpha = 0.3) +
  scale_y_continuous(name = "Number of ISBNs per work",
                     breaks = c(1, 5, 10, 15, 20, 40, 60),
                     limits = c(1, 70),
                     position = "right") +
  scale_x_discrete(name = "(b) Group", labels = c("GV-SE" = "GV-SE\n", "GV-ME", "Misc", "GI")) +
  theme_academic_ls() +
  theme(
    #axis.line.y.right = element_line(color = "grey70")
  )

# Combine plots

plot.combined <- grid.arrange(plot.source, plot.group, nrow = 1, ncol = 2)

ggsave("./NISBNs_300_edB.tiff", plot = plot.combined, width = 25, height = 15, units = "cm", dpi = 300)