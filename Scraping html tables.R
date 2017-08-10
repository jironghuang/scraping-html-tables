#Web Scraping and Applied Clustering Global Happiness and Social Progress Index

require(rvest)
require(magrittr)
require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)
require(FactoMineR)
require("factoextra")
require(cluster)
require("useful")

#Scraping the data from html in table format
# Import the first data set from the site
url1 <- "https://en.wikipedia.org/wiki/World_Happiness_Report"
# happy <- read_html(url1) %>% 
#   html_nodes("table") %>% 
#   extract2(1) %>% 
#   html_table()

webpage = read_html(url1)
tbls <- html_nodes(webpage, "table")

#Extract all the tables
tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[1:4] %>%
  html_table(fill = TRUE)

happy = tbls_ls[[1]]

# inspect imported data structure 

str(happy)

## Exclude columns with ranks and scores, retain the other columns
happy <- happy[c(3,6:11)]
### rename column headers 
colnames(happy) <- gsub(" ", "_", colnames(happy), perl=TRUE)
names(happy)

### scrape social progress index data report from the site
url2 <- "https://en.wikipedia.org/wiki/List_of_countries_by_Social_Progress_Index"
webpage = read_html(url2)
tbls <- html_nodes(webpage, "table")

#Extract all the tables
tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[1:5] %>%
  html_table(fill = TRUE)

social = tbls_ls[[4]]

### Again, exclude columns with ranks, keep the rest
social <- social[c(1,5,7,9)]
### rename column headers 
names(social) <- c("Country", "basic_human_needs", "foundations_well_being", "opportunity")

### Standardize country names to confirm with country names in the map dataset 
social$Country <- as.character(mapvalues(social$Country, 
                                         from = c("United States", "Côte d'Ivoire","Democratic Republic of Congo", "Congo", "Trinidad and Tobago"),
                                         to=c("USA", "Ivory Cost","Democratic Republic of the Congo", "Democratic Republic of the Congo", "Trinidad")))

## coerce character data columns to numeric
social[, 2:4] <- sapply(social[, 2:4], as.numeric)

### perform left join
soc.happy <- left_join(happy, social, by = c('Country' = 'Country'))
### check for missing values in the combined data set
mean(is.na(soc.happy[, 2:10]))

### median imputation
for(i in 1:ncol(soc.happy[, 2:10])) {
  soc.happy[, 2:10][is.na(soc.happy[, 2:10][,i]), i] <- median(soc.happy[, 2:10][,i], na.rm = TRUE)
}
### summary of the raw data
summary(soc.happy[,2:10])



