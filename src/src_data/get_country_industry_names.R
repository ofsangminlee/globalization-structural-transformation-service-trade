## get_country_industry_names.R
## Get country and industry codes & names from OECD ICIO.

## Solving package dependency with Renv
renv::load("../")

## Read the ICIO readme file.
library("openxlsx")
list.country.industry <- read.xlsx("../../data/icio/ReadMe_ICIO2021_CSV.xlsx", sheet = "Country_Industry", startRow = 3)

## Country names
list.country.oecd <- list.country.industry[, c("V1", "OECD.countries")]
list.country.non.oecd <- list.country.industry[, c("V3", "Non-OECD.economies")]
colnames(list.country.oecd) <- colnames(list.country.non.oecd) <- c("code", "name") # ISO alpha-3 code and names
list.country <- rbind(list.country.oecd, list.country.non.oecd)
list.country <- subset(list.country, nchar(code) == 3)
list.country <- subset(list.country, !code %in% c("MX1", "MX2", "CN1", "CN2")) # 1,2 are production disaggregation into exporting firms vs non-exporting terms
list.country$name <- gsub("[[:digit:]]", "", list.country$name) # Remove Israel and Cyprus footnote
rm(list = c("list.country.oecd", "list.country.non.oecd"))

write.csv(list.country, file = "../../output/cleaned_data/list_country.csv", row.names = FALSE)

## Industry names
list.ind <- list.country.industry[, c("Code", "Industry", "ISIC.Rev.4")]
colnames(list.ind) <- c("code", "ind", "isic")
list.ind <- subset(list.ind, !is.na(code))
list.ind$code <- substr(list.ind$code, 2, nchar(list.ind$code))
list.ind$gs <- c(rep("g", 24), rep("s", 21)) # Categorize them into goods and services (24 goods and 21 services industries).

write.csv(list.ind, file = "../../output/cleaned_data/list_industry_two.csv", row.names = FALSE)
