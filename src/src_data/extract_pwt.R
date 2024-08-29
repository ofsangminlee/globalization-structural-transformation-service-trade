## extract_pwt.R
## Extract variables (i.e. real GDP, total labor force) from Penn World Tables.

## Solving package dependency with Renv
renv::load("../")

## Load the dataset
library("openxlsx")
pwt <- read.xlsx("../../data/pwt/pwt100.xlsx", sheet = "Data")

## Country list
list.country <- read.csv("../../output/cleaned_data/list_country.CSV")
list.country$code[!list.country$code %in% unique(pwt$countrycode)] # All countries in ICIO are in PWT
list.country <- list.country$code

## Excluding countries in ICIO, they will all be ROW
list.row <- setdiff(unique(pwt$countrycode), setdiff(list.country, "ROW"))

## Variables extracting from PWT.
## RGDPE: Expenditure-side real GDP at chained PPPs (in mil. 2017US$)
## EMP: persons engaged in millions
pwt.icio <- subset(pwt, year %in% 1995:2018 & countrycode %in% list.country)[, c("countrycode", "year", "rgdpe", "emp")]
colnames(pwt.icio)[1] <- "country"

pwt.row <- subset(pwt, year %in% 1995:2018 & countrycode %in% list.row)[, c("countrycode", "year", "rgdpe", "emp")]
colnames(pwt.row)[1] <- "country"

## Remove countries that has NAs
list.row.na <- unique(subset(pwt.row, is.na(rgdpe) | is.na(emp))[, "country"]) 
length(list.row.na) # 7 countries
list.row <- setdiff(list.row, list.row.na)
length(list.row) # 110 countries
pwt.row <- subset(pwt.row, country %in% list.row)

## Sum up 110 countries to ROW for EMP and RGDPE
pwt.row <- do.call("rbind", lapply(split(pwt.row, pwt.row$year), function(dat){
    temp <- data.frame(t(colSums(dat[, c("rgdpe", "emp")])))
    temp$year <- dat$year[1]
    return(temp)
}))

pwt.row$country <- "ROW"

## Get per-capita rgdpe
pwt <- rbind(pwt.icio, pwt.row)
pwt$rgdpe.pc <- pwt$rgdpe/pwt$emp

## Normalize with respect to US 2018
pwt.rgdpe.usa.2018 <- subset(pwt, country == "USA" & year == 2018)[, "rgdpe.pc"]
pwt$rgdpe.pc.norm <- pwt$rgdpe.pc/pwt.rgdpe.usa.2018

## Put X in front of years. (1995 -> X1995)
## In the wide format, more convenient to have column names starting with a character instead of a numeric.
pwt$year <- paste0("X", pwt$year)

write.csv(pwt, file = "../../output/cleaned_data/pwt_variables.csv", row.names = FALSE)
