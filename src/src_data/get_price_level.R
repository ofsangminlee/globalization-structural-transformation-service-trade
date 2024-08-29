## get_price_level.R
## Read the price level data from Groningen Growth and Development Center (GGDC) data.
## Convert price deflators to level and impute the prices for ROW.

## Solving package dependency with Renv
renv::load("../")

## Loading required packages
library("openxlsx")
library("tidyverse")
source("../common_functions.R")

list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.ind <- read.csv("../../output/cleaned_data/list_industry_three.csv")
list.ind.alt <- read.csv("../../output/cleaned_data/list_industry_three_alt.csv")

## Load data in 10 sector version and 35 industry version.
## 35 industry version is needed to separate JtK.
loc.file <- "../../data/prices/groningen/benchmark_2005.xlsx"
dat.10sec <- read.xlsx(loc.file, sheet = "GO_10Sector", startRow = 3)
dat.35ind <- read.xlsx(loc.file, sheet = "GO_35Industry", startRow = 3)

gron <- merge(dat.10sec, dat.35ind[, c("ISO-code", "J", "70", "71t74")], by = "ISO-code", all.x = TRUE)
gron[, c("Country", "JtK")] <- NULL
colnames(gron)[1] <- "country"

## 24 countries are not in this dataset. Impute them later.
setdiff(list.country$code, gron$country)

## Change into a long format
gron <- pivot_longer(gron, cols = !country, names_to = "ind", values_to = "p")

#########################################
## Gross-output price level conversion ##
#########################################

## Get gross-output weights for each GGDC sector from ICIO.
## Get 12 GGDC sectors and 45 ICIO sectors match.
list.re.go <- c("AtB", "C", "D", "E", "F", "G", "H", "I", "J", "70", "71t74", "LtP")
setdiff(unique(gron$ind), list.re.go)
setdiff(list.re.go, unique(gron$ind))

list.re.go.dis <- list(list.ind$code[1:2],
                       list.ind$code[3:5],
                       list.ind$code[6:22],
                       list.ind$code[23:24],
                       list.ind$code[25],
                       list.ind$code[26],
                       list.ind$code[32],
                       list.ind$code[c(27:31,33:35)],
                       list.ind$code[36],
                       list.ind$code[37],
                       list.ind$code[38:39],
                       list.ind$code[40:45])

setdiff(unlist(list.re.go.dis), list.ind$code)
setdiff(list.ind$code, unlist(list.re.go.dis))
length(unlist(list.re.go.dis)) == length(list.ind$code)

## Load ICIO and derive gross output for each GGDC sector.
read.icio.inner <- function(years, year) read.csv(unz(paste0("../../data/icio/ICIO_", years, ".zip"), paste0("ICIO2021_", year, ".csv")), header = T)

icio <- read.icio.inner("2005-2009", "2005")

colnames(icio)[1] <- "country.ind"
colnames(icio) <- gsub("_", "\\.", colnames(icio))
icio$country.ind <- gsub("_", "\\.", icio$country.ind)

icio <- col.agg.country(icio, "MEX", c("MEX", "MX1", "MX2"))
icio <- col.agg.country(icio, "CHN", c("CHN", "CN1", "CN2"))
icio <- row.agg.country(icio, "MEX", c("MEX", "MX1", "MX2"))
icio <- row.agg.country(icio, "CHN", c("CHN", "CN1", "CN2"))

icio.orig <- icio # Copy icio in the original format for later use

for (i in 1:length(list.re.go)){
    icio <- row.agg.ind(icio, list.re.go[i], list.re.go.dis[[i]])
}

icio <- icio[, c("country.ind", "TOTAL")]
icio <- subset(icio, name.ind(country.ind) != "TAXSUB")
icio <- subset(icio, !country.ind %in% c("VALU", "OUTPUT"))
icio$country <- name.country(icio$country.ind)
icio$ind <- name.ind(icio$country.ind)
icio$country.ind <- NULL
icio <- clean.colnames(icio, "TOTAL", "go")

## Price level derivation for 12 GGDC sectors.
dat <- merge(gron, icio, by = c("country", "ind"), all.x = TRUE)

dat$q <- dat$go/dat$p

## 12 GGDC sectors to reclassified sectors
list.reclass <- c("g", "bts", "hts")
list.sub <- list(c("AtB", "C", "D", "E"), c("F", "70", "LtP"), c("G", "I", "H", "J", "71t74"))
setdiff(unlist(list.sub), list.re.go)
setdiff(list.re.go, unlist(list.sub))

list.reclass.alt <- c("g", "cs", "ps")
list.sub.alt <- list(c("AtB", "C", "D", "E"), c("F", "H", "70", "LtP"), c("G", "I", "J", "71t74"))
setdiff(unlist(list.sub.alt), list.re.go)
setdiff(list.re.go, unlist(list.sub.alt))

list.reclass.gs <- c("g", "s")
list.sub.gs <- list(c("AtB", "C", "D", "E"), c("F", "H", "70", "LtP", "G", "I", "J", "71t74"))
setdiff(unlist(list.sub.gs), list.re.go)
setdiff(list.re.go, unlist(list.sub.gs))

list.country.gron <- setdiff(unique(dat$country), c("EU27", "EURO17"))

## Conversion
convert <- function(list.reclass, list.sub){
    ## Preallocation
    res <- data.frame(country = rep(list.country.gron, each = length(list.reclass)), ind = rep(list.reclass, length(list.country.gron)))
    res$pl.2005 <- NA

    ## Conversion
    for (i.country in list.country.gron){
        for (i in 1:length(list.reclass)){
            old.inds <- list.sub[[i]]
            new.ind <- list.reclass[i]
            temp <- subset(dat, country == i.country & ind %in% old.inds)
            res$pl.2005[res$country == i.country & res$ind == new.ind] <- sum(temp$go)/sum(temp$q)
        }
    }
    return(res)    
}

res <- convert(list.reclass, list.sub)
res.gs <- convert(list.reclass.gs, list.sub.gs)
res.alt <- convert(list.reclass.alt, list.sub.alt)

## What matters is the relative price. So normalize with respect to the US.
norm.us <- function(res){
    res.usa <- subset(res, country == "USA")
    colnames(res.usa)[3] <- "pl.2005.usa"

    res <- merge(res, res.usa[, c("ind", "pl.2005.usa")], by = "ind", all.x = TRUE)

    res$rel.pl.2005 <- res$pl.2005/res$pl.2005.usa
    return(res)
}

res <- norm.us(res)
res.gs <- norm.us(res.gs)
res.alt <- norm.us(res.alt)

###########################
## Imputing 24 countries ##
###########################

## Checking whether GDP per capita is a good predictor of relative prices.
## Derive total GDP.
gdp <- data.frame(country = list.country$code, gdp = (sapply(list.country$code, function(x) gdp.prod(x, icio.orig, TRUE))))

## Use labor force data to derive per-capita GDP
pwt <- read.csv("../../output/cleaned_data/pwt_variables.csv")
pwt.emp <- subset(pwt, year == "X2005")[, c("country", "emp", "rgdpe.pc")]
gdp <- merge(gdp, pwt.emp, by = "country", all.x = TRUE)
gdp$gdppc <- gdp$gdp/gdp$emp

## Check how price level correlates with GDPPC: VERY strong linear relationship. Also, the relationship is higher than PPP based real gdp variables.
merge.pl.gdppc <- function(res) merge(res, gdp[, c("country", "gdppc", "rgdpe.pc")], by = "country", all.x = TRUE)

res <- merge.pl.gdppc(res)
res.alt <- merge.pl.gdppc(res.alt)
res.gs <- merge.pl.gdppc(res.gs)

## Plotting
res$ind <- toupper(res$ind)
res.alt$ind <- toupper(res.alt$ind)
res.gs$ind <- toupper(res.gs$ind)

res <- add.title(res)
res.alt <- add.title.alt(res.alt)
res.gs <- add.title.gs(res.gs)

common.options <- list(geom_text(aes(x = log(gdppc), y = log(rel.pl.2005), color = ind, label = country), alpha = 0.5),
             geom_smooth(method = "lm", aes(x = log(gdppc), y = log(rel.pl.2005), color = ind)),         
             xlab("Log GDP per capita (2005US$)"),
             ylab("Log(price relative to the US)"))

pdf(file = "../../doc/figures/price_levels_paper.pdf", width = 10, height = 6) # If you want vertical plot, width 10, height = 13
ggplot(data = res) +
    common.options +
    plot.scatter.options(bw = TRUE) +
    scale_color_manual(values = default.color.base) +
    facet_wrap(~title.alp, scale = "free")
dev.off()

pdf(file = "../../doc/figures/price_levels_beamer.pdf", width = 10, height = 6)
ggplot(data = res) +
    common.options +
    plot.scatter.options(bw = FALSE) +
    scale_color_manual(values = default.color.base) +
    facet_wrap(~title, scale = "free")
dev.off()

pdf(file = "../../doc/figures/price_levels_alt.pdf", width = 10, height = 6)
ggplot(data = res.alt) +
    common.options +
    plot.scatter.options(bw = TRUE) +
    scale_color_manual(values = default.color.alt) +
    facet_wrap(~title.alp, scale = "free") +
    theme(strip.text = element_text(size = 15))
dev.off()

pdf(file = "../../doc/figures/price_levels_gs.pdf", width = 10, height = 6) # If you want vertical plot, width 10, height = 13
ggplot(data = res.gs) +
    common.options +
    plot.scatter.options(bw = FALSE) +
    scale_color_manual(values = default.color.gs) +
    facet_wrap(~title.alp, scale = "free")
dev.off()

##### SAVE RES AS CSV #####

## Goods: gdppc vs rgdpe.pc (both work well, but GDPPC works slightly better, based on R^2)
## Also will be using regression to impute price levels of countries not in sample.
temp.g <- subset(res, ind == "G")
lm.g <- lm(log(rel.pl.2005) ~ log(gdppc), data = temp.g)
summary(lm.g)

lm.g.2 <- lm(log(rel.pl.2005) ~ log(rgdpe.pc), data = temp.g)
summary(lm.g.2)

## BTS
temp.bts <- subset(res, ind == "BTS")
lm.bts <- lm(log(rel.pl.2005) ~ log(gdppc), data = temp.bts)

## HTS
temp.hts <- subset(res, ind == "HTS")
lm.hts <- lm(log(rel.pl.2005) ~ log(gdppc), data = temp.hts)

## CS
temp.cs <- subset(res.alt, ind == "CS")
lm.cs <- lm(log(rel.pl.2005) ~ log(gdppc), data = temp.cs)

## PS
temp.ps <- subset(res.alt, ind == "PS")
lm.ps <- lm(log(rel.pl.2005) ~ log(gdppc), data = temp.ps)

## S
temp.s <- subset(res.gs, ind == "S")
lm.s <- lm(log(rel.pl.2005) ~ log(gdppc), data = temp.s)


## Using the strong linear relationship, impute the price levels for countries not in Groningen data.
list.country.not <- setdiff(list.country$code, list.country.gron)

impute.not <- function(list.reclass, res){
    res.not <- data.frame(country = rep(list.country.not, each = length(list.reclass)), ind = rep(list.reclass, length(list.country.not)))
    res.not$rel.pl.2005 <- NA

    for (i.country in list.country.not){
        for (i.ind in list.reclass){
            temp.gdppc <- gdp$gdppc[gdp$country == i.country]
            temp <- exp(predict(get(pasted("lm", i.ind)), data.frame(gdppc = temp.gdppc)))
            res.not$rel.pl.2005[res.not$country == i.country & res.not$ind == i.ind] <- temp
        }
    }
    res.not$imputed <- 1
    res$imputed <- 0

    result <- rbind(res[, c("country", "ind", "rel.pl.2005", "imputed")], res.not)
    return(result)
}

## Result (price level relative to that of US)
rel.pl.2005 <- impute.not(list.reclass, res)
rel.pl.2005.alt <- impute.not(list.reclass.alt, res.alt)
rel.pl.2005.gs <- impute.not(list.reclass.gs, res.gs)
colnames(rel.pl.2005)[3] <- colnames(rel.pl.2005.alt)[3] <- colnames(rel.pl.2005.gs)[3] <- "rel.pl"

## Load gross-output price deflators
load("../../output/cleaned_data/gopd.RData", verbose = TRUE)
load("../../output/cleaned_data/gopd_alt.RData", verbose = TRUE)
load("../../output/cleaned_data/gopd_gs.RData", verbose = TRUE)

## Industry lower case
rel.pl.2005$ind <- tolower(rel.pl.2005$ind)
rel.pl.2005.alt$ind <- tolower(rel.pl.2005.alt$ind)
rel.pl.2005.gs$ind <- tolower(rel.pl.2005.gs$ind)

## Using price deflators and using price levels for a single year, covert deflators to levels.
gopd.level <- function(gopd, i.year, pl.data){
    ## First, get the conversion factor that you  multiply to price deflators.
    ## PD for all countries and PDs of US.
    gopd.year <- subset(gopd, year == paste0("X", i.year))[, c("country", "ind", "p.usd")]
    colnames(gopd.year)[3] <- "gopd"
    gopd.usa <- subset(gopd.year, country == "USA")[, c("ind", "gopd")]
    colnames(gopd.usa)[2] <- "gopd.usa"
    gopd.year <- merge(gopd.year, gopd.usa, by = "ind", all.x = TRUE)

    ## Derive conversion factor using the following relationship
    ## PD-country*CONV/PD-USA = Relative price level
    gopd.year <- merge(gopd.year, pl.data, by = c("country", "ind"), all.x = TRUE)
    gopd.year$conv <- gopd.year$rel.p*gopd.year$gopd.usa/gopd.year$gopd
    gopd.conv <- gopd.year[, c("country", "ind", "conv")]

    ## Do the conversino using the factor
    gopl <- merge(gopd, gopd.conv, by = c("country", "ind"), all.x = TRUE)
    gopl$pl <- gopl$p.usd*gopl$conv
    return(gopl)
}

gopl <- gopd.level(gopd.final, 2005, rel.pl.2005)
gopl.alt <- gopd.level(gopd.final.alt, 2005, rel.pl.2005.alt)
gopl.gs <- gopd.level(gopd.final.gs, 2005, rel.pl.2005.gs)

## Imputing row
## This version: impute prices for ROW from the relationship between price levels and gdp per capita.

## Another way: take average or weighted average of developing countries' prices. (probably this might be a better way to do it, for exact hat algebra. Because you don't want to use the levels. Check this.)

## Note: imputed ROW prices are not used for estimation.

## Each year's GDP
load("../../output/cleaned_data/cleaned_icio.RData", verbose = TRUE)
list.country <- read.csv("../../output/cleaned_data/list_country.csv")
derive.gdp <- function(dat) do.call("rbind", lapply(list.country$code, function(x) data.frame(country = x, gdp = gdp.prod(x, dat, FALSE))))

icio.gdp <- lapply(icio.years, derive.gdp)
for (i in 1:24){
    icio.gdp[[i]][, "year"] <- paste0("X", 1994 + i)
}
icio.gdp <- do.call("rbind", icio.gdp)

## Per capita gdp
pwt.emp <- read.csv("../../output/cleaned_data/pwt_variables.csv")
icio.gdp <- merge(icio.gdp, pwt.emp, by = c("country", "year"), all = TRUE)
icio.gdp <- subset(icio.gdp, country != "TOT")
icio.gdp$gdppc <- icio.gdp$gdp/icio.gdp$emp

## Save gdp per capita
## This will be the starting point for model simulation.
gdp.data <- icio.gdp[, c("country", "year", "gdp", "emp", "gdppc")]
write.csv(gdp.data, file = "../../output/cleaned_data/gdp_per_capita.csv", row.names = FALSE)

## Share of GDP by ROW (I mention this in the appendix)
gdp.2018 <- subset(gdp.data, year == "X2018")
writeLines(paste0("ROW share of the world total GDP is ", gdp.2018$gdp[gdp.2018$country == "ROW"]/sum(gdp.2018$gdp), "."), "../../doc/nums/row_share_gdp.txt")

## Function to impute row
impute.row <- function(gopl, list.reclass){

    ## Merge with gopl
    gopl.temp <- gopl

    gopl.temp <- gopl.temp[, c("country", "year", "ind", "pl")]
    gopl.temp <- pivot_wider(gopl.temp, names_from = "ind", values_from = "pl")
    gopl.temp <- clean.colnames(gopl.temp, list.reclass, pasted("p", list.reclass))
    gopl.temp <- merge(gopl.temp, icio.gdp[, c("country", "year", "gdppc")], by = c("country", "year"), all.x = TRUE)

    ## Regression of price on gdp per capita + year fixed effects
    ## They show very linear relationship.
    for (i.ind in list.reclass){
        ttt <- paste0("lm.", i.ind, " <- lm(log(p.", i.ind, ") ~ log(gdppc) + year, data = gopl.temp)")
        ## For example, lm.g <- lm(log(p.g) ~ log(gdppc) + year, data = gopl.temp)
        eval(parse(text = ttt))
    }

    icio.gdp.row <- subset(icio.gdp, country == "ROW")[, c("country", "gdppc", "year")]

    for (i.ind in list.reclass){
        ttt2 <- paste0("icio.gdp.row$p.", i.ind, " <- exp(predict(lm.", i.ind, ", icio.gdp.row))")
        ## For example, icio.gdp.row$p.g <- exp(predict(lm.g, icio.gdp.row))
        eval(parse(text = ttt2))
    }

    ## Clean and save the result
    icio.gdp.row <- icio.gdp.row[, c("country", "year", pasted("p", list.reclass))]
    icio.gdp.row <- pivot_longer(icio.gdp.row, cols = pasted("p", list.reclass), names_to = "ind", values_to = "pl")
    icio.gdp.row$ind <- substr(icio.gdp.row$ind, 3, nchar(icio.gdp.row$ind))
    icio.gdp.row[, c("p.lcu", "ner.2015", "p.usd", "conv")] <- NA
    gopl <- rbind(gopl, icio.gdp.row)
    return(gopl)
}

gopl <- impute.row(gopl, list.reclass)
stopifnot(nrow(gopl) == (2018-1994)*67*3)

gopl.alt <- impute.row(gopl.alt, list.reclass.alt)
stopifnot(nrow(gopl.alt) == (2018-1994)*67*3)

gopl.gs <- impute.row(gopl.gs, list.reclass.gs)
stopifnot(nrow(gopl.gs)== (2018-1994)*67*2)

## Save the result
write.csv(gopl, file = "../../output/cleaned_data/gopl.csv", row.names = FALSE)
write.csv(gopl.alt, file = "../../output/cleaned_data/gopl_alt.csv", row.names = FALSE)
write.csv(gopl.gs, file = "../../output/cleaned_data/gopl_gs.csv", row.names = FALSE)




