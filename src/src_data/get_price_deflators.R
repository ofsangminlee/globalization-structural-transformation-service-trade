## get_price_deflators.R
## Obtain gross-output price deflators (GOPD) for the sample countries.

## A. Aggregate local currency price deflators into three reclassified industries (g, bts, hts), alternative three industry classification (g, cs, ps), two industry classification (g, s)

## 1. OECD STAN gross-output price deflators
## 2. OECD STAN value-added price deflators (VAPD)
## 3. Check whether converting VAPD to GOPD through double deflation method is reasonable by comparing A.2 and A.3.
## 4. UN NA_MAIN VAPD
## 5. UN NA AMA VAPD (Precision: OECD STAN > UN NA_MAIN > UN NA_AMA)
## 6. For Taiwan, obtain VAPD from their own statistics agency. (Precision = OECD STAN)
## 7. Check whether A.4~5 (which lack full sectoral details) are good proxies for A.2.

## B. Convert VAPD to GOPD.

## C. Adjust for nominal exchange rate fluctuations.

## D. Save the result.
## Solving package dependency with Renv
renv::load("../")

## Loading required packages
library("openxlsx")
library("tidyverse")
library("xtable")
library("directlabels")
source("../common_functions.R")

## Load list of countries in ICIO
list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.ind.three <- read.csv("../../output/cleaned_data/list_industry_three.csv")
list.ind.three.alt <- read.csv("../../output/cleaned_data/list_industry_three_alt.csv")

## Some often-used variables
years <- 1995:2018
years.x <- paste0("X", years)

list.re <- c("total", "g", "bts", "hts")
list.re.alt <- c("total", "g", "cs", "ps")
list.re.gs <- c("total", "g", "s")

##########################################
## Function for price index aggregation ##
##########################################

## Reason why we need this: Chain-type price indices are not additive.
## Function input: total nominal output, real volume, price index in a wide format (wide in terms of years).
## Function output: aggregated version
pi.qi.agg <- function(data, years, base.year, new.ind, old.inds, var.output, var.volume, var.price, method){
    ## Construct quantity index
    ## \sum P_t Q_t (P_{t-1} Q_{t-1})
    X.t <- colSums(do.call("rbind", lapply(old.inds, function(i){
        temp <- data[data$var == var.output & data$ind == i, years]
        stopifnot(nrow(temp) == 1)
        return(temp)
        })))
    ## \sum P_{t-1} Q_{t}
    years.minus <- years[1:(length(years)-1)] 
    years.plus <- years[2:length(years)] 
    I.t <- colSums(do.call("rbind", lapply(old.inds, function(i){
        temp <- data[data$var == var.price & data$ind == i, years.minus]*
            data[data$var == var.volume & data$ind == i, years.plus]
        stopifnot(nrow(temp) == 1)
        return(temp)
    })))
    ## \sum P_t Q_{t-1}
    I.t2 <- colSums(do.call("rbind", lapply(old.inds, function(i){
        temp <- data[data$var == var.price & data$ind == i, years.plus]*
            data[data$var == var.volume & data$ind == i, years.minus]
        stopifnot(nrow(temp) == 1)
        return(temp)
    })))
    ## QI current year / QI previous year
    if (method == "laspeyres"){
        ## QI previous year: I_t / I_{t-1}
        qi.prev <- I.t/(X.t[1:(length(X.t)-1)])   
    }
    if (method == "fisher"){
        ## QI previous year: I_t^f / I_{t-1}^f
        qi.prev  <- sqrt( (I.t/(X.t[1:(length(X.t)-1)])) * (X.t[2:length(X.t)]/I.t2) )
    }
    qi.prev <- c(1,qi.prev)
    names(qi.prev) <- years

    ## Data for all years are available, then there is no problem
    ## If not (e.g. the data does not start from 1995.), there will be NA's in "qi.prev." Substitute NA to 1's.
    ## NA's will be reintroduced later since "X.t" contains NA's for those years.
    qi.prev <- replace(qi.prev, is.na(qi.prev), 1)
    ## QI, where the base year is the first year
    qi.new.base1 <- cumprod(qi.prev)

    ## Rebasing to new base year (PI in base year is 100)
    ## If the base year has NA's, this incurs NA's. However, for all dataset and countries, info in 2015 is available.
    qi.new <- qi.new.base1/qi.new.base1[base.year]*X.t[base.year]
    pi.new <- X.t/qi.new
    ## Return the result
    res <- as.data.frame(rbind(X.t, qi.new, pi.new))
    res$var <- c(var.output, var.volume, var.price)
    res$ind <- new.ind
    res$country <- data$country[1]
    return(res)
}

## Function for aggregating into aggregated sectors (G, CS, PS)
pi.qi.agg.reclass <- function(data, list.reclass, list.reclass.inds, years, base.year, var.output, var.volume, var.price, method) do.call("rbind", lapply(1:length(list.reclass), function(i) pi.qi.agg(data, years, base.year, list.reclass[i], list.reclass.inds[[i]], var.output, var.volume, var.price, method)))

#######################################
## A.1. STAN GOPD (19 + 4 countries) ##
#######################################

## For G-BTS-HTS, 16 countries with full years, 2 countries with missing 1 or 2 years.
## For G-CS-PS, 19 countries with full years, 4 countries with missing 1 or 2 years.

## Gross output price indices for countries in OECD STAN and which span ICIO time period 1995 ~ 2018.
## OECD STAN is in million local currency units.

## Load OECD STAN
dat <- read.table(unz("../../data/prices/stan/STANi4_ALL.zip", "STANi4_ALL/DATA.txt"), header = T, sep = "|")
colnames(dat)[1] <- "country"

## Are all STAN countries in ICIO?
stan.country <- unique(dat$country)
prod(stan.country %in% list.country$code)

## Key variables:
## PROD, PROK, PRDP: current prices output, constant price output (volumes), deflators for PRODuction (gross output)
## VALU, VALK, VALP: for VALUe-added
go.vars <- c("PROD", "PRDK", "PRDP")
va.vars <- c("VALU", "VALK", "VALP")
dat <- subset(dat, var %in% c(go.vars, va.vars) & year %in% years)

## Right now base year price indices are 100, change it to 1.
dat$value[dat$var %in% c("PRDP", "VALP")] <- dat$value[dat$var %in% c("PRDP", "VALP")]/100

## Dataset for STAN gopd and vapd
dat.gopd <- subset(dat, var %in% go.vars)
dat.vapd <- subset(dat, var %in% va.vars)
rm("dat")

## Check a) each country's time coverage for GO PI. b) whether each country has all three variables (real output, price, nominal output)
check.coverage <- function(dat, vars) do.call("rbind", lapply(split(dat, dat$country), function(dat.country) 
    data.frame(country = dat.country$country[1], start = min(dat.country$year), end = max(dat.country$year), exist.all = prod(vars %in% dat.country$var))))

cov.gopd <- check.coverage(dat.gopd, go.vars)

## Which country has full coverage (data from 1995 to 2018 for the three variables)
(country.gopd <- cov.gopd$country[(cov.gopd$start <= 1995 & cov.gopd$end >= 2018) & (cov.gopd$exist.all == 1)]) # 25 countries out of total 38 ICIO countries in STAN.

(country.gopd.partial <- cov.gopd[(!(cov.gopd$start <= 1995 & cov.gopd$end >= 2018)) & (cov.gopd$exist.all == 1), ]) ## For Canada and Chile only 1 year is missing, Colombia 5 years. For Canada and Chile, Add information elsewhere.

## Check whether three gross-output variables show up for industry categories needed for all years.
list.bts <- list.ind.three$code[list.ind.three$g.bts.hts == "bts"]
list.hts <- list.ind.three$code[list.ind.three$g.bts.hts == "hts"]

list.g <- c("D01T03", "D05T39") ## Agriculture, hunting, forestry and fishing [A], Industry including energy [B-E]

list.bts <- paste0("D", list.bts)

list.hts.disagg <- paste0("D", list.hts)
list.hts <- c("D45T47", "D49T53", "D55T56", "D58T63", "D64T66", "D69T75", "D77T82", "D90T93") # Change industries into 1 digit ISIC for HTS. (Some countries report the industries of PS in a less aggregated ways.)

list.ind <- c(list.g, list.bts, list.hts)

## Robustness: alternative three industries (G-CS-PS) vs two industry (G-S)
ind.aa <- c("D55T56", "D90T93") # Accommodation / Arts
list.cs <- c(list.bts, ind.aa)
setequal(list.cs, paste0("D", list.ind.three.alt$code[list.ind.three.alt$g.cs.ps == "cs"]))
list.ps <- setdiff(list.hts, ind.aa)

list.s <- c(list.cs, list.ps)

## Check whether disaggregated information is available for each industry.
dat.gopd.ind <- subset(dat.gopd, ind %in% list.ind)

check.coverage.ind <- function(dat, list.country, list.ind, years)
    lapply(list.country, function(ex.country){
        temp <- do.call("rbind", lapply(list.ind, function(ex.ind){
            return(do.call("cbind", lapply(years, function(ex.year){
                return(nrow(subset(dat, country == ex.country & year == ex.year & ind == ex.ind)) == 3)
            })))
        }))
        colnames(temp) <- years.x
        temp <- data.frame(temp)
        temp$country <- ex.country
        temp$ind <- list.ind
        temp <- temp[, c("country", "ind", years.x)]
        return(temp)
    })

## COVERAGE of GOPD
## country.gopd: all years
## country.gopd.partial: CAN 1st year missing CHL last year missing
cov.gopd.ind <- check.coverage.ind(dat.gopd.ind, c(country.gopd, "CAN", "CHL"), list.ind, years)
names(cov.gopd.ind) <- c(country.gopd, "CAN", "CHL")

## Countries with coverage for all industries: 16 countries
(country.gopd.ind <- c(country.gopd, "CAN", "CHL")[unlist(lapply(cov.gopd.ind, function(dat) prod(dat[, years.x]) == 1))])
## Countries that need manual treatment
(country.gopd.manual <- setdiff(c(country.gopd, "CAN", "CHL"), country.gopd.ind))

## Among country.gopd.manual, you can do PRT and ITA automatically.
## For countries with ALL needed information, do the 3-sector conversion (16 countries + PRT and ITA, which have missing information last years.)

## cov.gopd.ind[["PRT"]]
## cov.gopd.ind[["ITA"]]

dat.gopd.ind.all <- subset(dat.gopd, country %in% c(country.gopd.ind, "ITA", "PRT") & ind %in% list.ind)

## Function to clean the STAN price data and turn it into wide format.
clean.stan <- function(dat){
    dat$year <- paste0("X", dat$year)
    dat$flag <- NULL
    dat <- pivot_wider(dat, names_from = "year", values_from = "value")
    return(dat)
}

dat.gopd.ind.all <- clean.stan(dat.gopd.ind.all)

## ## LAST YEARS FOR ITA AND PRT
## subset(dat.gopd.ind.all, country == "ITA")[, c("country", "var", "ind", "X2018")] %>% data.frame()
## subset(dat.gopd.ind.all, country == "PRT")[, c("country", "var", "ind", "X2018")] %>% data.frame()

## Convert it to three sectors.

oecd.stan.auto <- function(dat.price, var.x, var.q, var.p, list.re, list.disagg, country.full, w.gopd = TRUE){

    temp.1 <- subset(dat.price, country %in% country.full)
    pd.ind.all.1 <- do.call("rbind", lapply(split(temp.1, temp.1$country), function(dat.country) pi.qi.agg.reclass(dat.country, list.re, list.disagg, years.x, "X2015", var.x, var.q, var.p, "laspeyres"))) # No USA and CAN for conversion using this function.
    
    if (w.gopd){ # For GOPD, PRT and ITA don't have last year info.

        years.x.2 <- years.x[1:length(years.x)-1]
        temp.2 <- subset(dat.price, country %in% c("PRT", "ITA"))
        pd.ind.all.2 <- do.call("rbind", lapply(split(temp.2, temp.2$country), function(dat.country) pi.qi.agg.reclass(dat.country, list.re, list.disagg, years.x.2, "X2015", var.x, var.q, var.p, "laspeyres")))
        pd.ind.all.2$X2018 <- NA
        
        pd.ind.all <- rbind(pd.ind.all.1, pd.ind.all.2)
        return(pd.ind.all)
    }

    else {
        return(pd.ind.all.1)
    }
}

list.orig <- list(list.ind, list.g, list.bts, list.hts)
list.orig.alt <- list(list.ind, list.g, list.cs, list.ps)
list.orig.gs <- list(list.ind, list.g, list.s)

gopd.ind.all <- oecd.stan.auto(dat.gopd.ind.all, "PROD", "PRDK", "PRDP", list.re, list.orig, country.gopd.ind) # country.gopd.ind + PRT & ITA

gopd.ind.all.alt <- oecd.stan.auto(dat.gopd.ind.all, "PROD", "PRDK", "PRDP", list.re.alt, list.orig.alt, country.gopd.ind)

gopd.ind.all.gs <- oecd.stan.auto(dat.gopd.ind.all, "PROD", "PRDK", "PRDP", list.re.gs, list.orig.gs, country.gopd.ind)

## Check countries with missing entries
cov.gopd.manual <- cov.gopd.ind[country.gopd.manual]

which.missing <- function(list.dat, years.x) lapply(list.dat, function(dat){
    dat.miss.col <- apply(dat[, years.x], 2, function(i.col) prod(i.col) == 1)
    dat.miss.col <- attr(dat.miss.col, "names")[!dat.miss.col]
    dat.miss.row <- !apply(dat[, years.x], 1, function(i.row) prod(i.row) == 1)
    dat <- dat[dat.miss.row, c("country", "ind", dat.miss.col)]
    return(dat)
})

cov.gopd.manual <- which.missing(cov.gopd.manual, years.x)

## Special cases conversion
get.data.special <- function(dat, ex.country, list.ind.country){
    temp <- subset(dat, country == ex.country & ind %in% list.ind.country)
    return(clean.stan(temp))
}

## Use the following function to manually check special cases along with the Excel file for coverage.
print.check <- function(ex.num){
    print(cov.gopd.manual[[ex.num]])
    print(cov.gopd.manual[[ex.num]]$ind)
}
        

## Manual check
## The key is where you can find more aggregated sectors that cover BTS or HTS / CS or PS.
## For example, even if you don't see D90T93, D94T96, and D97T98, you can do the price aggregation for CS if you see D90T99.
## a: BTS-HTS, b: CS-PS

## 1) CHE: 69~75, 77~82, 90~93, 94~96 / 2018 NOT possible
print.check(1)

## 1-a) CHE: Do for years 97 ~ 17 with full sectoral details
dat.che.a <- get.data.special(dat.gopd, "CHE", list.ind)

## Special case with subset of years
get.gopd.special.years <- function(dat, list.re, list.inds, years) pi.qi.agg.reclass(dat, list.re, list.inds, years, "X2015", "PROD", "PRDK", "PRDP", "laspeyres")

## Special case with full years
get.gopd.special <- function(dat, list.re, list.inds) pi.qi.agg.reclass(dat, list.re, list.inds, years.x, "X2015", "PROD", "PRDK", "PRDP", "laspeyres")

gopd.che <- get.gopd.special.years(dat.che.a, list.re, list(list.ind, list.g, list.bts, list.hts), paste0("X", 1997:2017))

gopd.che[, c("X1995", "X1996", "X2018")] <- NA

## 1-b) CHE: Merge 69 ~ 82, 90 ~ 99 -> 1995 ~ 2017. Get 2018 somewhere else 69 ~ 82, you cannot do.
list.cs.che <- c(setdiff(list.cs, c("D90T93", "D94T96", "D97T98")), "D90T99")
list.ps.che <- c(setdiff(list.ps, c("D69T75", "D77T82")), "D69T82")
list.ind.che <- c(list.g, list.cs.che, list.ps.che)

dat.che <- get.data.special(dat.gopd, "CHE", list.ind.che)
## dat.che[, "X2018"]

gopd.che.alt <- get.gopd.special.years(dat.che, list.re.alt, list(list.ind.che, list.g, list.cs.che, list.ps.che), paste0("X", 1995:2017))
gopd.che.alt[, "X2018"] <- NA

gopd.che.gs <- get.gopd.special.years(dat.che, list.re.gs, list(list.ind.che, list.g, c(list.cs.che, list.ps.che)), paste0("X", 1995:2017))
gopd.che.gs[, "X2018"] <- NA

## 2) IRL: can't (17 years missing)
print.check(2)

## 3) ITA: except last year
print.check(3)

## 4) JPN: 69 ~ 82, 90 ~ 99. (Since 90~93 information is not provided, we cannot do BTS-HTS case.)
print.check(4)

## 4-b) JPN CS-PS: do the same with CHE
list.cs.jpn <- list.cs.che
list.ps.jpn <- list.ps.che
list.ind.jpn <- list.ind.che

dat.jpn <- get.data.special(dat.gopd, "JPN", list.ind.jpn)
gopd.jpn.alt <- get.gopd.special(dat.jpn, list.re.alt, list(list.ind.jpn, list.g, list.cs.jpn, list.ps.jpn))
gopd.jpn.gs <- get.gopd.special(dat.jpn, list.re.gs, list(list.ind.jpn, list.g, c(list.cs.jpn, list.ps.jpn)))

## 5) LVA: 97 ~ 98 not showing up in only the last year.
print.check(5)

## 5-a) LVA: Do it without D97T98
list.bts.lva <- setdiff(list.bts, "D97T98")
list.ind.lva.a <- c(list.g, list.bts.lva, list.hts)
dat.lva.a <- get.data.special(dat.gopd, "LVA", list.ind.lva.a)
gopd.lva <- get.gopd.special.years(dat.lva.a, list.re, list(list.ind.lva.a, list.g, list.bts.lva, list.hts), years.x)

## 5-b) Merge 90~99
list.cs.lva <- list.cs.che
list.ind.lva <- c(list.g, list.cs.lva, list.ps)

dat.lva <- get.data.special(dat.gopd, "LVA", list.ind.lva)
gopd.lva.alt <- get.gopd.special(dat.lva, list.re.alt, list(list.ind.lva, list.g, list.cs.lva, list.ps))
gopd.lva.gs <- get.gopd.special(dat.lva, list.re.gs, list(list.ind.lva, list.g, c(list.cs.lva, list.ps)))

## 6) PRT: except last year
print.check(6)

## 7) SVK: 97~98 / Last two years 
print.check(7)

## 7-a) Do it without D97T98
list.bts.svk <- setdiff(list.bts, "D97T98")
list.ind.svk.a <- c(list.g, list.bts.svk, list.hts)

dat.svk.a <- get.data.special(dat.gopd, "SVK", list.ind.svk.a)
gopd.svk <- get.gopd.special.years(dat.svk.a, list.re, list(list.ind.svk.a, list.g, list.bts.svk, list.hts), paste0("X", 1995:2016))
gopd.svk[, c("X2017", "X2018")] <- NA
    
## 7-b) Use 90 ~ 99
list.cs.svk <- list.cs.che
list.ind.svk <- c(list.g, list.cs.svk, list.ps)

dat.svk <- get.data.special(dat.gopd, "SVK", list.ind.svk)

gopd.svk.alt <- get.gopd.special.years(dat.svk, list.re.alt, list(list.ind.svk, list.g, list.cs.svk, list.ps), paste0("X", 1995:2016))
gopd.svk.alt[, c("X2017", "X2018")] <- NA

gopd.svk.gs <- get.gopd.special.years(dat.svk, list.re.gs, list(list.ind.svk, list.g, c(list.cs.svk, list.ps)), paste0("X", 1995:2016))
gopd.svk.gs[, c("X2017", "X2018")] <- NA

## 8) SVN: can't (5 years missing)
print.check(8)

## 9) USA
print.check(9)

## 9-a) Full sectoral details for 1997~2018
dat.usa.a <- get.data.special(dat.gopd, "USA", list.ind)
gopd.usa <- get.gopd.special.years(dat.usa.a, list.re, list(list.ind, list.g, list.bts, list.hts), paste0("X", 1997:2018))
gopd.usa[, c("X1995", "X1996")] <- NA

## 9-b) 84 ~ 99

list.cs.usa <- c(setdiff(list.cs, c("D84", "D85", "D86T88", "D90T93", "D94T96", "D97T98")), "D84T99")
list.ind.usa <- c(list.g, list.cs.usa, list.ps)

dat.usa <- get.data.special(dat.gopd, "USA", list.ind.usa)

gopd.usa.alt <- pi.qi.agg.reclass(dat.usa, list.re.alt, list(list.ind.usa, list.g, list.cs.usa, list.ps), years.x, "X2015", "PROD", "PRDK", "PRDP", "fisher")

gopd.usa.gs <- pi.qi.agg.reclass(dat.usa, list.re.gs, list(list.ind.usa, list.g, c(list.cs.usa, list.ps)), years.x, "X2015", "PROD", "PRDK", "PRDP", "fisher")

## 10) CAN: can't manufacturing starts from 2010.
print.check(10)

## 11) CHL: can't
print.check(11)

## Merge the gopd data

## For Baseline G-BTS-HTS, 22 countries (ITA, PRT one year missing, CHE 97~17, SVK two years missing, USA 97~18)
country.gopd.final <- c(country.gopd.ind, "ITA", "PRT", "CHE", "SVK", "LVA", "USA")
length(country.gopd.final)

gopd <- rbind(gopd.ind.all, gopd.che, gopd.svk, gopd.lva, gopd.usa)
gopd$var <- ifelse(gopd$var == "PROD", "go.v", ifelse(gopd$var == "PRDK", "go.q", ifelse(gopd$var == "PRDP", "go.p", "")))

## Alternative: CS and PS set up.
## 23 countries (ITA, PRT, CHE one year missing, SVK two years missing.)
country.gopd.final.alt <- c(country.gopd.ind, "ITA", "PRT", "CHE", "JPN", "SVK", "LVA", "USA")
length(country.gopd.final.alt)

gopd.alt <- rbind(gopd.ind.all.alt, gopd.che.alt, gopd.jpn.alt, gopd.svk.alt, gopd.lva.alt, gopd.usa.alt)
gopd.alt$var <- ifelse(gopd.alt$var == "PROD", "go.v", ifelse(gopd.alt$var == "PRDK", "go.q", ifelse(gopd.alt$var == "PRDP", "go.p", "")))

## G-S
country.gopd.final.gs <- country.gopd.final.alt

gopd.gs <- rbind(gopd.ind.all.gs, gopd.che.gs, gopd.jpn.gs, gopd.svk.gs, gopd.lva.gs, gopd.usa.gs)
gopd.gs$var <- ifelse(gopd.gs$var == "PROD", "go.v", ifelse(gopd.gs$var == "PRDK", "go.q", ifelse(gopd.gs$var == "PRDP", "go.p", "")))

## Remove unnecessary objects
rm(list = c("dat.gopd", "dat.gopd.ind", "dat.gopd.ind.all"))

## Function to check availablility of price deflators for country-years
check.pd.avail <- function(pd, var.p, list.re, num.ind = 3){
    avail <- data.frame(country = rep(setdiff(list.country$code, "ROW"), each = num.ind), ind = rep(list.re, 66))
    for (i.year in years.x){
        avail[, i.year] <- 0
    }
    for (ex.country in setdiff(list.country$code, "ROW")){
        for (i.year in years.x){
            for (i.ind in list.re){
                temp <- subset(pd, var == var.p & country == ex.country & ind == i.ind)[, i.year]
                if (length(temp) == 1){
                    if (!is.na(temp)){
                        avail[avail$country == ex.country & avail$ind == i.ind, i.year] <- 1
                    }
                }
            }
        }
    }
    return(avail)
}

avail.to.table <- function(dat, list.re){
    dat <- pivot_longer(dat, cols = all_of(years.x), names_to = "year")
    dat <- pivot_wider(dat, names_from = ind, values_from = "value")
    dat$avail <- apply(dat[, list.re], 1, prod)
    dat <- dat[, c("country", "year", "avail")]
    dat$year <- as.numeric(substr(dat$year, 2, 5))
    
    dat <- do.call("rbind", lapply(list.country$code, function(x){
        temp <- subset(dat, country == x)
        if (sum(temp$avail) == 0){
            return(data.frame(country = x, s.year = NA, e.year = NA))
        }
        else {
            temp <- temp[temp$avail != 0, ]
            return(data.frame(country = x, s.year = min(temp$year), e.year = max(temp$year)))
        }
    }))
    colnames(dat)[2:3] <- c("year.start", "year.end")
    return(dat)
}

avail.gopd.tab <- avail.to.table(check.pd.avail(gopd, "go.p", c("g", "bts", "hts")), c("g", "bts", "hts"))
avail.gopd.tab.alt <- avail.to.table(check.pd.avail(gopd.alt, "go.p", c("g", "cs", "ps")), c("g", "cs", "ps"))
avail.gopd.tab.gs <- avail.to.table(check.pd.avail(gopd.gs, "go.p", c("g", "s"), num.ind = 2), c("g", "s"))


###################################
## A.2. STAN VAPD (35 countries) ##
###################################

## Check: whether three VA vars show up for industry categories needed for all years.
cov.vapd <- check.coverage(dat.vapd, va.vars)

## Industry coverage
dat.vapd.ind <- subset(dat.vapd, ind %in% list.ind)
cov.vapd.ind <- check.coverage.ind(dat.vapd.ind, stan.country, list.ind, years)

## Countries with all coverage (industry and years): 22 countries
names(cov.vapd.ind) <- stan.country
(country.vapd.ind <- stan.country[unlist(lapply(cov.vapd.ind, function(dat) prod(dat[, years.x]) == 1))])

## For countries with all needed information, do the 3-sector conversion
dat.vapd.ind.all <- subset(dat.vapd, country %in% country.vapd.ind & ind %in% list.ind)
dat.vapd.ind.all <- clean.stan(dat.vapd.ind.all)

## Convert it to three sectors.
vapd.ind.all <- oecd.stan.auto(dat.vapd.ind.all, "VALU", "VALK", "VALP", list.re, list.orig, country.vapd.ind, w.gopd = FALSE)

vapd.ind.all.alt <- oecd.stan.auto(dat.vapd.ind.all, "VALU", "VALK", "VALP", list.re.alt, list.orig.alt, country.vapd.ind, w.gopd = FALSE)

vapd.ind.all.gs <- oecd.stan.auto(dat.vapd.ind.all, "VALU", "VALK", "VALP", list.re.gs, list.orig.gs, country.vapd.ind, w.gopd = FALSE)

## Manual check: 16 countries
(country.vapd.manual <- setdiff(stan.country, country.vapd.ind))

## Check countries with missing entries
cov.vapd.manual <- cov.vapd.ind[country.vapd.manual]
cov.vapd.manual <- which.missing(cov.vapd.manual, years.x)

## Print them out and check
print.check.2 <- function(ex.num){
    print(cov.vapd.manual[[ex.num]])
    print(cov.vapd.manual[[ex.num]]$ind)
}

## 1) CAN: can't
print.check.2(1)

## 2) COL: can't
print.check.2(2)

## 3) CRI: can't
print.check.2(3)

## 4) ISL
print.check.2(4)

## 4-a) Do without 97, 98
list.bts.isl <- setdiff(list.bts, "D97T98")
list.ind.isl.a <- c(list.g, list.bts.isl, list.hts)
dat.isl.a <- get.data.special(dat.vapd, "ISL", list.ind.isl.a)

get.vapd.special <- function(dat, list.re, list.inds) pi.qi.agg.reclass(dat, list.re, list.inds, years.x, "X2015", "VALU", "VALK", "VALP", "laspeyres")

vapd.isl <- get.vapd.special(dat.isl.a, list.re, list(list.ind.isl.a, list.g, list.bts.isl, list.hts))

## 4-b) 90 ~ 99
list.cs.isl <- c(setdiff(list.cs, c("D90T93", "D94T96", "D97T98")), "D90T99")
list.ind.isl <- c(list.g, list.cs.isl, list.ps)

dat.isl <- get.data.special(dat.vapd, "ISL", list.ind.isl)

vapd.isl.alt <- get.vapd.special(dat.isl, list.re.alt, list(list.ind.isl, list.g, list.cs.isl, list.ps))

vapd.isl.gs <- get.vapd.special(dat.isl, list.re.gs, list(list.ind.isl, list.g, c(list.cs.isl, list.ps)))

## 5) TUR: No first three years; but I do it with UN AMA.
print.check.2(5)


## 6) USA: Same with GOPD
print.check.2(6)

## 6-a)
dat.usa.a <- get.data.special(dat.vapd, "USA", list.ind)
vapd.usa <- get.vapd.special(dat.usa.a, list.re, list(list.ind, list.g, list.bts, list.hts))
gopd.usa[, c("X1995", "X1996")] <- NA

## 6-b)
dat.usa <- get.data.special(dat.vapd, "USA", list.ind.usa)

vapd.usa.alt <- pi.qi.agg.reclass(dat.usa, list.re.alt, list(list.ind.usa, list.g, list.cs.usa, list.ps), years.x, "X2015", "VALU", "VALK", "VALP", "fisher")

vapd.usa.gs <- pi.qi.agg.reclass(dat.usa, list.re.gs, list(list.ind.usa, list.g, c(list.cs.usa, list.ps)), years.x, "X2015", "VALU", "VALK", "VALP", "fisher")

## 7) AUS 94~96, 97~98 missing.
print.check.2(7)

## 7-a) Not possible.
## 7-b) 90 ~ 99
list.cs.aus <- list.cs.isl
list.ind.aus <- list.ind.isl

dat.aus <- get.data.special(dat.vapd, "AUS", list.ind.aus)
vapd.aus.alt <- get.vapd.special(dat.aus, list.re.alt, list(list.ind.aus, list.g, list.cs.aus, list.ps))
vapd.aus.gs <- get.vapd.special(dat.aus, list.re.gs, list(list.ind.aus, list.g, c(list.cs.aus, list.ps)))

## 8) CHE: same strategy as GOPD.
print.check.2(8)

## 8-a): 97~17 with full details
dat.che.a <- get.data.special(dat.vapd, "CHE", list.ind)

vapd.che <- get.vapd.special(dat.che.a, list.re, list(list.ind, list.g, list.bts, list.hts))

vapd.che[, c("X1995", "X1996", "X2018")] <- NA

## 8-b) (difference with documentation) G, H, I information missing in 2018. Other than last year, doable. it's the same with GOPD
dat.che <- get.data.special(dat.vapd, "CHE", list.ind.che)
vapd.che.alt <- get.vapd.special(dat.che, list.re.alt, list(list.ind.che, list.g, list.cs.che, list.ps.che))
vapd.che.alt$X2018 <- NA

vapd.che.gs <- get.vapd.special(dat.che, list.re.gs, list(list.ind.che, list.g, c(list.cs.che, list.ps.che)))
vapd.che.gs$X2018 <- NA

## 9) CHL: can't
print.check.2(9)

## 10) GBR: 97~ 98 problem.
print.check.2(10)

## 10-a) do it without 97~98
list.bts.gbr <- setdiff(list.bts, "D97T98")
list.ind.gbr.a <- c(list.g, list.bts.gbr, list.hts)
dat.gbr.a <- get.data.special(dat.vapd, "GBR", list.ind.gbr.a)

vapd.gbr <- get.vapd.special(dat.gbr.a, list.re, list(list.ind.gbr.a, list.g, list.bts.gbr, list.hts))

## 10-b): 90 ~ 99
list.cs.gbr <- list.cs.isl
list.ind.gbr <- list.ind.isl

dat.gbr <- get.data.special(dat.vapd, "GBR", list.ind.gbr)
vapd.gbr.alt <- get.vapd.special(dat.gbr, list.re.alt, list(list.ind.gbr, list.g, list.cs.gbr, list.ps))
vapd.gbr.gs <- get.vapd.special(dat.gbr, list.re.gs, list(list.ind.gbr, list.g, c(list.cs.gbr, list.ps)))

## 11) ISR: can't
print.check.2(11)

## 12) JPN: Same with GOPD.
print.check.2(12)
print.check(4)

## 12-a) Can't
## 12-b)
dat.jpn <- get.data.special(dat.vapd, "JPN", list.ind.jpn)
vapd.jpn.alt <- get.vapd.special(dat.jpn, list.re.alt, list(list.ind.jpn, list.g, list.cs.jpn, list.ps.jpn))
vapd.jpn.gs <- get.vapd.special(dat.jpn, list.re.gs, list(list.ind.jpn, list.g, c(list.cs.jpn, list.ps.jpn)))

## 13) KOR:
print.check.2(13)

## 13-a): can't

## 13-b) 90 ~ 99
list.cs.kor <- list.cs.isl
list.ind.kor <- list.ind.isl

dat.kor <- get.data.special(dat.vapd, "KOR", list.ind.kor)
vapd.kor.alt <- get.vapd.special(dat.kor, list.re.alt, list(list.ind.kor, list.g, list.cs.kor, list.ps))
vapd.kor.gs <- get.vapd.special(dat.kor, list.re.gs, list(list.ind.kor, list.g, c(list.cs.kor, list.ps)))

## 14) LVA: 97~98 issue
print.check.2(14)
print.check(5)

## 14-a) LVA: Do it without D97T98
list.bts.lva <- setdiff(list.bts, "D97T98")
list.ind.lva.a <- c(list.g, list.bts.lva, list.hts)
dat.lva.a <- get.data.special(dat.vapd, "LVA", list.ind.lva.a)
vapd.lva <- get.vapd.special(dat.lva.a, list.re, list(list.ind.lva.a, list.g, list.bts.lva, list.hts))

## 14-b) LVA: 90 ~ 99, Same with GOPD.
dat.lva <- get.data.special(dat.vapd, "LVA", list.ind.lva)
vapd.lva.alt <- get.vapd.special(dat.lva, list.re.alt, list(list.ind.lva, list.g, list.cs.lva, list.ps))
vapd.lva.gs <- get.vapd.special(dat.lva, list.re.gs, list(list.ind.lva, list.g, c(list.cs.lva, list.ps)))

## 15) NOR: 2018, 90 ~ 99
print.check.2(15)

## 15-a) Do it without 2018
dat.nor.a <- get.data.special(dat.vapd, "NOR", list.ind)
vapd.nor <- get.vapd.special(dat.nor.a, list.re, list(list.ind, list.g, list.bts, list.hts))
vapd.nor$X2018 <- NA

## 15-b) 90 ~ 99
list.cs.nor <- list.cs.isl
list.ind.nor <- list.ind.isl

dat.nor <- get.data.special(dat.vapd, "NOR", list.ind.nor)
vapd.nor.alt <- get.vapd.special(dat.nor, list.re.alt, list(list.ind.nor, list.g, list.cs.nor, list.ps))
vapd.nor.gs <- get.vapd.special(dat.nor, list.re.gs, list(list.ind.nor, list.g, c(list.cs.nor, list.ps)))

## 16) NZL: can't
print.check.2(16)

## Merge the auto + manual data
## 28 countries
country.vapd.final <- c(country.vapd.ind, "ISL", "USA", "CHE", "GBR", "LVA", "NOR")
length(country.vapd.final)

vapd <- rbind(vapd.ind.all, vapd.isl, vapd.usa, vapd.che, vapd.gbr, vapd.lva, vapd.nor)
vapd$var <- ifelse(vapd$var == "VALU", "v", ifelse(vapd$var == "VALK", "q", ifelse(vapd$var == "VALP", "p", "")))


## Merge the vapd data
## 31 countries (CHE last year missing so 30 + 1)
country.vapd.final.alt <- c(country.vapd.ind, "ISL", "USA", "AUS", "CHE", "GBR", "JPN", "KOR", "LVA", "NOR")
length(country.vapd.final.alt)

vapd.alt <- rbind(vapd.ind.all.alt, vapd.isl.alt, vapd.usa.alt, vapd.aus.alt, vapd.che.alt, vapd.gbr.alt, vapd.jpn.alt, vapd.kor.alt, vapd.lva.alt, vapd.nor.alt)
vapd.alt$var <- ifelse(vapd.alt$var == "VALU", "v", ifelse(vapd.alt$var == "VALK", "q", ifelse(vapd.alt$var == "VALP", "p", "")))

## 31 for G-S
country.vapd.final.gs <- country.vapd.final.alt

vapd.gs <- rbind(vapd.ind.all.gs, vapd.isl.gs, vapd.usa.gs, vapd.aus.gs, vapd.che.gs, vapd.gbr.gs, vapd.jpn.gs, vapd.kor.gs, vapd.lva.gs, vapd.nor.gs)
vapd.gs$var <- ifelse(vapd.gs$var == "VALU", "v", ifelse(vapd.gs$var == "VALK", "q", ifelse(vapd.gs$var == "VALP", "p", "")))

## All 23 gopd countries pop up for vapd. 8 are new ones. These 23, you can check VAPD vs GOPD.
length(country.gopd.final)
length(country.vapd.final)
setdiff(country.vapd.final, country.gopd.final)
setdiff(country.gopd.final, country.vapd.final)

rm(list = c("dat.vapd", "dat.vapd.ind", "dat.vapd.ind.all"))

avail.vapd.tab <- avail.to.table(check.pd.avail(vapd, "p", c("g", "bts", "hts")), c("g", "bts", "hts"))
avail.vapd.tab.alt <- avail.to.table(check.pd.avail(vapd.alt, "p", c("g", "cs", "ps")), c("g", "cs", "ps"))
avail.vapd.tab.gs <- avail.to.table(check.pd.avail(vapd.gs, "p", c("g", "s"), num.ind = 2), c("g", "s"))

##################################
## A.3. VAPD to GOPD Conversion ##
##################################

## Using the countries with GOPD, let's see whether utilizing double deflation method makes sense
## Strategy: compare actual GOPDs with imputed GOPDs

## Load icio to get the input-output structure of a country.
load("../../output/cleaned_data/cleaned_icio.RData", verbose = TRUE)
list.re.wt <- c("g", "bts", "hts") # List of reclassified sector without total

## Function to convert VAPD to GOPD
## Double deflation method
vapd.from.gopd <- function(v.dat, gopd, list.re.wt, num.ind){ # v.dat is the single country input-output table
    stopifnot(v.dat$country.ind == c(pasted("ALL", list.re.wt), "OUTPUT", "VALU"))
    v.dat$country.ind <- NULL
    mat.gopd <- rbind(replicate(num.ind, gopd), gopd, rep(NA, length(gopd)))
    q.dat <- v.dat/mat.gopd
    q.dat[length(gopd) + 2, ] <- q.dat[length(gopd) + 1, ] - colSums(q.dat[1:length(gopd), ])
    return(v.dat[length(gopd) + 2, ]/q.dat[length(gopd) + 2, ])    
}

## Distance from an arbitrary VAPD from the VAPD constructed from double deflation.
dist.vapd <- function(v.dat, gopd, vapd, list.re.wt, num.ind) sum((vapd.from.gopd(v.dat, gopd, list.re.wt, num.ind) - vapd)^2)

## Preallocation
prealloc <- function(country.final, list.re.wt, num.ind){
    gopd.con <- data.frame(country = rep(country.final, each = num.ind), ind = rep(list.re.wt, length(country.final)), var = "go.p.con")
    gopd.con[, years.x] <- NA
    return(gopd.con)
}

## Main function: VAPD to GOPD conversion
convert.vapd.gopd <- function(country.gopd.final, list.re.wt, num.ind = 3, icio.years, vapd){

    gopd.constructed <- prealloc(country.gopd.final, list.re.wt, num.ind)
    
    for (i.year in 1:length(icio.years)){ # no need for the base year, but just included it.
        icio <- icio.years[[i.year]] 
        icio <- row.agg.country(icio, "ALL", list.country$code)
        for (i.country in 1:length(country.gopd.final)){
            ex.country <- country.gopd.final[i.country]
            temp <- icio[, colnames(icio) == "country.ind" | (name.country(colnames(icio)) == ex.country & name.ind(colnames(icio)) %in% list.re.wt)]
            temp <- temp[match(c(pasted("ALL", list.re.wt), "OUTPUT", "VALU"), temp$country.ind), c("country.ind", pasted(ex.country, list.re.wt))]
            vapd.country <- vapd[vapd$country == ex.country & vapd$var == "p", ]
            vapd.country <- vapd.country[match(list.re.wt, vapd.country$ind), years.x[i.year]]
            if(prod(!is.na(vapd.country)) == 1){
                vapd.min <- function(gopd) dist.vapd(temp, gopd, vapd.country, list.re.wt, num.ind)
                res <- optim(vapd.country, vapd.min)
                stopifnot(res$convergence == 0)
                gopd.constructed[gopd.constructed$country == ex.country, years.x[i.year]] <- res$par
            }
        }
    }
    return(gopd.constructed)
}

gopd.constructed <- convert.vapd.gopd(country.gopd.final, list.re.wt, 3, icio.years, vapd)

rm("icio.years")

## Check the performance of the conversion in three way to check their comovement and levels.
## 1. correlation, 2. regression GOPD ~ \beta VOPD and check whether \beta is closer to one. 3. RMSE

## Correlation of sectoral price deflators for a country
cor.country.ind.pd <- function(name.ind, name.country, dat.1, var.1, dat.2, var.2){
    dat.1 <- subset(dat.1, ind == name.ind & country == name.country & var == var.1)
    dat.1 <- dat.1[, years.x]
    dat.2 <- subset(dat.2, ind == name.ind & country == name.country & var == var.2)
    dat.2 <- dat.2[, years.x]
    return(cor(t(dat.1), t(dat.2), use = "pairwise.complete.obs"))
}

## Compare VA PI and GO PI and VA
cor.go.va <- do.call("rbind", lapply(country.gopd.final, function(ex.country)
    do.call("rbind", lapply(list.re.wt, function(ex.ind){
    temp <- data.frame(ex.country, ex.ind, cor.country.ind.pd(ex.ind, ex.country, gopd, "go.p", vapd, "p"), cor.country.ind.pd(ex.ind, ex.country, gopd, "go.p", gopd.constructed, "go.p.con"))
    colnames(temp) <- c("country", "ind", "cor.gopd.vapd", "cor.gopd.con")
    return(temp)
    }))))

cor.go.va$better <- cor.go.va$cor.gopd.con > cor.go.va$cor.gopd.vapd
subset(cor.go.va, ind == "g")
subset(cor.go.va, country == "FRA") # France's goods PD correction: very good.

## RMSE?
rmse.country.ind.pd <- function(name.ind, name.country, dat.1, var.1, dat.2, var.2){
    dat.1 <- subset(dat.1, ind == name.ind & country == name.country & var == var.1)
    dat.1 <- dat.1[, years.x]
    dat.2 <- subset(dat.2, ind == name.ind & country == name.country & var == var.2)
    dat.2 <- dat.2[, years.x]
    gap <- dat.1 - dat.2
    gap <- gap[!is.na(gap)] #Empty ones remove
    res <- sqrt(sum(gap^2)/length(gap))
    return(res)
}

rmse.go.va <- do.call("rbind", lapply(country.gopd.final, function(ex.country)
    do.call("rbind", lapply(list.re.wt, function(ex.ind){
    temp <- data.frame(ex.country, ex.ind, rmse.country.ind.pd(ex.ind, ex.country, gopd, "go.p", vapd, "p"), rmse.country.ind.pd(ex.ind, ex.country, gopd, "go.p", gopd.constructed, "go.p.con"))
    colnames(temp) <- c("country", "ind", "rmse.gopd.vapd", "rmse.gopd.con")
    return(temp)
    }))))

sum(cor.go.va$cor.gopd.con > cor.go.va$cor.gopd.vapd)
sum(cor.go.va$cor.gopd.con < cor.go.va$cor.gopd.vapd)
sum(rmse.go.va$rmse.gopd.con < rmse.go.va$rmse.gopd.vapd)
sum(rmse.go.va$rmse.gopd.con > rmse.go.va$rmse.gopd.vapd)

## Beta, wheather they are equal. (coef and stderr)
beta.country.ind.pd <- function(name.ind, name.country, dat.1, var.1, dat.2, var.2){
    dat.1 <- subset(dat.1, ind == name.ind & country == name.country & var == var.1)
    dat.1 <- dat.1[, years.x]
    dat.2 <- subset(dat.2, ind == name.ind & country == name.country & var == var.2)
    dat.2 <- dat.2[, years.x]
    temp <- lm(t(dat.1) ~ t(dat.2) - 1)
    return(coef(summary(temp))[1:2])
}

beta.go.va <- do.call("rbind", lapply(country.gopd.final, function(ex.country)
    do.call("rbind", lapply(list.re.wt, function(ex.ind){
    temp <- data.frame(ex.country, ex.ind, t(beta.country.ind.pd(ex.ind, ex.country, gopd, "go.p", vapd, "p")), t(beta.country.ind.pd(ex.ind, ex.country, gopd, "go.p", gopd.constructed, "go.p.con")))
    colnames(temp) <- c("country", "ind", "beta.gopd.vapd", "beta.gopd.vapd.std", "beta.gopd.con", "beta.gopd.con.std")
    return(temp)
    }))))

beta.go.va$better <- abs(beta.go.va$beta.gopd.con - 1) < abs(beta.go.va$beta.gopd.vapd - 1)
subset(beta.go.va, ind == "g")
subset(beta.go.va, country == "FRA")
nrow(subset(beta.go.va, ind == "g"))
sum(subset(beta.go.va, ind == "g")$better) # 18 out of 23 countries, the measure improved.

## Table for the three measures. 
aa <- cor.go.va[, c("country", "ind", "cor.gopd.vapd", "cor.gopd.con")]
aa$cor.improve <- ifelse(aa$cor.gopd.con > aa$cor.gopd.vapd, "Y", "N")
bb <- rmse.go.va
bb$rmse.improve <- ifelse(bb$rmse.gopd.con < bb$rmse.gopd.vapd, "Y", "N")
cc <- beta.go.va[, c("country", "ind", "beta.gopd.vapd", "beta.gopd.con")]
cc$beta.improve <- ifelse(abs(cc$beta.gopd.con -1) < abs(cc$beta.gopd.vapd -1), "Y", "N")
res.gopd.vapd <- merge(aa, bb, by = c("country", "ind"))
res.gopd.vapd <- merge(res.gopd.vapd, cc, by = c("country", "ind"))

nrow(res.gopd.vapd)
sum(res.gopd.vapd$cor.improve == "Y")
sum(res.gopd.vapd$rmse.improve == "Y")
sum(res.gopd.vapd$beta.improve == "Y")

writeLines(paste("Among", nrow(res.gopd.vapd), "country-sectors,", sum(res.gopd.vapd$cor.improve == "Y"), ",", sum(res.gopd.vapd$rmse.improve == "Y"), ", and", sum(res.gopd.vapd$beta.improve == "Y"), "improved based on correlation, RMSE, and regression coefficients, respectively."), "../../doc/nums/imputed_gopd_vs_vapd.txt")

## For brevity, print country-sectors for which VAPD is a bad proxy for GOPD.
worst.country.sectors <- c(order(res.gopd.vapd$cor.gopd.vapd)[1:10],order(-res.gopd.vapd$rmse.gopd.vapd)[1:10],order(-abs(res.gopd.vapd$beta.gopd.vapd -1))[1:10])
worst.country.sectors <- unique(worst.country.sectors)

res.gopd.vapd.worst <- res.gopd.vapd[worst.country.sectors, ]
res.gopd.vapd.worst <- res.gopd.vapd.worst[order(res.gopd.vapd.worst$ind), ]
res.gopd.vapd.worst <- res.gopd.vapd.worst[order(res.gopd.vapd.worst$country), ]
rownames(res.gopd.vapd.worst) <- NULL
res.gopd.vapd.worst$ind <- toupper(res.gopd.vapd.worst$ind)

## Save the result in a TeX table
sink(file = "../../doc/tables/gopd_vapd_conversion_worst.tex")
print(xtable(res.gopd.vapd.worst),floating = FALSE)
sink()

## Summary stats
select.cols <- c("cor.gopd.vapd", "cor.gopd.con", "rmse.gopd.vapd", "rmse.gopd.con", "beta.gopd.vapd", "beta.gopd.con")
res.g <- res.gopd.vapd[res.gopd.vapd$ind == "g", select.cols]

sum.gopd.vapd <- res.gopd.vapd %>%
    group_by(ind) %>%
    reframe(stats = c("Mean", "Q1", "Q2", "Q3"),
              cor.vapd = c(mean(cor.gopd.vapd), quantile(cor.gopd.vapd, c(0.25, 0.5, 0.75))),
              cor.con = c(mean(cor.gopd.con), quantile(cor.gopd.con, c(0.25, 0.5, 0.75))),
              rmse.vapd = c(mean(rmse.gopd.vapd), quantile(rmse.gopd.vapd, c(0.25, 0.5, 0.75))),
              rmse.con = c(mean(rmse.gopd.con), quantile(rmse.gopd.con, c(0.25, 0.5, 0.75))),
              beta.vapd = c(mean(beta.gopd.vapd), quantile(beta.gopd.vapd, c(0.25, 0.5, 0.75))),
              beta.con = c(mean(beta.gopd.con), quantile(beta.gopd.con, c(0.25, 0.5, 0.75)))
            )

sum.gopd.vapd$ind <- toupper(sum.gopd.vapd$ind)
sum.gopd.vapd$ind <- factor(sum.gopd.vapd$ind, levels = c("G", "HTS", "BTS"))
sum.gopd.vapd <- sum.gopd.vapd[order(sum.gopd.vapd$ind), ]

## Save the result in a TeX table
sink(file = "../../doc/tables/gopd_vapd_conversion_summary.tex")
print(xtable(sum.gopd.vapd),floating = FALSE)
sink()

## Example plot for Slovakia-goods

ex.country <- "SVK" ## Nice plots: SVK, FIN, FRA for goods
ex.ind <- "g"
dat.country <- rbind(subset(gopd,
                            country == ex.country & ind == ex.ind & var == "go.p"),
                     subset(vapd,
                            country == ex.country & ind == ex.ind & var == "p"),
                     subset(gopd.constructed,
                            country == ex.country & ind == ex.ind & var == "go.p.con"))

dat.country <- pivot_longer(dat.country, cols = paste0("X", years), names_to = "year")
dat.country$year <- as.numeric(substr(dat.country$year, 2,5))
dat.country$var <- ifelse(dat.country$var == "go.p", "GO",
                   ifelse(dat.country$var == "p", "VA",
                   ifelse(dat.country$var == "go.p.con", "GO (imputed)", "")))

## Here the warning say that 2 rows have missing values, this is because for GOPD, Slovakia has yet to report 2017 and 2018.
pdf(file = "../../doc/figures/gopd_vapd_svk_g.pdf", width = 10, height = 6)
ggplot(dat.country, aes(x = year, y = value, color = var)) +
    geom_point(aes(shape = var), size = 2.5) +
    geom_line(aes(linetype = var), linewidth = 1) +
    geom_dl(aes(label = var), method = list(dl.trans(x = x - 0.2), "first.points", cex = 1.4)) +
    ylab("Price deflators (2015 = 1)") +
    xlab("Year") +
    scale_x_continuous(limits = c(1991, 2018), breaks = seq(1995, 2018, 5)) +
    theme_bw() +
    theme(legend.position = "none", 
          axis.text = element_text(size = 14), 
          text = element_text(size = 16), 
          strip.text = element_text(size = 17), 
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank()) +
    guides(color = "none", shape = "none", linetype = "none")
dev.off()


###################################################################
## For A.4. and A.5., you need value-added information from ICIO ##
###################################################################

## Load each year's ICIO
read.icio.inner <- function(years, year) read.csv(unz(paste0("../../data/icio/ICIO_", years, ".zip"), paste0("ICIO2021_", year, ".csv")), header = T)

read.icio <- function(year){
    if (year %in% 1995:1999){
        dat <- read.icio.inner("1995-1999", year)
    }
    if (year %in% 2000:2004){
        dat <- read.icio.inner("2000-2004", year)
    }
    if (year %in% 2005:2009){
        dat <- read.icio.inner("2005-2009", year)
    }
    if (year %in% 2010:2014){
        dat <- read.icio.inner("2010-2014", year)
    }
    if (year %in% 2015:2018){
        dat <- read.icio.inner("2015-2018", year)
    }
    colnames(dat)[1] <- "country.ind"
    colnames(dat) <- gsub("_", "\\.", colnames(dat))
    dat$country.ind <- gsub("_", "\\.", dat$country.ind)
    return(dat)
}

## This takes some time to load the data set.
icio.years <- lapply(1995:2018, function(year) read.icio(year))

## For aggregated industries (i.e., ISIC rev.4 G-to-I and R-to-U and ISIC rev.3 G-H, J-P),
## Get the value-added share for BTS and HTS and for CS and PS.

## For HTS and BTS
## Note: 45T47 & 49T53: PS / 55T56: CS vs all three: HTS
## Note: 90T93: HTS / 94T96 & 97T98: BTS vs all three: CS
## What we need
## Share of HTS in J-to-P (rev.3) = HTS share in "58T98"
## Share of R (HTS) in R-to-U = "90T93"/("90T93" + "94T96" + "97T98")

list.re.valu <- c("45T56", "HTS_58T98", "BTS_58T98")
list.re.valu.dis <- list(c("45T47", "49", "50", "51", "52", "53", "55T56"), c("58T60", "61", "62T63", "64T66", "69T75", "77T82", "90T93"), c("68", "84", "85", "86T88", "94T96", "97T98"))

list.re.valu.2 <- c("45T88", "90T93", "94T98")
list.re.valu.dis.2 <- list(c("45T47", "49", "50", "51", "52", "53", "55T56", "58T60", "61", "62T63", "64T66", "68", "69T75", "77T82","84", "85", "86T88"), "90T93", c("94T96", "97T98")) 

## What we need for the alternative setup.
## Share of I (CS) from G-H-I (rev. 4) = share of "55T56" in "45T47" + "49T53" + "55T56"
## Share of CS in G-H (rev.3) = "55T56"/("45T47" + "55T56")
## Share of PS in J-to-P (rev.3) = PS share in "58T98"
list.re.valu.alt <- c("45T47", "49T53", "55T56", "PS_58T98", "CS_58T98")
list.re.valu.dis.alt <- list("45T47", c("49", "50", "51", "52", "53"), "55T56", c("58T60", "61", "62T63", "64T66", "69T75", "77T82"), c("68", "84", "85", "86T88", "90T93", "94T96", "97T98"))
                             
## Set of list.re.valu.dis, list.re.value.dis.alt, and list.re.value.dis.alt.2 = mutually exclusive and exhaustive set of "services industries excluding construction"?
setequal(unlist(list.re.valu.dis), unlist(list.re.valu.dis.2))
setequal(unlist(list.re.valu.dis), unlist(list.re.valu.dis.alt))
length(unlist(list.re.valu.dis)) == length(unlist(list.re.valu.dis.2))
length(unlist(list.re.valu.dis)) == length(unlist(list.re.valu.dis.alt))

prod(unlist(list.re.valu.dis) %in% unique(name.ind(colnames(icio.years[[1]]))))
list.ind <- read.csv("../../output/cleaned_data/list_industry_three.csv")
prod(unlist(list.re.valu.dis) %in% list.ind$code[26:nrow(list.ind)])
length(unlist(list.re.valu.dis)) == nrow(list.ind) - 25

## Value-added information for the reclassified more aggregate sectors  
valu.agg <- function(list.re.valu, list.re.valu.dis)
    lapply(icio.years, function(dat){
    dat <- col.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- col.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))
    dat <- row.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- row.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))
    dat <- dat[dat$country.ind == "VALU", ]
    for (i in 1:length(list.re.valu)){
        if (!identical(list.re.valu[i], list.re.valu.dis[[i]])){
            dat <- col.agg.ind(dat, list.re.valu[i], list.re.valu.dis[[i]])
        }
    }
    dat <- dat[, sapply(list.country$code, function(x) pasted(x, list.re.valu))]
    return(dat)
})

icio.valu <- valu.agg(list.re.valu, list.re.valu.dis)
icio.valu.2 <- valu.agg(list.re.valu.2, list.re.valu.dis.2)
icio.valu.alt <- valu.agg(list.re.valu.alt, list.re.valu.dis.alt)

rm("icio.years")

for (i in 1:length(icio.valu)){
    icio.valu[[i]][, "year"] <- years.x[i]
    icio.valu.2[[i]][, "year"] <- years.x[i]
    icio.valu.alt[[i]][, "year"] <- years.x[i]
}

icio.valu <- do.call("rbind", lapply(icio.valu, function(dat){
    dat <- pivot_longer(dat, cols = -year, names_sep = "\\.", names_to = c("country", "ind"))
    dat <- pivot_wider(dat, names_from = "ind", values_from = "value")
    ## For A.5, I need to split between HTS vs BTS
    ## HTS share from J-P.rev3
    dat[, "HTS.share.JP.rev3"] <- dat[, "HTS_58T98"]/(dat[, "HTS_58T98"] + dat[, "BTS_58T98"])
    dat <- dat[, c("country", "year", "HTS.share.JP.rev3")]
    return(dat)    
}))

icio.valu.2 <- do.call("rbind", lapply(icio.valu.2, function(dat){
    dat <- pivot_longer(dat, cols = -year, names_sep = "\\.", names_to = c("country", "ind"))
    dat <- pivot_wider(dat, names_from = "ind", values_from = "value")
    ## For A.4, I need to split RTU into R and STU  (90-93:HTS vs 94-98:BTS)
    ## Share of R (HTS) from RTU
    dat[, "R.HTS.share.RTU"] <- dat[, "90T93"]/(dat[, "90T93"] + dat[, "94T98"])
    dat <- dat[, c("country", "year", "R.HTS.share.RTU")]
    return(dat)    
}))

icio.valu.alt <- do.call("rbind", lapply(icio.valu.alt, function(dat){
    dat <- pivot_longer(dat, cols = -year, names_sep = "\\.", names_to = c("country", "ind"))
    dat <- pivot_wider(dat, names_from = "ind", values_from = "value")
    ## For A.4, I need to split G-H-I into G-H and I (45~53:PS, vs 55 & 56:CS)
    ## Share of I (CS) from GTI
    dat[, "I.CS.share.GTI"] <- dat[, "55T56"]/(dat[, "45T47"] + dat[, "49T53"] + dat[, "55T56"])
    ## For A.5, I need to split 45 ~ 47 vs 55 & 56 and PS vs CS
    ## H (CS) share from GH.rev3
    dat[, "H.CS.share.GH.rev3"] <- dat[, "55T56"]/(dat[, "45T47"] + dat[, "55T56"])
    ## PS share from J-P.rev3
    dat[, "PS.share.JP.rev3"] <- dat[, "PS_58T98"]/(dat[, "PS_58T98"] + dat[, "CS_58T98"])
    dat <- dat[, c("country", "year", "I.CS.share.GTI", "H.CS.share.GH.rev3", "PS.share.JP.rev3")]
    return(dat)    
}))

## This we need for BTS-HTS division
icio.valu.R.HTS.RTU <- pivot_wider(icio.valu.2[, c("country", "year", "R.HTS.share.RTU")], names_from = "year", values_from = "R.HTS.share.RTU")
icio.valu.HTS.JP <- pivot_wider(icio.valu[, c("country", "year", "HTS.share.JP.rev3")], names_from = "year", values_from = "HTS.share.JP.rev3")

## This we need for CS-PS division
icio.valu.I.CS.GTI <- pivot_wider(icio.valu.alt[, c("country", "year", "I.CS.share.GTI")], names_from = "year", values_from = "I.CS.share.GTI")
icio.valu.H.CS.GH <- pivot_wider(icio.valu.alt[, c("country", "year", "H.CS.share.GH.rev3")], names_from = "year", values_from = "H.CS.share.GH.rev3")
icio.valu.PS.JP <- pivot_wider(icio.valu.alt[, c("country", "year", "PS.share.JP.rev3")], names_from = "year", values_from = "PS.share.JP.rev3")

#################################
## A.4. UN_MAIN Data from SDMX ##
#################################

## For the UN DATA, NAs are created. So you don't need to worry about recycling of a vector.

## Constant and current price value-added 
## Source: https://data.un.org/SdmxBrowser/start -> download as tabular data (National Accounts -> NA_MAIN)
## Another alternative: OECD data, but the coverage is better here.
un.na <- read.csv("../../data/prices/un_na_main/ESTAT+NA_MAIN+1.9_2022_04_05_22_06_03.csv")
colnames(un.na) <- gsub("_", "\\.", tolower(colnames(un.na)))

## Remove unnecessary columns
## Columns that are all equal.
list.check <- c("freq", "dataflow", "adjustment", "counterpart.area", "ref.sector", "counterpart.sector", "accounting.entry", "sto", "instr.asset", "expenditure", "unit.measure", "transformation", "conf.status", "comment.obs", "embargo.date","repyearend", "time.format", "title", "title.compl", "unit.mult", "comment.dset", "comment.ts", "pre.break.value", "data.comp", "currency")
unique(un.na[, list.check])
un.na[, list.check] <- NULL

## For detailed explanation regarding codes, eurostat has information
## In Eurostat, MAIN_NA is "IDCM : National accounts, Main aggregates in the International Data Cooperation TF context (7678)"
## https://sdw.ecb.europa.eu/datastructure.do?datasetinstanceid=353

## Missing observations: remove them
table(un.na$obs.status)
prod(is.na(subset(un.na, obs.status %in% c("L", "M"))[, "obs.value"]))
sum(is.na(un.na$obs.value))
nrow(un.na)
un.na <- subset(un.na, !obs.status %in% c("L", "M"))

## Print to check other unnecessary variables
table(un.na$ref.period.detail) ## Fiscal year start month, C: calendar year
table(un.na$repyearstart) ## KZ, LA are the reason for these I guess
table(un.na$time.per.collect)
table(un.na$table.identifier)
table(un.na$compiling.org)
table(un.na$diss.org)

un.na[, c("obs.status", "ref.period.detail", "repyearstart", "time.per.collect", "table.identifier", "compiling.org", "diss.org", "decimals", "last.update")] <- NULL

## Select only necessary variables
un.na <- clean.colnames(un.na, c("ref.area", "time.period", "obs.value", "ref.year.price"), c("country", "year", "value", "base"))

unique(un.na$prices) # L: Chain-linked volume, LR: rebased L, Q: constant prices, V: current prices, Y: previous year prices
unique(un.na$country)

## ISO alpha 2 to alpha 3 conversion
iso.2.3 <- read.csv("../../data/icio/alpha3_2.csv")
iso.2.3$country <- NULL
colnames(iso.2.3) <- c("country", "alpha2")

un.na$alpha2 <- un.na$country
un.na$country <- NULL
un.na$country <- iso.2.3$country[match(un.na$alpha2, iso.2.3$alpha2)]
un.na$alpha2 <- NULL

## Check availability
avail.country <- do.call("rbind", lapply(unique(un.na$country), function(ex.country){
    temp <- subset(un.na, country == ex.country)
    temp2 <- subset(temp, activity == "A") # To check sector disaggregation
    if (nrow(temp2) == 0){
        temp2 <- data.frame(year = 9999)
    }
    return(data.frame(country = ex.country, start.year = min(temp2$year), end.year = max(temp2$year), list.activity = I(list(unique(temp$activity))), list.variables = I(list(unique(temp$prices)))))}))

avail.country$full.years <- (avail.country$start.year <= 1995 & avail.country$end.year >= 2018)

## What type of sectoral disaggregation does it have?
type.activity <- data.frame(list.activity = I(unique(avail.country$list.activity)), type = 1:length(unique(avail.country$list.activity)))

avail.country$type <- sapply(avail.country$list.activity, function(x) type.activity$type[match(list(x), type.activity$list.activity)])

## 2nd type is the majority.
table(avail.country$type)
table(avail.country$type[avail.country$full.years])

## Countries that need special attentions
avail.country$country[(!avail.country$full.years) | (avail.country$type != 2)]

## Countries that can be easily done.
avail.country$country[(avail.country$full.years)]

## What you need to do. conversion for 2 ~ 7
## The only information needed for type 2,3,4,7 conversion: G, H, I split. How much is G, H and how much is I?
## For 2,3,7, for BTS-HTS, RTU split is needed.

## ISIC rev. 4.
## A (Agri) BTE (Industry), F (Construction), G (wholesale and retail trade, repair of motor vehicles), H (transportation and storage), I (accomodation and food services), J (Information and communication), K (Financial and insurance), L (Real estate), M (Professional, scientific and technical activities), N (administrative and support service activities), OTQ (Public administration, defence, education, human health and social work activities), RTU (Arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies)

## Goods: A, BTE
## Producer services: GTI*(G and H's share), J, K, M_N
## Consumer services: F, GTI (I's share), L, OTQ, RTU

## Highly tradable services: GTI, J, K, M_N, M_N, RTU*(R's share)
## Barely tradable services: F, L, OTQ, RTU*(STU's share)

un.na <- subset(un.na, year %in% 1995:2018 & prices %in% c("L", "LR", "V"))

## ARG and MMR only reports economy-total and KAZ and LAO (type 5,6) do not report quantities.
un.na <- subset(un.na, !country %in% c("ARG", "MMR", "KAZ", "LAO"))

## For each country is it single base year? YES
prod(unlist(lapply(unique(un.na$country), function(x){
    dat <- subset(un.na, country == x)
    return(length(unique(dat$base[!is.na(dat$base)])) %in% c(0,1))
})))

un.na$base <- NULL

## Rebased L (LR) into L
un.na <- pivot_wider(un.na, names_from = c("prices"))
sum(!is.na(un.na$L) & !is.na(un.na$LR))
miss.L <- is.na(un.na$L)
un.na$L[miss.L] <- un.na$LR[miss.L]
un.na$LR[miss.L] <- NA
sum(!is.na(un.na$LR))
un.na$LR <- NULL

## Rename columns into volume and quantity
un.na <- clean.colnames(un.na, c("L", "V"), c("q", "v"))
un.na$p <- un.na$v/un.na$q
un.na$year <- paste0("X", un.na$year)
un.na <- pivot_longer(un.na, cols = c("v", "q", "p"), names_to = "var")
un.na <- pivot_wider(un.na, names_from = "year", values_from = "value")
un.na <- clean.colnames(un.na, "activity", "ind")
un.na <- un.na[, c("country", "ind", "var", years.x)]

## For countries in types 2,3,4,7, need split GTI into G_H and I.
## For countries in types 2,3,7, need split RTU into R and STU
## Note one exception: CHL is type 2 but "RTU" is missing
subset(un.na, country == "CHL" & ind == "RTU")

split.ind <- function(dat, ind.origin, ind.dest.1, ind.dest.2, dat.weight.2, with.p = TRUE){
    for (ex.country in unique(dat[, "country", drop = TRUE])){
        temp <- subset(dat, country == ex.country & ind == ind.origin)
        if (nrow(temp) > 0){
            dat <- subset(dat, !(country == ex.country & ind == ind.origin))
    
            temp.2.ratio <- subset(dat.weight.2, country == ex.country)[, years.x]
            temp.1.v <- temp[temp$var == "v", years.x]*(1-temp.2.ratio)
            temp.1.q <- temp[temp$var == "q", years.x]*(1-temp.2.ratio)
            temp.2.v <- temp[temp$var == "v", years.x]*temp.2.ratio
            temp.2.q <- temp[temp$var == "q", years.x]*temp.2.ratio
            if (with.p){
                temp.1.p <- temp.2.p <- temp[temp$var == "p", years.x]            
                temp.new <- data.frame(ind = c(rep(ind.dest.1,3), rep(ind.dest.2, 3)), country = ex.country, var = rep(c("v", "q", "p"),2))
                temp.new <- cbind(temp.new, rbind(temp.1.v, temp.1.q, temp.1.p, temp.2.v, temp.2.q, temp.2.p))
                dat <- rbind(dat, temp.new)
            }
            else {
                temp.new <- data.frame(ind = c(rep(ind.dest.1,2), rep(ind.dest.2, 2)), country = ex.country, var = rep(c("v", "q"),2))
                temp.new <- cbind(temp.new, rbind(temp.1.v, temp.1.q, temp.2.v, temp.2.q))
                dat <- rbind(dat, temp.new)
            }
        }
    }
    return(dat)    
}

un.na <- split.ind(un.na, "GTI", "G_H", "I", icio.valu.I.CS.GTI)
un.na <- split.ind(un.na, "RTU", "STU", "R", icio.valu.R.HTS.RTU)

## Function to do the price aggregation
conversion.un <- function(dat.price, var.x, var.q, var.p, list.re, list.disagg, list.countries){
    res <- do.call("rbind",
                   lapply(list.countries,
                          function(ex.country){
                              if (ex.country %in% c("USA", "CAN")){
                                  return(pi.qi.agg.reclass(subset(dat.price, country == ex.country), list.re, list.disagg, paste0("X", 1995:2018), "X2015", var.x, var.q, var.p, "fisher"))
                              }
                              else{
                                  return(pi.qi.agg.reclass(subset(dat.price, country == ex.country), list.re, list.disagg, paste0("X", 1995:2018), "X2015", var.x, var.q, var.p, "laspeyres"))
                              }
                          }
                          )
                   )
    return(res)
}

## Type 2 conversion
avail.country$country[avail.country$type == 2]

vapd.un.2 <- conversion.un(un.na, "v", "q", "p", c("total", "g", "bts", "hts"), list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ", "STU"), c("G_H", "I", "J", "K", "M_N", "R")), setdiff(avail.country$country[avail.country$type == 2], "CHL"))

vapd.un.2.alt <- conversion.un(un.na, "v", "q", "p", c("total", "g", "cs", "ps"), list(c("_T"), c("A", "BTE"), c("I", "F", "L", "OTQ", "R", "STU"), c("G_H", "J", "K", "M_N")), setdiff(avail.country$country[avail.country$type == 2], "CHL"))

vapd.un.2.gs <- conversion.un(un.na, "v", "q", "p", c("total", "g", "s"), list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ", "STU", "G_H", "I", "J", "K", "M_N", "R")), setdiff(avail.country$country[avail.country$type == 2], "CHL"))

avail.country[avail.country$country == "CHL", ]

vapd.un.chl <- conversion.un(un.na, "v", "q", "p", c("total", "g", "bts", "hts"), list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ"), c("G_H", "I", "J", "K", "M_N")), "CHL")

vapd.un.chl.alt <- conversion.un(un.na, "v", "q", "p", c("total", "g", "cs", "ps"), list(c("_T"), c("A", "BTE"), c("I", "F", "L", "OTQ"), c("G_H", "J", "K", "M_N")), "CHL")

vapd.un.chl.gs <- conversion.un(un.na, "v", "q", "p", c("total", "g", "s"), list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ", "G_H", "I", "J", "K", "M_N")), "CHL")

## Type 3 conversion (China is the only country for this case)

avail.country[avail.country$type == 3, ]

vapd.un.3 <- conversion.un(un.na, "v", "q", "p", c("total", "g", "bts", "hts"), list(c("_T"), c("A", "BTE"), c("F", "L", "STU"), c("G_H", "I", "K", "R")), avail.country$country[avail.country$type == 3])

vapd.un.3.alt <- conversion.un(un.na, "v", "q", "p", c("total", "g", "cs", "ps"), list(c("_T"), c("A", "BTE"), c("I", "F", "L", "R", "STU"), c("G_H", "K")), avail.country$country[avail.country$type == 3])

vapd.un.3.gs <- conversion.un(un.na, "v", "q", "p", c("total", "g", "s"), list(c("_T"), c("A", "BTE"), c("F", "L", "STU", "G_H", "I", "K", "R")), avail.country$country[avail.country$type == 3])

## Type 4 conversion (India is the only country for this case)

avail.country[avail.country$type == 4, ]

vapd.un.4 <- conversion.un(un.na, "v", "q", "p", c("total", "g", "bts", "hts"), list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ"), c("G_H", "I")), avail.country$country[avail.country$type == 4])

vapd.un.4.alt <- conversion.un(un.na, "v", "q", "p", c("total", "g", "cs", "ps"), list(c("_T"), c("A", "BTE"), c("I", "F", "L", "OTQ"), c("G_H")), avail.country$country[avail.country$type == 4])

vapd.un.4.gs <- conversion.un(un.na, "v", "q", "p", c("total", "g", "s"),  list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ", "G_H", "I")), avail.country$country[avail.country$type == 4])

## Type 7 conversion (SAU, ZAF)
avail.country[avail.country$type == 7, ]

vapd.un.7 <- conversion.un(un.na, "v", "q", "p", c("total", "g", "bts", "hts"), list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ", "STU"), c("G_H", "I", "J", "K", "R")), avail.country$country[avail.country$type == 7])

vapd.un.7.alt <- conversion.un(un.na, "v", "q", "p", c("total", "g", "cs", "ps"), list(c("_T"), c("A", "BTE"), c("I", "F", "L", "OTQ", "R", "STU"), c("G_H", "J", "K")), avail.country$country[avail.country$type == 7])

vapd.un.7.gs <- conversion.un(un.na, "v", "q", "p", c("total", "g", "s"), list(c("_T"), c("A", "BTE"), c("F", "L", "OTQ", "STU", "G_H", "I", "J", "K", "R")), avail.country$country[avail.country$type == 7])

## Merge the results
vapd.un <- rbind(vapd.un.2, vapd.un.chl, vapd.un.3, vapd.un.4, vapd.un.7)
vapd.un.alt <- rbind(vapd.un.2.alt, vapd.un.chl.alt, vapd.un.3.alt, vapd.un.4.alt, vapd.un.7.alt)
vapd.un.gs <- rbind(vapd.un.2.gs, vapd.un.chl.gs, vapd.un.3.gs, vapd.un.4.gs, vapd.un.7.gs)

country.vapd.un.final <- unique(vapd.un$country)

rm("un.na")

avail.vapd.un.sdmx.tab <- avail.vapd.un.sdmx.tab.alt <- avail.vapd.un.sdmx.tab.gs <- avail.to.table(check.pd.avail(vapd.un, "p", c("g", "bts", "hts")), c("g", "bts", "hts"))

######################
## A.5. UN AMA DATA ##
######################

## UN AMA dataset does not contain iso alpha32 or alpha 3 country name, but only full name.
## OECD ICIO, UN AMA name difference adjustment
list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.country.oecd.un <- data.frame(name.oecd = c("Czech Republic - Czechia", "Korea", "Slovak Republic", "China (People's Republic of)", "Hong Kong, China", "Lao People's Democratic Republic"), name.un =  c("Czechia", "Republic of Korea", "Slovakia", "China", "China, Hong Kong SAR", "Lao People's DR"))
list.country <- merge(list.country, list.country.oecd.un, by.x = "name", by.y = "name.oecd", all.x = TRUE)
list.country$name.un[is.na(list.country$name.un)] <- list.country$name[is.na(list.country$name.un)]
list.country$name <- NULL

## Load current price VA.
## UN data, its base year is also 2015.
dat.cons <- read.xlsx("../../data/prices/un_ama/Download-GDPconstant-NCU-countries.xlsx", sheet = "Download-GDPconstant-NCU-countr", startRow = 3)
dat.curr <- read.xlsx("../../data/prices/un_ama/Download-GDPcurrent-NCU-countries.xlsx", sheet = "Download-GDPcurrent-NCU-countri", startRow = 3)

## Clean the dataset.
convert.ind <- data.frame(ind.original = c("Agriculture, hunting, forestry, fishing (ISIC A-B)", "Mining, Manufacturing, Utilities (ISIC C-E)", "Construction (ISIC F)", "Wholesale, retail trade, restaurants and hotels (ISIC G-H)", "Transport, storage and communication (ISIC I)", "Other Activities (ISIC J-P)", "Total Value Added"), ind = c("A-B", "C-E", "F", "G-H", "I", "J-P", "TOTAL"))

clean.un <- function(dat){
    colnames(dat) <- tolower(colnames(dat))
    dat <- subset(dat, country %in% list.country$name.un & indicatorname %in% convert.ind$ind.original)
    colnames(dat)[1:4] <- c("country.num", "name.un", "currency", "ind.original")
    colnames(dat)[5:ncol(dat)] <- paste0("X", colnames(dat)[5:ncol(dat)])
    dat <- dat[, c(colnames(dat)[1:4], years.x)]
    dat <- merge(dat, list.country, by = "name.un", all.x = TRUE) # Change country name to iso alpha 3.
    dat$name.un <- NULL
    dat$country <- dat$code
    dat$code <- NULL
    dat <- merge(dat, convert.ind, by = "ind.original", all.x = TRUE) # Change industry name
    return(dat[, c("country", "ind", years.x)])
}

dat.cons <- clean.un(dat.cons)
dat.curr <- clean.un(dat.curr)

## I have information for all countries except Taiwan.
list.not.un <- list.country$name.un[!list.country$code %in% unique(dat.cons$country)]
list.not.un
nrow(list.country) - length(list.not.un) ## 65 countries

## Merge constant and current one.
dat.curr$var <- "v"
dat.cons$var <- "q"
dat.un <- rbind(dat.curr, dat.cons)
rm(list = c("dat.curr", "dat.cons"))

## Split J-P (Rev.3) into BTS and HTS part
dat.un.alt <- dat.un

dat.un <- split.ind(dat.un, "J-P", "J-P.BTS", "J-P.HTS", icio.valu.HTS.JP, with.p = FALSE)

## Split G-H (Rev.3) to G and H and split J-P (Rev.3) into PS and CS part.
dat.un.alt <- split.ind(dat.un.alt, "G-H", "G", "H", icio.valu.H.CS.GH, with.p = FALSE)
dat.un.alt <- split.ind(dat.un.alt, "J-P", "J-P.CS", "J-P.PS", icio.valu.PS.JP, with.p = FALSE)

## Derive deflators.
get.pd <- function(dat.un){
    dat.un <- pivot_longer(dat.un, cols = paste0("X", 1995:2018), names_to = "year")
    dat.un <- pivot_wider(dat.un, names_from = var, values_from = "value")
    dat.un$p <- dat.un$v/dat.un$q
    dat.un <- pivot_longer(dat.un, cols = c("v", "q", "p"), names_to = "var")
    dat.un <- pivot_wider(dat.un, names_from = year, values_from = "value")
    return(dat.un)
}

dat.un <- get.pd(dat.un)
dat.un.alt <- get.pd(dat.un.alt)

## list.reclass <- c("total", "g", "cs", "ps")
## list.reclass.inds <- list(c("TOTAL"), c("A-B", "C-E"), c("F", "H", "J-P.CS"), c("I", "G", "J-P.PS"))

vapd.un.ama <- conversion.un(dat.un, "v", "q", "p", c("total", "g", "bts", "hts"), list(c("TOTAL"), c("A-B", "C-E"), c("F", "J-P.BTS"), c("I", "G-H", "J-P.HTS")), unique(dat.un$country))

vapd.un.ama.alt <- conversion.un(dat.un.alt, "v", "q", "p", c("total", "g", "cs", "ps"), list(c("TOTAL"), c("A-B", "C-E"), c("F", "H", "J-P.CS"), c("I", "G", "J-P.PS")), unique(dat.un.alt$country))

vapd.un.ama.gs <- conversion.un(dat.un.alt, "v", "q", "p", c("total", "g", "s"), list(c("TOTAL"), c("A-B", "C-E"), c("F", "H", "J-P.CS", "I", "G", "J-P.PS")), unique(dat.un.alt$country))

avail.vapd.un.ama.tab <- avail.vapd.un.ama.tab.alt <- avail.vapd.un.ama.tab.gs <- avail.to.table(check.pd.avail(vapd.un.ama, "p", c("g", "bts", "hts")), c("g", "bts", "hts"))


#################
## A.6. TAIWAN ##
#################

## Load data for Taiwan from National Statistics of the Republic of China. Units: million NT$
filename.twn <- "../../data/prices/taiwan/生產帳_年_對外發布版(040)_eng.xlsx"
dat.twn.v <- read.xlsx(filename.twn, sheet = "Gross Domestic Product ", startRow = 4)
dat.twn.q <- read.xlsx(filename.twn, sheet = "GDP(chained dollars)", startRow = 4)
dat.twn.p <- read.xlsx(filename.twn, sheet = "Price Deflators for GDP", startRow = 4)

for (year in as.character(1981:2020)){
    dat.twn.p[, year][dat.twn.p[, year] == "-"] <- NA # Statistical discrepancy and value-added tax
    dat.twn.p[, year] <- as.numeric(dat.twn.p[, year])
}

clean.twn <- function(dat, name.var){
    colnames(dat)[1] <- "ind"
    dat$ind <- gsub("^[[:blank:]]*", "", dat$ind)
    dat <- subset(dat, substr(ind,1,2) %in% paste0(toupper(letters[1:19]), ".")) # A ~ S sectors
    dat$ind <- substr(dat$ind,1,1)
    dat$var <- name.var
    dat$country <- "TWN"
    dat$currency <- "New Taiwan Dollar"
    colnames(dat)[colnames(dat) %in% as.character(1981:2020)] <- paste0("X", colnames(dat)[colnames(dat) %in% as.character(1981:2020)])
    return(dat)
}

dat.twn.v <- clean.twn(dat.twn.v, "v")
dat.twn.q <- clean.twn(dat.twn.q, "q")
dat.twn.p <- clean.twn(dat.twn.p, "p")

dat.twn.p[, paste0("X", 1981:2020)] <- dat.twn.p[, paste0("X", 1981:2020)]/100

dat.twn <- rbind(dat.twn.v, dat.twn.q, dat.twn.p)
rm(list = c("dat.twn.v", "dat.twn.q", "dat.twn.p"))

## Derive deflators for 1995 ~ 2018.
dat.twn[ paste0("X", c(1981:1994, 2019:2020))] <- NULL

vapd.twn <- pi.qi.agg.reclass(dat.twn, c("total", "g", "bts", "hts"), list(toupper(letters[1:19]), toupper(letters[1:5]), c("F", "L", "O", "P", "Q", "S"), c("I", "R", "G", "H", "J", "K", "M", "N")), paste0("X", 1995:2018), "X2015", "v", "q", "p", "laspeyres")

vapd.twn.alt <- pi.qi.agg.reclass(dat.twn, c("total", "g", "cs", "ps"), list(toupper(letters[1:19]), toupper(letters[1:5]), c("F", "I", "L", "O", "P", "Q", "R", "S"), c("G", "H", "J", "K", "M", "N")), paste0("X", 1995:2018), "X2015", "v", "q", "p", "laspeyres")

vapd.twn.gs <- pi.qi.agg.reclass(dat.twn, c("total", "g", "s"), list(toupper(letters[1:19]), toupper(letters[1:5]), c("F", "I", "L", "O", "P", "Q", "R", "S", "G", "H", "J", "K", "M", "N")), paste0("X", 1995:2018), "X2015", "v", "q", "p", "laspeyres")

##################################################################
## A.7. Check whether the less accurate VAPD's are good proxies ##
##################################################################

list.reclass <- c("total", "g", "bts", "hts")

cor.va.stan.un.unama <- do.call("rbind", lapply(country.vapd.final, function(ex.country)
    do.call("rbind", lapply(list.reclass, function(ex.ind){
        temp <- data.frame(ex.country, ex.ind, cor.country.ind.pd(ex.ind, ex.country, vapd, "p", vapd.un, "p"), cor.country.ind.pd(ex.ind, ex.country, vapd, "p", vapd.un.ama, "p"))
        colnames(temp) <- c("country", "ind", "cor.vapd.stan.un", "cor.vapd.stan.unama")
        return(temp)
    }))))

beta.va.stan.un.unama <- do.call("rbind", lapply(country.vapd.final, function(ex.country)
    do.call("rbind", lapply(list.reclass, function(ex.ind){
        temp <- data.frame(ex.country, ex.ind, t(beta.country.ind.pd(ex.ind, ex.country, vapd, "p", vapd.un, "p")), t(beta.country.ind.pd(ex.ind, ex.country, vapd, "p", vapd.un.ama, "p")))
        colnames(temp) <- c("country", "ind", "beta.vapd.stan.un", "std.vapd.stan.un", "beta.vapd.stan.unama", "std.vapd.stan.unama")
        return(temp)
    }))))

rmse.va.stan.un.unama <- do.call("rbind", lapply(country.vapd.final, function(ex.country)
    do.call("rbind", lapply(list.reclass, function(ex.ind){
        temp <- data.frame(ex.country, ex.ind, rmse.country.ind.pd(ex.ind, ex.country, vapd, "p", vapd.un, "p"), rmse.country.ind.pd(ex.ind, ex.country, vapd, "p", vapd.un.ama, "p"))
    colnames(temp) <- c("country", "ind", "rmse.vapd.stan.un", "rmse.vapd.stan.unama")
    return(temp)
    }))))

bb <- beta.va.stan.un.unama[, c("country", "ind", "beta.vapd.stan.un", "beta.vapd.stan.unama")]

res.vapd.detail <- merge(cor.va.stan.un.unama, bb, by = c("country", "ind"))
res.vapd.detail <- merge(res.vapd.detail, rmse.va.stan.un.unama, by = c("country", "ind"))

res.vapd.detail <- subset(res.vapd.detail, ind != "total")
res.vapd.detail$ind <- toupper(res.vapd.detail$ind)
res.vapd.detail$ind <- factor(res.vapd.detail$ind, levels = c("G", "HTS", "BTS"))

sum.vapd.detail <- res.vapd.detail %>%
    group_by(ind) %>%
    reframe(stats = c("Mean", "Q1", "Q2", "Q3"),
              cor.vapd.un = c(mean(cor.vapd.stan.un), quantile(cor.vapd.stan.un, c(0.25, 0.5, 0.75))),
              cor.vapd.unama = c(mean(cor.vapd.stan.unama), quantile(cor.vapd.stan.unama, c(0.25, 0.5, 0.75))),
              rmse.vapd.un = c(mean(rmse.vapd.stan.un), quantile(rmse.vapd.stan.un, c(0.25, 0.5, 0.75))),
              rmse.vapd.unama = c(mean(rmse.vapd.stan.unama), quantile(rmse.vapd.stan.unama, c(0.25, 0.5, 0.75))),
              beta.vapd.un = c(mean(beta.vapd.stan.un), quantile(beta.vapd.stan.un, c(0.25, 0.5, 0.75))),
              beta.vapd.unama = c(mean(beta.vapd.stan.unama), quantile(beta.vapd.stan.unama, c(0.25, 0.5, 0.75)))
              )

sink(file = "../../doc/tables/vapd_detail_imputed_summary.tex")
print(xtable(sum.vapd.detail),floating = FALSE)
sink()

#####################################################
## B.1. Constructing the full VA PD from VAPD data ##
#####################################################


## Accuracy: vapd (stan + twn) > vapd.un (UN NA_MAIN) > vapd.un.ama

## Filling in the empty cells

## check.no.empty.che <- function(dat) prod(!is.na(dat[dat$country != "CHE", years.x]))
## check.no.empty.che(vapd)
## check.no.empty.che(vapd.alt)
## check.no.empty.che(vapd.gs)

## Adding Taiwan
vapd <- rbind(vapd, vapd.twn)
vapd.alt <- rbind(vapd.alt, vapd.twn.alt)
vapd.gs <- rbind(vapd.gs, vapd.twn.gs)

country.vapd.final <- c(country.vapd.final, "TWN")
country.vapd.final.alt <- c(country.vapd.final.alt, "TWN")
country.vapd.final.gs <- c(country.vapd.final.gs, "TWN")

## Subsetting only "p" variables
subset.p <- function(dat) dat[dat$var == "p", ]
vapd.final <- subset.p(vapd)
vapd.final.alt <- subset.p(vapd.alt)
vapd.final.gs <- subset.p(vapd.gs)

vapd.un <- subset.p(vapd.un)
vapd.un.alt <- subset.p(vapd.un.alt)
vapd.un.gs <- subset.p(vapd.un.gs)

vapd.un.ama <- subset.p(vapd.un.ama)
vapd.un.ama.alt <- subset.p(vapd.un.ama.alt)
vapd.un.ama.gs <- subset.p(vapd.un.ama.gs)

## 29 countries (G-BTS-HTS) for baseline, 32 countries (G-CS-PS and G-S)
nrow(vapd.final)/4
nrow(vapd.final.gs)/3
nrow(vapd.final.alt)/4

## Function to update missing entries using the second best VAPDs
## e.g. NOR 2017~2018 price increase information is not available in OECD STAN, obtain it from UN SDMX

update.2 <- function(list.country, list.re, vapd.1, vapd.2){

    vapd.1.updated <- subset(vapd.1, country %in% list.country)

    for (ex.country in list.country){
        for (i.ind in list.re){
            ## Case where not every entry is provided in "vapd.1",
            if (!(prod(!is.na(vapd.1[vapd.1$country == ex.country & vapd.1$ind == i.ind & vapd.1$var == "p", years.x])) == 1)){
                print(paste0(ex.country, "-", i.ind))
                na.years.ind <- as.numeric(is.na(vapd.1[vapd.1$country == ex.country & vapd.1$ind == i.ind & vapd.1$var == "p", years.x]))
                ## index that first entry pops up
                ind.first <- which(c(0, diff(na.years.ind)) == -1)
                if (length(ind.first) == 1){ # length = 0 means that there was no missing entry in the first part of the time period.
                    na.years.first <- years.x[1:ind.first]
                    vapd.1.updated[vapd.1.updated$country == ex.country & vapd.1.updated$ind == i.ind & vapd.1.updated$var == "p", na.years.first] <- vapd.2[vapd.2$country == ex.country & vapd.2$ind == i.ind & vapd.2$var == "p", na.years.first]/vapd.2[vapd.2$country == ex.country & vapd.2$ind == i.ind & vapd.2$var == "p", years.x[ind.first]]*vapd.1[vapd.1$country == ex.country & vapd.1$ind == i.ind & vapd.1$var == "p", years.x[ind.first]] # Price sequences from UN AMA/ UN AMA first entry year * UN NA_MAIN first entry year
            }
                ## index that last entry pops up
                ind.last <- which(c(diff(na.years.ind), 0) == 1)
                if (length(ind.last) == 1){
                    na.years.last <- years.x[ind.last:length(years.x)]
                    vapd.1.updated[vapd.1.updated$country == ex.country & vapd.1.updated$ind == i.ind & vapd.1.updated$var == "p", na.years.last] <- 
                        vapd.2[vapd.2$country == ex.country & vapd.2$ind == i.ind & vapd.2$var == "p", na.years.last]/vapd.2[vapd.2$country == ex.country & vapd.2$ind == i.ind & vapd.2$var == "p", years.x[ind.last]]*vapd.1[vapd.1$country == ex.country & vapd.1$ind == i.ind & vapd.1$var == "p", years.x[ind.last]]
                }
            }
        }
    }
    return(vapd.1.updated)
}

## Missing years for countries
## NOR and CHE: missing entries in STAN VA
subset(avail.vapd.tab, year.start != 1995 | year.end != 2018)

## Update VAPD STAN using VAPD UN SMDX
vapd.final <- update.2(country.vapd.final, list.re, vapd.final, vapd.un)
subset(vapd.final, country %in% c("NOR", "CHE"))

## Update VAPD STAN using VAPD UN AMA
vapd.final <- update.2(country.vapd.final, list.re, vapd.final, vapd.un.ama)
subset(vapd.final, country %in% c("CHE"))

## Check whether they are full
check.full <- function(dat) prod(unlist(lapply(years.x, function(x) prod(!is.na(dat[, x])))))

check.full(vapd.final)
nrow(vapd.final)

## Alternative specifications
subset(avail.vapd.tab.alt, year.start != 1995 | year.end != 2018)
vapd.final.alt <- update.2(country.vapd.final.alt, list.re.alt, vapd.final.alt, vapd.un.alt)
check.full(vapd.final.alt)

subset(avail.vapd.tab.gs, year.start != 1995 | year.end != 2018)
vapd.final.gs <- update.2(country.vapd.final.gs, list.re.gs, vapd.final.gs, vapd.un.gs)
check.full(vapd.final.gs)


## Add UN AMA to countries where UN NA_MAIN does not contain full year information.

## For countries not in "vapd", use UN NA_MAIN and fill in the blanks using UN AMA
country.vapd.un.only <- setdiff(country.vapd.un.final, country.vapd.final)
length(country.vapd.un.only) # 22 countries

country.vapd.un.only.alt <- setdiff(country.vapd.un.final, country.vapd.final.alt)
length(country.vapd.un.only.alt) # 19 countries

country.vapd.un.only.gs <- setdiff(country.vapd.un.final, country.vapd.final.gs)
length(country.vapd.un.only.gs)
setequal(country.vapd.un.only.alt, country.vapd.un.only.gs)

## Check whether middle years are empty
## No middle years are empty, missing years are just at the front or at the end
check.mid.empty <- function(list.country, list.re, dat.price){
    for (ex.country in list.country){
        for (i.ind in list.re){
            stopifnot(sum(abs(diff(as.numeric(is.na(dat.price[dat.price$country == ex.country & dat.price$ind == i.ind & dat.price$var == "p", years.x])), collapse = ""))) < 3)
        }
    }
}

check.mid.empty(country.vapd.un.only, list.re, vapd.un)
check.mid.empty(country.vapd.un.only.gs, list.re.gs, vapd.un)
check.mid.empty(country.vapd.un.only.alt, list.re.alt, vapd.un)

## Update missing VAPD from UN NA_MAIN using AMA.
## UN AMA: full
subset(avail.vapd.un.ama.tab, year.start != 1995 | year.end != 2018)
subset(avail.vapd.un.ama.tab.alt, year.start != 1995 | year.end != 2018)
subset(avail.vapd.un.ama.tab.gs, year.start != 1995 | year.end != 2018)

subset(avail.vapd.un.sdmx.tab, year.start != 1995 | year.end != 2018)
vapd.un.updated <- update.2(country.vapd.un.only, list.re, vapd.un, vapd.un.ama)
check.full(vapd.un.updated)

subset(avail.vapd.un.sdmx.tab.alt, year.start != 1995 | year.end != 2018)
vapd.un.updated.alt <- update.2(country.vapd.un.only.alt, list.re.alt, vapd.un.alt, vapd.un.ama.alt)
check.full(vapd.un.updated.alt)

subset(avail.vapd.un.sdmx.tab.gs, year.start != 1995 | year.end != 2018)
vapd.un.updated.gs <- update.2(country.vapd.un.only.gs, list.re.gs, vapd.un.gs, vapd.un.ama.gs)
check.full(vapd.un.updated.gs)

## Sanity check for one country
ex.country <- "CAN"

subset(vapd.un.updated, country == ex.country & var == "p" & ind == "bts")
subset(vapd.un, country == ex.country & var == "p" & ind == "bts")
subset(vapd.un.ama, country == ex.country & var == "p" & ind == "bts")

subset(vapd.un.updated.alt, country == ex.country & var == "p" & ind == "cs")
subset(vapd.un.alt, country == ex.country & var == "p" & ind == "cs")
subset(vapd.un.ama.alt, country == ex.country & var == "p" & ind == "cs")

## Add it to the data (countries in STAN + UN NA_MAIN)
nrow(vapd.final)/4
vapd.final <- rbind(vapd.final, vapd.un.updated)
nrow(vapd.final)/4 # 29 + ?? countries

nrow(vapd.final.alt)/4
vapd.final.alt <- rbind(vapd.final.alt, vapd.un.updated.alt)
nrow(vapd.final.alt)/4 # 23 + 28 countries

nrow(vapd.final.gs)/3
vapd.final.gs <- rbind(vapd.final.gs, vapd.un.updated.gs)
nrow(vapd.final.gs)/3 # 32 + 19 countries

## Countries in OECD STAN + UN MAIN augmented with UN AMA are the same across three specifications.
aa <- unique(vapd.final$country)
setequal(aa, unique(vapd.final.gs$country))
setequal(aa, unique(vapd.final.alt$country))

## For the rest of the countries, use UN_AMA
country.vapd.unama.only <- setdiff(list.country$code, c(aa, "ROW")) ## 15 countries

vapd.final <- rbind(vapd.final, subset(vapd.un.ama, country %in% country.vapd.unama.only))

nrow(vapd.final) == 66*4

vapd.final.gs <- rbind(vapd.final.gs, subset(vapd.un.ama.gs, country %in% country.vapd.unama.only))

nrow(vapd.final.gs) == 66*3

vapd.final.alt <- rbind(vapd.final.alt, subset(vapd.un.ama.alt, country %in% country.vapd.unama.only))

nrow(vapd.final.alt) == 66*4

##############################
## B.2. Conversion to GO PD ##
##############################

## For validity of this exercise, see A.3.

## Load icio to get the input-output structure of a country.
load("../../output/cleaned_data/cleaned_icio.RData", verbose = TRUE)
load("../../output/cleaned_data/cleaned_icio_alt.RData", verbose = TRUE)
load("../../output/cleaned_data/cleaned_icio_gs.RData", verbose = TRUE)

list.re.wt <- c("g", "bts", "hts") # List of reclassified sector without total
list.re.wt.alt <- c("g", "cs", "ps") 
list.re.wt.gs <- c("g", "s")

## Conversion from VAPD to GOPD
country.final <- setdiff(list.country$code, "ROW")

gopd.con <- convert.vapd.gopd(country.final, list.re.wt, 3, icio.years, vapd.final)
gopd.con.alt <- convert.vapd.gopd(country.final, list.re.wt.alt, 3, icio.years.alt, vapd.final.alt)
gopd.con.gs <- convert.vapd.gopd(country.final, list.re.wt.gs, 2, icio.years.gs, vapd.final.gs)

rm("icio.years")
rm("icio.years.alt")
rm("icio.years.gs")

## For countries with reported GOPD, use them, but for missing years, complement them with constructed GOPD.

subset(avail.gopd.tab, year.start != 1995 | year.end != 2018)

gopd <- subset(gopd, var == "go.p" & ind != "total")
gopd$var <- "p"

gopd.con <- subset(gopd.con, var == "go.p.con")
gopd.con$var <- "p"

gopd <- update.2(country.gopd.final, list.re.wt, gopd, gopd.con)

check.full(gopd)

## Alternative specifications
subset(avail.gopd.tab.alt, year.start != 1995 | year.end != 2018)

gopd.alt <- subset(gopd.alt, var == "go.p" & ind != "total")
gopd.alt$var <- "p"
gopd.con.alt <- subset(gopd.con.alt, var == "go.p.con")
gopd.con.alt$var <- "p"
gopd.alt <- update.2(country.gopd.final.alt, list.re.wt.alt, gopd.alt, gopd.con.alt)
check.full(gopd.alt)

subset(avail.gopd.tab.gs, year.start != 1995 | year.end != 2018)

gopd.gs <- subset(gopd.gs, var == "go.p" & ind != "total")
gopd.gs$var <- "p"
gopd.con.gs <- subset(gopd.con.gs, var == "go.p.con")
gopd.con.gs$var <- "p"
gopd.gs <- update.2(country.gopd.final.gs, list.re.wt.gs, gopd.gs, gopd.con.gs)
check.full(gopd.gs)


## As with Robert Inklaar's recommendation, countries that use single deflation, use VAPD, those that use double deflation, utilize the constructed ones.

## List of countries with single and double deflation
list.sd <- read.csv("../../data/others/single_double_deflation.csv")
list.sd <- list.sd[, c(1,3)]

## Adding them
add.vapd.gopd.con <- function(gopd, gopd.con, vapd){
    country.gg <- unique(gopd$country)
    country.gc <- setdiff(list.sd$code[list.sd$s_d == "d"], country.gg)
    country.vv <- setdiff(list.sd$code[list.sd$s_d == "s"], country.gg)
    ## print(country.gg)
    ## print(country.gc)
    ## print(country.vv)
    gopd.final <-
        rbind(gopd,
              subset(gopd.con, country %in% country.gc),
              subset(vapd, country %in% country.vv))
    return(gopd.final)
}

vapd.final <- subset(vapd.final, ind != "total")
gopd.final <- add.vapd.gopd.con(gopd, gopd.con, vapd.final)

vapd.final.gs <- subset(vapd.final.gs, ind != "total")
gopd.final.gs <- add.vapd.gopd.con(gopd.gs, gopd.con.gs, vapd.final.gs)

vapd.final.alt <- subset(vapd.final.alt, ind != "total")
gopd.final.alt <- add.vapd.gopd.con(gopd.alt, gopd.con.alt, vapd.final.alt)

nrow(gopd.final) == length(country.final) * 3
nrow(gopd.final.gs) == length(country.final) * 2
nrow(gopd.final.alt) == length(country.final) * 3

############################
## C. USD price deflators ##
############################

## For steps A and B, everything was in local currency units. Change them into USDs.

## NER (how many local currencies per USD) that OECD ICIO uses
ner <- read.xlsx("../../data/icio/ReadMe_ICIO2021_CSV.xlsx", sheet = "NCU-USD")
ner[, c(1,3)] <- NULL
ner <- ner[1:67, ] ## Remove footnotes
colnames(ner)[1] <- c("code")

## Change in NER relative to base year 2015
for (year in setdiff(as.character(1995:2018), "2015")){
    ner[, year] <- ner[, year]/ner[, "2015"]
}
ner[, "2015"] <- 1
ner <- pivot_longer(ner, cols = as.character(1995:2018), names_to = "year")
colnames(ner) <- c("country", "year", "ner.2015")
ner$year <- paste0("X", ner$year)

## Conversion of into USD
convert.usd <- function(dat, var.old, var.new){
    dat$var <- NULL
    dat <- pivot_longer(dat, cols = paste0("X", 1995:2018), names_to = "year", values_to = var.old)
    dat <- merge(dat, ner, by = c("country", "year"), all.x = TRUE)
    dat[, var.new] <- dat[, var.old]/dat[, "ner.2015"]
    return(dat)
}

gopd.final <- convert.usd(gopd.final, "p.lcu", "p.usd")
gopd.final.gs <- convert.usd(gopd.final.gs, "p.lcu", "p.usd")
gopd.final.alt <- convert.usd(gopd.final.alt, "p.lcu", "p.usd")

## Check whether all entries are full and positive
stopifnot(as.logical(prod(!is.na(gopd.final$p.usd))))
stopifnot(as.logical(prod(!is.na(gopd.final.gs$p.usd))))
stopifnot(as.logical(prod(!is.na(gopd.final.alt$p.usd))))

########################
## D. Save the result ##
########################

## Plot the availability
change.cols <- function(xx, suff){
    colnames(xx)[2:3] <- pasted(colnames(xx)[2:3], suff)
    return(xx)
    }

get.res.avail <- function(aa, bb, cc, dd){
    aa <- change.cols(aa, "go")
    bb <- change.cols(bb, "va")
    cc <- change.cols(cc, "va_UN_SDMX")
    dd <- change.cols(dd, "va_UN_AMA")
    res <- merge(aa, bb, by = "country", all = TRUE)
    res <- merge(res, cc, by = "country", all = TRUE)
    res <- merge(res, dd, by = "country", all = TRUE)
    return(res)
}

res.avail <- get.res.avail(avail.gopd.tab, avail.vapd.tab, avail.vapd.un.sdmx.tab, avail.vapd.un.ama.tab)
res.avail.gs <- get.res.avail(avail.gopd.tab.gs, avail.vapd.tab.gs, avail.vapd.un.sdmx.tab.gs, avail.vapd.un.ama.tab.gs)
res.avail.alt <- get.res.avail(avail.gopd.tab.alt, avail.vapd.tab.alt, avail.vapd.un.sdmx.tab.alt, avail.vapd.un.ama.tab.alt)

## Change 1995.00 to 1995.
ff.int <- function(res.avail){
    res.avail[colnames(res.avail) != "country"] <- lapply(res.avail[colnames(res.avail) != "country"], as.integer)
    res.avail <- subset(res.avail, country != "ROW")
    rownames(res.avail) <- NULL
    return(res.avail)
}

res.avail <- ff.int(res.avail)
res.avail.gs <- ff.int(res.avail.gs)
res.avail.alt <- ff.int(res.avail.alt)

sink(file = "../../doc/tables/avail_pd.tex")
print(xtable(res.avail),floating = FALSE)
sink()

sink(file = "../../doc/tables/avail_pd_gs.tex")
print(xtable(res.avail.gs),floating = FALSE)
sink()

sink(file = "../../doc/tables/avail_pd_alt.tex")
print(xtable(res.avail.alt),floating = FALSE)
sink()

save(gopd.final, res.avail, file = "../../output/cleaned_data/gopd.RData")

save(gopd.final.gs, res.avail.gs, file = "../../output/cleaned_data/gopd_gs.RData")

save(gopd.final.alt, res.avail.alt, file = "../../output/cleaned_data/gopd_alt.RData")
