## condense_icio_remove_tax.R
## Condense OECD ICIO into broad sectors from industries
## Remove tax terms from ICIO

## Solving package dependency with Renv
renv::load("../")

## Load functions
source("../common_functions.R")

## Load country and industry names
list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.ind <- read.csv("../../output/cleaned_data/list_industry_three.csv")
list.ind.alt <- read.csv("../../output/cleaned_data/list_industry_three_alt.csv")

#################################
## Load ICIO from 1995 to 2018 ##
#################################

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

icio.years <- lapply(1995:2018, function(year) read.icio(year))

###################
## Condense ICIO ##
###################

## Consumption terms
list.cons.full <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DPABR")
list.cons <- setdiff(list.cons.full, "DPABR") # I separate DPABR from other consumption terms. This is to deal with taxes paid to foreign governments when cleaning data.

## Sanity checks on input-output table identities before condensing it.
sanity <- function(dat, list.ind, with.tax, list.cons){
    list.country.ind <- unlist(lapply(list.country$code, function(country) pasted(country, list.ind)))
    ## Check 1. column-wise sum for production = OUTPUT?
    check.col <- max(abs(dat[dat$country.ind == "OUTPUT", colnames(dat) %in% list.country.ind] - colSums(dat[dat$country.ind != "OUTPUT", colnames(dat) %in% list.country.ind])))
    ## Check 2. row-wise sum for production, TAXSUB, VALU = TOTAL?
    check.row <- max(abs(dat$TOTAL - rowSums(dat[, !colnames(dat) %in% c("country.ind", "TOTAL")])))
    ## Check 3. input and output for production (OUTPUT = TOTAL?)
    check.in.out <- max(abs(t(dat[dat$country.ind == "OUTPUT", match(list.country.ind, colnames(dat))]) - dat$TOTAL[match(list.country.ind, dat$country.ind)]))
    ## Check 4. GDP Identity (production = expenditure)
    check.gdp <- max(sapply(list.country$code, function(x) abs(gdp.prod(x, dat, with.tax) - gdp.exp(x, dat, list.cons))))
    return(data.frame(check.col = check.col, check.row = check.row, check.in.out = check.in.out, check.gdp = check.gdp))   
}

(sanity.orig <- do.call("rbind", lapply(icio.years, function(dat) sanity(dat, list.ind$code, TRUE, list.cons.full))))

## List of reclassificed industry
list.re <- c("g", "bts", "hts")
list.re.alt <- c("g", "cs", "ps")
list.re.gs <- c("g", "s")

## Function to condense ICIO
condense.icio <- function(dat, list.re, list.ind, name.var){    
    ## Aggregate 45 industries into goods, consumer services, and producer services
    for (i.ind in list.re){
        dat <- row.agg.ind(dat, i.ind, list.ind$code[list.ind[, name.var] == i.ind])
        dat <- col.agg.ind(dat, i.ind, list.ind$code[list.ind[, name.var] == i.ind])
    }
    ## Aggregate 6 consumption columns to "c" and "DPABR"
    dat <- col.agg.ind(dat, "c", list.cons)

    ## For Mexico, production is separated for Global manufacturing, and for China, for export processing. (MX1, MX2, CN1, CN2)
    ## Consumption is only for MEX and CHN. No consumption terms for MX1, MX2, CN1, CN2
    ## Aggregate MEX, MX1, MX2 -> MEX and CHN, CN1, CN2 -> CHN
    dat <- col.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- col.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))
    dat <- row.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- row.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))

    return(dat)
}

## Condense (baseline, bts.hts, g.s versions)
icio.years.orig <- icio.years

icio.years <- lapply(icio.years.orig, function(x) condense.icio(x, list.re, list.ind, "g.bts.hts"))
icio.years.alt <- lapply(icio.years.orig, function(x) condense.icio(x, list.re.alt, list.ind.alt, "g.cs.ps"))
icio.years.gs <- lapply(icio.years.orig, function(x) condense.icio(x, list.re.gs, list.ind, "g.s"))

rm("icio.years.orig")

## Sanity check
(sanity.condense <- do.call("rbind", lapply(icio.years, function(dat) sanity(dat, list.re, TRUE, c("c", "DPABR"))))) # Sanity checks passed for all years. One thoudsand USD difference.
(sanity.condense.alt <- do.call("rbind", lapply(icio.years.alt, function(dat) sanity(dat, list.re.alt, TRUE, c("c", "DPABR")))))
(sanity.condense.gs <- do.call("rbind", lapply(icio.years.gs, function(dat) sanity(dat, list.re.gs, TRUE, c("c", "DPABR")))))

################
## Remove tax ##
################

## Removing tax while keeping GDP value and GDP identity.
## The process is to convert (output at basic prices) -> (output at purchaser's price)
## Note: GDP = Value-added + taxes less subsidies (TLS) on intermediate consumption + TLS on final consumption = C + I + G + NX in purchaser's prices
## Note: (TLS) include those on imports and exports. ICIO does not provide information on the sources of taxsub. I use proportionality assumption to separate them. For example, if consumptions are half half split across two categories, I split TAXSUB into halves as well.

adjust.tax <- function(dat, list.re){
    
    ## Adjustment 1: TLS on intermediate consumption -> value-added terms
    for(country in list.country$code){
        for(ind in list.re){
            temp <- prod.cons(country, "TAXSUB", country, ind, dat)
            ## Add TAXSUB to VALU
            dat[dat$country.ind == "VALU", pasted(country, ind)] <- dat[dat$country.ind == "VALU", pasted(country, ind)] + temp
            ## Adjust total TAXSUB (row-wise total)
            dat[dat$country.ind == pasted(country, "TAXSUB"), "TOTAL"] <- dat[dat$country.ind == pasted(country, "TAXSUB"), "TOTAL"] - temp
            ## Adjust total VALU (row-wise total)
            dat[dat$country.ind == "VALU", "TOTAL"] <- dat[dat$country.ind == "VALU", "TOTAL"] + temp
            ## TAXSUB to 0
            dat[dat$country.ind == pasted(country, "TAXSUB"), pasted(country, ind)] <- 0
        }
    }

    ## Adjustment 2
    ## TLS on final consumption -> value-added (row-wise)
    ## TLS on final consumption ->  consumption on goods and services (column-wise)
    ## Proportionality assumption: Use final consumption on domestic goods and services ratio to allocate TLS on final consumptions
    for (country in list.country$code){ # country is a producing country
        ## Adj 2.1 for domestic "c" consumption
        cons.ind <- sapply(list.re, function(ind) prod.cons(country, ind, country, "c", dat)) # consumption per industry
        cons.ind <- cons.ind/sum(cons.ind) # ratio
        adj.dom <- prod.cons(country, "TAXSUB", country, "c", dat)*cons.ind
        for (i in 1:length(list.re)){
            dat[dat$country.ind == pasted(country, list.re[i]), pasted(country, "c")] <- dat[dat$country.ind == pasted(country, list.re[i]), pasted(country, "c")] + adj.dom[i]
        }
        ## Adjust TAXSUB total (row-wise)
        dat[dat$country.ind == pasted(country, "TAXSUB"), "TOTAL"] <- dat[dat$country.ind == pasted(country, "TAXSUB"), "TOTAL"] - sum(adj.dom)       
        ## Adjust production total from basic prices to purchaser's prices (row-wise)
        for (i in 1:length(list.re)){
            dat[dat$country.ind == pasted(country, list.re[i]), "TOTAL"] <- dat[dat$country.ind == pasted(country, list.re[i]), "TOTAL"] + adj.dom[i]
        }
        ## Tax on final consumption -> 0    
        dat[dat$country.ind == pasted(country, "TAXSUB"), pasted(country, "c")] <- 0

        ## Adj 2.2 foreginers' DPABR
        ## Adjustment ratio for each foreign country's consumption
        ## Note: ROW has DPABR TAXSUB on ROW.
        ## Note: There are 0's in DPABR for some country pairs (e.g. 1999 LAO on PER and ISL)
        total.dpabr <- colSums(prod.cons(country, list.re, list.country$code, "DPABR", dat))
        adj.dpabr.list <- c()
        for (i in 1:length(list.re)){
            ind <- list.re[i]
            ratio.adj <- prod.cons(country, ind, list.country$code, "DPABR", dat)/total.dpabr
            adj.dpabr <- t(ratio.adj*prod.cons(country, "TAXSUB", list.country$code, "DPABR", dat))
            adj.dpabr[is.nan(adj.dpabr)] <- 0 # Take care of NaN's (0/0) for 0 DPABR
            adj.dpabr.list[i] <- sum(adj.dpabr)
            dat[dat$country.ind == pasted(country, ind), pasted(list.country$code, "DPABR")] <- dat[dat$country.ind == pasted(country, ind), pasted(list.country$code, "DPABR")] + adj.dpabr
            ## Adjust TAXSUB total
            dat[dat$country.ind == pasted(country, "TAXSUB"), "TOTAL"] <- dat[dat$country.ind == pasted(country, "TAXSUB"), "TOTAL"] - sum(adj.dpabr)
            ## Adjust production total
            dat[dat$country.ind == pasted(country, ind), "TOTAL"] <- dat[dat$country.ind == pasted(country, ind), "TOTAL"] + sum(adj.dpabr)
        }

        ## Tax on DPABR -> 0 
        dat[dat$country.ind == pasted(country, "TAXSUB"), pasted(list.country$code, "DPABR")] <- 0


        ## Adj 2.3: TLS on final consumption -> value-added terms
        for (i in 1:length(list.re)){
            ind <- list.re[i]
            dat[dat$country.ind == "VALU", pasted(country, ind)] <- dat[dat$country.ind == "VALU", pasted(country, ind)] + adj.dom[i] + adj.dpabr.list[i]
            ## Adjust total of value-added.
            dat[dat$country.ind == "VALU", "TOTAL"] <- dat[dat$country.ind == "VALU", "TOTAL"] + adj.dom[i] + adj.dpabr.list[i]
            ## Adjust input total (column-wise total)
            dat[dat$country.ind == "OUTPUT", pasted(country, ind)] <- dat[dat$country.ind == "OUTPUT", pasted(country, ind)] + adj.dom[i] + adj.dpabr.list[i]
            ## Adjust total of output
            dat[dat$country.ind == "OUTPUT", "TOTAL"] <- dat[dat$country.ind == "OUTPUT", "TOTAL"] + adj.dom[i] + adj.dpabr.list[i]

        }
    }
    
    ## Adjustment 3:  Countries pay for TAXSUB on other countries for domestic production and consumption. For ROW, I think it is adjustment terms to balance icio; the numbers are pretty low (often below 100,000 dollars). Also, for ROU, ARG, LTU, BRN there are export subsidies.
    for (country.exp.tax in c("ROW", "ROU", "ARG", "LTU", "BRN")){ # Exporting country
        for (country in setdiff(list.country$code, country.exp.tax)){ # Importing country
            for (ind in c(list.re, "c")){ # Importing industry + final consumption
                cons.ind <- sapply(list.re, function(ind.exp) prod.cons(country.exp.tax, ind.exp, country, ind, dat)) # Consumption of country.exp.tax per importing country-ind
                cons.ind <- cons.ind/sum(cons.ind)
                adj <- cons.ind*prod.cons(country.exp.tax, "TAXSUB", country, ind, dat)
                
                ## Column-wise adjustment
                for (i in 1:length(list.re)){
                    ind.exp <- list.re[i]
                    dat[dat$country.ind == pasted(country.exp.tax, ind.exp), pasted(country, ind)] <- dat[dat$country.ind == pasted(country.exp.tax, ind.exp), pasted(country, ind)] + adj[i]
                }
                ## TAXSUB to 0.
                dat[dat$country.ind == pasted(country.exp.tax, "TAXSUB"), pasted(country, ind)] <- 0
                ## Row-wise sum adjust (TOTAL for TAXSUB, ROW.g ROW.s)
                dat[dat$country.ind == pasted(country.exp.tax, "TAXSUB"), "TOTAL"] <- dat[dat$country.ind == pasted(country.exp.tax, "TAXSUB"), "TOTAL"] - sum(adj)
                for (i in 1:length(list.re)){
                    ind.exp <- list.re[i]
                    dat[dat$country.ind == pasted(country.exp.tax, ind.exp), "TOTAL"] <- dat[dat$country.ind == pasted(country.exp.tax, ind.exp), "TOTAL"] + adj[i]
                    ## Row-wise adjustment (Add TAXSUB to VALU) and column-wise sums
                    dat[dat$country.ind == "VALU", pasted(country.exp.tax, ind.exp)] <- dat[dat$country.ind == "VALU", pasted(country.exp.tax, ind.exp)] + adj[i]
                    ## Adjust total of value-added.
                    dat[dat$country.ind == "VALU", "TOTAL"] <- dat[dat$country.ind == "VALU", "TOTAL"] + adj[i]
                    ## Adjust input total (column-wise total)
                    dat[dat$country.ind == "OUTPUT", pasted(country.exp.tax, ind.exp)] <- dat[dat$country.ind == "OUTPUT", pasted(country.exp.tax, ind.exp)] + adj[i]
                    ## Adjust total of output
                    dat[dat$country.ind == "OUTPUT", "TOTAL"] <- dat[dat$country.ind == "OUTPUT", "TOTAL"] + adj[i]
                }
            }
        }
    }
    return(dat)
}
   
icio.years <- lapply(icio.years, function(x) adjust.tax(x, list.re))
icio.years.alt <- lapply(icio.years.alt, function(x) adjust.tax(x, list.re.alt))
icio.years.gs <- lapply(icio.years.gs, function(x) adjust.tax(x, list.re.gs))

## Sanity checks using input-output identities
(sanity.tax <- do.call("rbind", lapply(icio.years, function(dat) sanity(dat, list.re, TRUE, c("c", "DPABR")))))
(sanity.tax.alt <- do.call("rbind", lapply(icio.years.alt, function(dat) sanity(dat, list.re.alt, TRUE, c("c", "DPABR")))))
(sanity.tax.gs <- do.call("rbind", lapply(icio.years.gs, function(dat) sanity(dat, list.re.gs, TRUE, c("c", "DPABR")))))

## Check whether taxes are all removed and remove tax columns
check.tax <- function(dat) max(abs(dat[dat$country.ind %in% pasted(list.country$code, "TAXSUB"), "TOTAL"]))

unlist(lapply(icio.years, check.tax))
unlist(lapply(icio.years.alt, check.tax))
unlist(lapply(icio.years.gs, check.tax))

remove.tax <- function(dat) dat[name.ind(dat$country.ind) != "TAXSUB", ]

icio.years <- lapply(icio.years, remove.tax)
icio.years.alt <- lapply(icio.years.alt, remove.tax)
icio.years.gs <- lapply(icio.years.gs, remove.tax)

## Now merge two consumptions c and DPABR
condense.c.dpabr <- function(dat) col.agg.ind(dat, "c", c("c", "DPABR"))

icio.years <- lapply(icio.years, condense.c.dpabr)
icio.years.alt <- lapply(icio.years.alt, condense.c.dpabr)
icio.years.gs <- lapply(icio.years.gs, condense.c.dpabr)

## Reorder columns and rows
reorder.col.row <- function(dat, list.re){

    list.country.ind <- unlist(lapply(list.country$code, function(country) pasted(country, list.re)))
    list.country.ind.c <- c(unlist(lapply(list.country$code, function(country) pasted(country, list.re))), pasted(list.country$code, "c"))
    
    dim.start <- dim(dat)
    dat <- dat[c(match(list.country.ind, dat$country.ind), which(dat$country.ind == "VALU"), which(dat$country.ind == "OUTPUT")), c("country.ind", list.country.ind.c, "TOTAL")]
    dim.end <- dim(dat)
    stopifnot(identical(dim.start, dim.end))
    return(dat)
}

icio.years <- lapply(icio.years, function(x) reorder.col.row(x, list.re))
icio.years.alt <- lapply(icio.years.alt, function(x) reorder.col.row(x, list.re.alt))
icio.years.gs <- lapply(icio.years.gs, function(x) reorder.col.row(x, list.re.gs))

## Final sanity check
(sanity.final <- do.call("rbind", lapply(icio.years, function(dat) sanity(dat, list.re, FALSE, "c"))))
(sanity.final.alt <- do.call("rbind", lapply(icio.years.alt, function(dat) sanity(dat, list.re.alt, FALSE, "c"))))
(sanity.final.gs <- do.call("rbind", lapply(icio.years.gs, function(dat) sanity(dat, list.re.gs, FALSE, "c"))))

## Check whether there is a negative entry anywhere.
(check.negative <- lapply(icio.years, function(dat){
    temp <- dat
    temp$country.ind <- NULL
    return(dat[apply(temp, 1, function(x) sum(x < 0) > 0), c(TRUE, apply(temp, 2, function(x) sum(x < 0) > 0))])}))

(check.negative.alt <- lapply(icio.years.alt, function(dat){
    temp <- dat
    temp$country.ind <- NULL
    return(dat[apply(temp, 1, function(x) sum(x < 0) > 0), c(TRUE, apply(temp, 2, function(x) sum(x < 0) > 0))])}))

(check.negative.gs <- lapply(icio.years.gs, function(dat){
    temp <- dat
    temp$country.ind <- NULL
    return(dat[apply(temp, 1, function(x) sum(x < 0) > 0), c(TRUE, apply(temp, 2, function(x) sum(x < 0) > 0))])}))

## One issue in year 1996. Romania is a country with export taxes/subsidies. This makes ROU->JPN.g flow negative. Negative flow breaks down \tau calculation because of log(-number). Reset the flow to $1.
## This will break down input-output identities, but ROU -> JPN.g are small flows.
icio.1996 <- icio.years[[2]]
icio.1996[icio.1996$country.ind %in% c("ROU.g", "ROU.bts", "ROU.hts"), c("country.ind", "JPN.g")]
icio.1996[icio.1996$country.ind %in% c("ROU.g", "ROU.bts", "ROU.hts"), c("JPN.g")] <- 0.000001
icio.years[[2]] <- icio.1996

icio.1996 <- icio.years.alt[[2]]
icio.1996[icio.1996$country.ind %in% c("ROU.g", "ROU.cs", "ROU.ps"), c("country.ind", "JPN.g")]
icio.1996[icio.1996$country.ind %in% c("ROU.g", "ROU.cs", "ROU.ps"), c("JPN.g")] <- 0.000001
icio.years.alt[[2]] <- icio.1996

icio.1996 <- icio.years.gs[[2]]
icio.1996[icio.1996$country.ind %in% c("ROU.g", "ROU.s"), c("country.ind", "JPN.g")]
icio.1996[icio.1996$country.ind %in% c("ROU.g", "ROU.s"), c("JPN.g")] <- 0.000001
icio.years.gs[[2]] <- icio.1996

## Save cleaned data
save(icio.years, file = "../../output/cleaned_data/cleaned_icio.RData")
save(icio.years.alt, file = "../../output/cleaned_data/cleaned_icio_alt.RData")
save(icio.years.gs, file = "../../output/cleaned_data/cleaned_icio_gs.RData")
