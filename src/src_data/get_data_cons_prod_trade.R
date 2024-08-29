## get_data_cons_prod_trade.R
## Reformat input-output tables and price data into data frames of consumption, production and trade.

## Solving package dependency with Renv
renv::load("../")

## Packages and functions
library("tidyverse")
source("../common_functions.R")

## Load ICIOs and price data
load("../../output/cleaned_data/cleaned_icio.RData", verbose = TRUE)
load("../../output/cleaned_data/cleaned_icio_alt.RData", verbose = TRUE)
load("../../output/cleaned_data/cleaned_icio_gs.RData", verbose = TRUE)

gopl <- read.csv("../../output/cleaned_data/gopl.csv")
gopl.alt <- read.csv("../../output/cleaned_data/gopl_alt.csv")
gopl.gs <- read.csv("../../output/cleaned_data/gopl_gs.csv")

clean.gopl <- function(gopl){
    gopl <- gopl[, c("country", "year", "ind", "pl")]
    gopl <- pivot_wider(gopl, names_from = "ind", values_from = "pl", names_prefix = "p.")
    return(gopl)
}

gopl <- clean.gopl(gopl)
gopl.alt <- clean.gopl(gopl.alt)
gopl.gs <- clean.gopl(gopl.gs)
    
## Reclassified industries
list.reclass <- c("g", "bts", "hts")
list.reclass.alt <- c("g", "cs", "ps")
list.reclass.gs <- c("g", "s")

## Load list of countries in ICIO
list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.ind <- read.csv("../../output/cleaned_data/list_industry_three.csv")
list.ind.alt <- read.csv("../../output/cleaned_data/list_industry_three_alt.csv")

## Other misc
years <- 1995:2018
years.x <- paste0("X", years)

## GDP, total labor force, GDP per capita.
gdp.emp <- read.csv("../../output/cleaned_data/gdp_per_capita.csv")

gdp.emp <- do.call("rbind", lapply(split(gdp.emp, gdp.emp$year), function(dat) rbind(dat, data.frame(country = "TOT", year = dat$year[1], gdp = sum(dat$gdp), emp = sum(dat$emp), gdppc = sum(dat$gdp)/sum(dat$emp))))) # add information for total.

## CHECK WHETHER I NEED THIS

######################
## Consumption data ##
######################

## Get data for utility function estimation: country, year, per capita consumption, consumption shares

clean.cons <- function(dat.icio.years, list.inds){

    ## Sectoral consumption by country
    icio.cons <- lapply(dat.icio.years, function(dat){
        dat <- row.agg.country(dat, "ALL", list.country$code)
        dat <- dat[, c("country.ind", pasted(list.country$code, "c"))]
        dat <- subset(dat, country.ind %in% pasted("ALL", list.inds))
        return(dat)
    })
    
    icio.cons <- lapply(icio.cons, function(dat){
        dat <- pivot_longer(dat, cols = pasted(list.country$code, "c"), names_to = "country", values_to = "value")
        dat$country <- name.country(dat$country)
        dat$ind <- name.ind(dat$country.ind)
        dat$country.ind <- NULL
        dat <- pivot_wider(dat, names_from = "ind", values_from = "value")
        dat$level.total <- rowSums(dat[colnames(dat) %in% list.inds])
        colnames(dat)[colnames(dat) %in% list.inds] <- pasted("level", colnames(dat)[colnames(dat) %in% list.inds])
        return(dat)
    })

    ## Add total
    icio.cons <- lapply(icio.cons, function(dat){
        temp <- data.frame(country = "TOT", t(colSums(dat[, pasted("level", c(list.inds, "total"))])))
        colnames(temp) <- c("country", pasted("level", c(list.inds, "total")))
        return(rbind(dat, temp))
    })
    
    ## Add year and unlist the list.
    for (i in 1:24){
        temp <- icio.cons[[i]]
        temp[, "year"] <- paste0("X", 1994 + i)
        icio.cons[[i]] <- temp
    }
    
    icio.cons <- do.call("rbind", icio.cons)
    
    ## Per capita consumption and shares
    icio.cons <- merge(icio.cons, gdp.emp, by = c("country", "year"), all.x = TRUE)
    for (ind in list.inds){
        icio.cons[, pasted("pc", ind)] <- icio.cons[, pasted("level", ind)]/icio.cons$emp
    }
    
    icio.cons[, "pc.total"] <- rowSums(icio.cons[, pasted("pc", list.inds)])
    for (ind in list.inds){
        icio.cons[, pasted("share", ind)] <- icio.cons[, pasted("pc", ind)]/icio.cons$pc.total
    }
    return(icio.cons)
}

icio.cons <- clean.cons(icio.years, list.reclass)
icio.cons.alt <- clean.cons(icio.years.alt, list.reclass.alt)
icio.cons.gs <- clean.cons(icio.years.gs, list.reclass.gs)

## Add price data
add.price <- function(dat, gopl) merge(dat, gopl, by = c("country", "year"), all.x = TRUE)

icio.cons <- merge(icio.cons, gopl, by = c("country", "year"), all.x = TRUE)
icio.cons.alt <- merge(icio.cons.alt, gopl.alt, by = c("country", "year"), all.x = TRUE)
icio.cons.gs <- merge(icio.cons.gs, gopl.gs, by = c("country", "year"), all.x = TRUE)

## Save results
write.csv(icio.cons, "../../output/cleaned_data/cons.csv", row.names = FALSE)
write.csv(icio.cons.alt, "../../output/cleaned_data/cons_alt.csv", row.names = FALSE)
write.csv(icio.cons.gs, "../../output/cleaned_data/cons_gs.csv", row.names = FALSE)

#####################
## Production data ##
#####################

clean.prod <- function(dat.icio, list.inds){

    list.country.ind <- unlist(lapply(list.country$code, function(x) pasted(x, list.inds)))

    ## Sectoral input consumption by country-industry.
    icio.prod <- lapply(dat.icio, function(dat){
        dat <- row.agg.country(dat, "ALL", list.country$code)
        dat <- dat[, c("country.ind", list.country.ind)]
        dat <- subset(dat, country.ind %in% c(pasted("ALL", list.inds), "VALU"))
        return(dat)
    })

    icio.prod <- lapply(icio.prod, function(dat){
        dat <- pivot_longer(dat, cols = all_of(list.country.ind), names_to = "country.ind.prod", values_to = "value")
        dat$country <- name.country(dat$country.ind.prod)
        dat$ind <- name.ind(dat$country.ind.prod)
        dat$country.ind.prod <- NULL
        dat$country.ind[substr(dat$country.ind,1,3) == "ALL"] <- name.ind(dat$country.ind[substr(dat$country.ind,1,3) == "ALL"])
        dat <- pivot_wider(dat, names_from = "country.ind", values_from = "value")
        dat <- clean.colnames(dat, c("VALU", list.inds), pasted(c("v", list.inds), "level"))
        return(dat)
    })

    for (i in 1:24){
        icio.prod[[i]][, "year"] <- paste0("X", 1994 + i)
    }

    icio.prod <- do.call("rbind", icio.prod)

    ## ## Relative share information
    ## for (i.ind in setdiff(c("v", list.inds), "g")){
    ##     icio.prod[, pasted(i.ind, "g")] <- icio.prod[, pasted(i.ind, "level")]/icio.prod[, "g.level"]
    ## }
        return(icio.prod)
}

icio.prod <- clean.prod(icio.years, list.reclass)
icio.prod.alt <- clean.prod(icio.years.alt, list.reclass.alt)
icio.prod.gs <- clean.prod(icio.years.gs, list.reclass.gs)

## Add price data
icio.prod <- add.price(icio.prod, gopl)
icio.prod.alt <- add.price(icio.prod.alt, gopl.alt)
icio.prod.gs <- add.price(icio.prod.gs, gopl.gs)

## Add average wage term (price of value-added)

add.wage <- function(dat, gdp.emp){
    dat <- merge(dat, gdp.emp, by = c("country", "year"), all.x = TRUE)
    dat$p.v <- dat$gdppc
    dat$gdppc <- NULL
    return(dat)
}

icio.prod <- add.wage(icio.prod, gdp.emp)
icio.prod.alt <- add.wage(icio.prod.alt, gdp.emp)
icio.prod.gs <- add.wage(icio.prod.gs, gdp.emp)

write.csv(icio.prod, "../../output/cleaned_data/prod.csv", row.names = FALSE)
write.csv(icio.prod.alt, "../../output/cleaned_data/prod_alt.csv", row.names = FALSE)
write.csv(icio.prod.gs, "../../output/cleaned_data/prod_gs.csv", row.names = FALSE)

################
## Trade data ##
################

## In the model, an industry is defined by producing industry, not purchasing industry.

list.country.ordered <- list.country$code[order(list.country$code)]

clean.trade <- function(dat.icio, list.inds){
    
    icio.trade <- lapply(dat.icio, function(dat){
        dat <- col.agg.ind(dat, "all", c(list.inds, "c"))
        dat$country <- name.country(dat$country.ind)
        dat$ind <- name.ind(dat$country.ind)
        colnames(dat) <- gsub("\\.all", "", colnames(dat))
        dat[!dat$country.ind %in% c("VALU", "OUTPUT"), 
            dat[, c("TOTAL", "country.ind")] <- NULL]
        return(dat)
    })

    icio.trade.ind <- list()
    for (i in 1:length(list.inds)){
        temp.list <- list()
        for (j in 1:length(icio.trade)){
            temp <- subset(icio.trade[[j]], ind == list.inds[i])
            temp$ind <- NULL
            temp.list[[j]] <- temp
        }
        icio.trade.ind[[i]] <- temp.list
    }

    tr.clean <- function(dat){
        dat <- dat[order(dat$country), c("country", list.country.ordered)]
        stopifnot(ncol(dat) - 1 == nrow(dat))
        return(dat)
    }

    icio.trade.ind <- lapply(icio.trade.ind, function(dat.list) lapply(dat.list, tr.clean))

    names(icio.trade.ind) <- list.inds

    return(icio.trade.ind)
}

icio.trade.ind <- clean.trade(icio.years, list.reclass)
icio.trade.ind.alt <- clean.trade(icio.years.alt, list.reclass.alt)
icio.trade.ind.gs <- clean.trade(icio.years.gs, list.reclass.gs)

save(icio.trade.ind, file = "../../output/cleaned_data/trade_matrix.RData")
save(icio.trade.ind.alt, file = "../../output/cleaned_data/trade_matrix_alt.RData")
save(icio.trade.ind.gs, file = "../../output/cleaned_data/trade_matrix_gs.RData")

