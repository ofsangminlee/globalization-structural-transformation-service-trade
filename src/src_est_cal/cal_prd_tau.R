## cal_prod_tau.R
## Calculation of productivity and trade costs through model inversion

## Solving package dependency with Renv
renv::load("../")

################
## Trade cost ##
################

## Baseline, alternative, two-sector
## Low and high trade elasticities for services

library("tidyverse")

## Load trade data
load("../../output/cleaned_data/trade_matrix.RData", verbose = TRUE)
load("../../output/cleaned_data/trade_matrix_alt.RData", verbose = TRUE)
load("../../output/cleaned_data/trade_matrix_gs.RData", verbose = TRUE)

list.reclass <- c("g", "bts", "hts")
list.reclass.alt <- c("g", "cs", "ps")
list.reclass.gs <- c("g", "s")

## Load price data
gopl <- read.csv("../../output/cleaned_data/gopl.csv")

format.gopl <- function(gopl){
    gopl <- gopl[, c("country", "ind", "year", "pl")]
    gopl <- pivot_wider(gopl, values_from = "pl", names_from = "ind", names_prefix = "p.")
    return(gopl)
}

gopl <- format.gopl(gopl)

gopl.alt <- format.gopl(read.csv("../../output/cleaned_data/gopl_alt.csv"))
gopl.gs <- format.gopl(read.csv("../../output/cleaned_data/gopl_gs.csv"))

## Sanity check: in trade matrices, are countries alphabetically ordered? YES.
check.order <- function(x){
    names.col <- colnames(x)[colnames(x) != "country"]
    names.row <- x$country
    names.ordered <- names.row[order(names.row)]
    return(identical(names.row, names.ordered) & identical(names.col, names.ordered))
}

check.all <- function(icio.trade.ind) prod(unlist(lapply(icio.trade.ind, function(dat.ind) lapply(dat.ind, function(x) check.order(x)))))

check.all(icio.trade.ind)
check.all(icio.trade.ind.alt)
check.all(icio.trade.ind.gs)

list.country.ordered <- icio.trade.ind[[1]][[1]]$country

## Function to calculate tau.
## Given a trade matrix (for a year and a sector), price data, theta (trade elasticity), this function returns trade cost matrix
## If (full.info = TRUE), then you can decompose trade costs into their components.
cal.tau <- function(trade, gopl, theta, i.ind, i.year){

    tau <- trade
    tau[, colnames(tau) != "country"] <- NA

    relp <- trade
    relp[, colnames(relp) != "country"] <- NA

    own.share <- trade
    own.share[, colnames(own.share) != "country"] <- NA

    trade.share <- trade
    trade.share[, colnames(trade.share) != "country"] <- NA
    
    for (exp.country in list.country.ordered){
        for (imp.country in list.country.ordered){
            if (exp.country == imp.country){
                tau[tau$country == exp.country, imp.country] <- 1
                relp[relp$country == exp.country, imp.country] <- NA
                own.share[own.share$country == exp.country, imp.country] <- NA
                trade.share[trade.share$country == exp.country, imp.country] <- NA
            }
            else {
                p.exp <- gopl[gopl$year == i.year & gopl$country == exp.country, paste0("p.", i.ind)]
                p.imp <- gopl[gopl$year == i.year & gopl$country == imp.country, paste0("p.", i.ind)]
                pi.ii <- trade[trade$country == exp.country, exp.country]/sum(trade[, exp.country])
                pi.ij <- trade[trade$country == exp.country, imp.country]/sum(trade[, imp.country])
                theta.ind <- theta$theta[theta$ind == i.ind]

                tau[tau$country == exp.country, imp.country] <- (pi.ij/pi.ii)^(-1/theta.ind)*p.imp/p.exp

                relp[relp$country == exp.country, imp.country] <- p.imp/p.exp
                own.share[own.share$country == exp.country, imp.country] <- pi.ii
                trade.share[trade.share$country == exp.country, imp.country] <- pi.ij
            }
        }
    }
    return(list(tau = tau, relp = relp, own.share = own.share, trade.share = trade.share))
}

## Baseline
generate.theta.mat <- function(list.reclass, list.theta, eta){
    res <- data.frame(ind = list.reclass, theta = list.theta)
    res$gamma <- gamma((res$theta + 1 - eta)/res$theta)^(1/(1-eta))
    return(res)
}

theta <- generate.theta.mat(list.reclass, c(4,4,4), 2)

years.x <- paste0("X", 1995:2018)

cal.tau.all <- function(list.reclass, icio.trade.ind, gopl, theta){
    tau <- relp <- own.share <- trade.share <- list()
    for (j in 1:length(list.reclass)){
        i.ind <- list.reclass[j]
        temp.1 <- temp.2 <- temp.3 <- temp.4 <- list()
        for (i in 1:24){
            temp <- cal.tau(icio.trade.ind[[i.ind]][[i]], gopl, theta, i.ind, years.x[i])
            temp.1[[i]] <- temp[["tau"]]
            temp.2[[i]] <- temp[["relp"]]
            temp.3[[i]] <- temp[["own.share"]]
            temp.4[[i]] <- temp[["trade.share"]]
        }
        tau[[j]] <- temp.1
        relp[[j]] <- temp.2
        own.share[[j]] <- temp.3
        trade.share[[j]] <- temp.4
    }

    names(tau) <- names(relp) <- names(own.share) <- names(trade.share) <- list.reclass
    return(list(tau = tau, relp = relp, own.share = own.share, trade.share = trade.share))
}

tt.base <- cal.tau.all(list.reclass, icio.trade.ind, gopl, theta)

tau <- tt.base[["tau"]]

## Other specifications
theta.alt <- generate.theta.mat(list.reclass.alt, c(4,4,4), 2)

tt.alt <- cal.tau.all(list.reclass.alt, icio.trade.ind.alt, gopl.alt, theta.alt)

tau.alt <- tt.alt[["tau"]]

save(tau.alt, file = "../../output/estimated_params/tau_alt.RData")

theta.gs <- generate.theta.mat(list.reclass.gs, c(4,4), 2)

tt.gs <- cal.tau.all(list.reclass.gs, icio.trade.ind.gs, gopl.gs, theta.gs)

tau.gs <- tt.gs[["tau"]]

## Robustness
## LOW theta
theta.low <- generate.theta.mat(list.reclass, c(4,2,2), 2)

tt.low <- cal.tau.all(list.reclass, icio.trade.ind, gopl, theta.low)

tau.low <- tt.low[["tau"]]

## HIGH theta
theta.high <- generate.theta.mat(list.reclass, c(4,6,6), 2)

tt.high <- cal.tau.all(list.reclass, icio.trade.ind, gopl, theta.high)

tau.high <- tt.high[["tau"]]

## Tau's < 1 case. check
## How much of tau's are below 1.
check.below1 <- function(tau)
    lapply(tau, function(tau.ind) sum(unlist(lapply(tau.ind, function(dat) sum(dat[, list.country.ordered] < 1))))/(67*66*24)*100)

lll <- unlist(check.below1(tau)) # G: 0.3, BTS: 0.7, HTS: 1.0 percent

writeLines(paste0("Percentage of bilateral trade costs below one for all sectors is ", mean(lll), "."), "../../doc/nums/tau_below_one.txt")

## writeLines(paste0("Percentage of bilateral trade costs below one for G, BTS, and HTS is ", lll["g"], ", ", lll["bts"], ", and ", lll["hts"], "."), "../../doc/nums/tau_below_one.txt")

## This one is to check triangular inequality. About 9 percent is broken down.
check.triangle <- function(tau){
    qwer <- as.matrix(tau[, list.country.ordered])
    asdf <- matrix(NA, 67, 67)
    for (i in 1:67){
        for (j in 1:67){
            asdf[i,j] <- min(qwer[i,]*qwer[,j])
        }
    }
    return(sum(asdf < qwer))
}

triangle.violation <- lapply(tau, function(tau.sec) lapply(tau.sec, check.triangle))
triangle.violation <- sum(unlist((lapply(triangle.violation, function(x) sum(unlist(x))))))
triangle.violation <- triangle.violation/(67*66*3*24)*100

writeLines(paste0("Percentage of country-pairs for all sector-years with a violation of triangular equations is ", triangle.violation, "."), "../../doc/nums/triangle_violation.txt")

## Original wedge tau
tau.orig <- tau

save(tau.orig, file = "../../output/estimated_params/tau_orig.RData") ## Original case with below-one tau's

## Change below one entries to 1.
convert.below1 <- function(tau)
    lapply(tau, function(tau.ind) lapply(tau.ind, function(tau.ind.year){
        tau.ind.year[, list.country.ordered] <- pmax(matrix(1,67,67), as.matrix(tau.ind.year[, list.country.ordered]))
        return(tau.ind.year)
    }))

tau <- convert.below1(tau)
tau.gs <- convert.below1(tau.gs)
tau.low <- convert.below1(tau.low)
tau.high <- convert.below1(tau.high)

## Save the result
save(tau, file = "../../output/estimated_params/tau.RData")
save(tau.gs, file = "../../output/estimated_params/tau_gs.RData")
save(tau.low, file = "../../output/estimated_params/tau_low.RData")
save(tau.high, file = "../../output/estimated_params/tau_high.RData")

##################
## Productivity ##
##################

## Baseline, alternative, two-sector
## Robustness: low-theta, high-theta
## Robustness: average vs wedges, non-nested vs nested

## Load input cost data
unit.cost <- read.csv("../../output/estimated_params/unit_costs.csv")
unit.cost.alt <- read.csv("../../output/estimated_params/unit_costs_alt.csv")
unit.cost.gs <- read.csv("../../output/estimated_params/unit_costs_gs.csv")
unit.cost.nest <- read.csv("../../output/estimated_params/unit_costs_nest.csv")

## TRADE INPUT CHANGE.
## Calculate productivity
cal.prod <- function(list.reclass, unit.cost, gopl, icio.trade.ind, theta){
    res <- data.frame(country = character(), year = character(), ind = character(), prd = numeric())
    for (ex.country in list.country.ordered){
        for (i.year in years.x){
            for (i.ind in list.reclass){
                gamma.ind <- theta$gamma[theta$ind == i.ind]
                theta.ind <- theta$theta[theta$ind == i.ind]
                ## Output price
                p.ind <- gopl[gopl$country == ex.country & gopl$year == i.year, paste0("p.", i.ind)]
                ## Unit cost
                rr <- unit.cost$unit.cost[unit.cost$country == ex.country & unit.cost$year == i.year & unit.cost$ind == i.ind]
                stopifnot(length(rr) == 1)
                ## Own-trade share (Ricardian selection)
                trade <- icio.trade.ind[[i.ind]][[as.numeric(substr(i.year,2,5)) - 1994]]
                pi.ii <- trade[trade$country == ex.country, ex.country]/sum(trade[, ex.country])
                ## Productivity
                prd <- gamma.ind*(1/p.ind)*rr*(pi.ii^(1/theta.ind))
                names(prd) <- NULL
                stopifnot(length(prd) == 1)
                res <- rbind(res, data.frame(country = ex.country, year = i.year, ind = i.ind, prd = prd))
            }
        }
    }
    return(res)
}

prd <- cal.prod(list.reclass, subset(unit.cost, method == "average"), gopl, icio.trade.ind, theta)

## Alternative case

prd.alt <- cal.prod(list.reclass.alt, subset(unit.cost.alt, method == "average"), gopl.alt, icio.trade.ind.alt, theta.alt)

prd.gs <- cal.prod(list.reclass.gs, subset(unit.cost.gs, method == "average"), gopl.gs, icio.trade.ind.gs, theta.gs)

## Robustness: low-theta, high-theta

prd.low <- cal.prod(list.reclass, subset(unit.cost, method == "average"), gopl, icio.trade.ind, theta.low)
prd.high <- cal.prod(list.reclass, subset(unit.cost, method == "average"), gopl, icio.trade.ind, theta.high)

## Robustness: average vs wedges, non-nested vs nested
prd.wedge <- cal.prod(list.reclass, subset(unit.cost, method == "year"), gopl, icio.trade.ind, theta)
prd.nest <- cal.prod(list.reclass, subset(unit.cost.nest, method == "average"), gopl, icio.trade.ind, theta)

## Alternative model: Productivity when services are not tradable!
cal.prod.no.s.trade <- function(list.reclass, unit.cost, gopl, icio.trade.ind, theta){
    res <- data.frame(country = character(), year = character(), ind = character(), prd = numeric())
    for (ex.country in list.country.ordered){
        for (i.year in years.x){
            for (i.ind in list.reclass){
                gamma.ind <- theta$gamma[theta$ind == i.ind]
                theta.ind <- theta$theta[theta$ind == i.ind]
                ## Output price
                p.ind <- gopl[gopl$country == ex.country & gopl$year == i.year, paste0("p.", i.ind)]
                ## Unit cost
                rr <- unit.cost$unit.cost[unit.cost$country == ex.country & unit.cost$year == i.year & unit.cost$ind == i.ind]
                stopifnot(length(rr) == 1)
                ## Own-trade share (Ricardian selection)
                trade <- icio.trade.ind[[i.ind]][[as.numeric(substr(i.year,2,5)) - 1994]]
                pi.ii <- trade[trade$country == ex.country, ex.country]/sum(trade[, ex.country])
                ## Productivity
                if (i.ind == "g"){
                    prd <- gamma.ind*(1/p.ind)*rr*(pi.ii^(1/theta.ind))
                    names(prd) <- NULL
                    stopifnot(length(prd) == 1)
                    res <- rbind(res, data.frame(country = ex.country, year = i.year, ind = i.ind, prd = prd))
                }
                else {
                    prd <- gamma.ind*(1/p.ind)*rr*(1^(1/theta.ind))
                    names(prd) <- NULL
                    stopifnot(length(prd) == 1)
                    res <- rbind(res, data.frame(country = ex.country, year = i.year, ind = i.ind, prd = prd))
                }
                
            }
        }
    }
    return(res)
}

prd.no.s.trade <- cal.prod.no.s.trade(list.reclass, subset(unit.cost, method == "average"), gopl, icio.trade.ind, theta)

## Save productivity ##
write.csv(prd, "../../output/estimated_params/prd.csv", row.names = FALSE)
write.csv(prd.alt, "../../output/estimated_params/prd_alt.csv", row.names = FALSE)
write.csv(prd.gs, "../../output/estimated_params/prd_gs.csv", row.names = FALSE)
write.csv(prd.low, "../../output/estimated_params/prd_low.csv", row.names = FALSE)
write.csv(prd.high, "../../output/estimated_params/prd_high.csv", row.names = FALSE)
write.csv(prd.wedge, "../../output/estimated_params/prd_wedge.csv", row.names = FALSE)
write.csv(prd.nest, "../../output/estimated_params/prd_nest.csv", row.names = FALSE)
write.csv(prd.no.s.trade, "../../output/estimated_params/prd_no_s_trade.csv", row.names = FALSE)

##################################
## Calculate NX (nominal) terms ##
##################################

load("../../output/cleaned_data/cleaned_icio.RData", verbose = TRUE)

name.country <- function(x) substr(x, 1, 3)
nx.country <- function(country, dat){
    x.country <- sum(dat[(!dat$country.ind %in% c("VALU", "OUTPUT")) & (name.country(dat$country.ind) == country), (colnames(dat) != "country.ind") & (name.country(colnames(dat)) != country) &  (colnames(dat) != "TOTAL")])
    i.country <- sum(dat[(!dat$country.ind %in% c("VALU", "OUTPUT")) & (name.country(dat$country.ind) != country), (colnames(dat) != "country.ind") & (name.country(colnames(dat)) == country) &  (colnames(dat) != "TOTAL")])
    return(x.country - i.country)
}

nx.year <- function(dat) do.call("rbind", lapply(list.country.ordered, function(x) data.frame(country = x, nx = nx.country(x, dat))))

icio.nx <- lapply(icio.years, nx.year)

for (i in 1:24){
    icio.nx[[i]][, "year"] <- paste0("X", 1994 + i)
}

icio.nx <- do.call("rbind", icio.nx)

## Sanity check: does nx term sum up to 0?
max(unlist(lapply(years.x, function(x) sum(subset(icio.nx, year == x)$nx))))

## Save it
write.csv(icio.nx, "../../output/estimated_params/nx.csv", row.names = FALSE)
