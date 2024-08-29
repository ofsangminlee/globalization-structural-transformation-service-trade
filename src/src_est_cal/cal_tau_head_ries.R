## cal_tau_head_ries.R
## Alternative trade cost calibration following Head and Ries (2001), who assume trade costs are symmetric.

## Solving package dependency with Renv
renv::load("../")

## Load necessary libraries
library("tidyverse")

## Load trade data
load("../../output/cleaned_data/trade_matrix.RData", verbose = TRUE)

list.reclass <- c("g", "bts", "hts")

## Sanity check: in trade matrices, are countries alphabetically ordered? YES.
check.order <- function(x){
    names.col <- colnames(x)[colnames(x) != "country"]
    names.row <- x$country
    names.ordered <- names.row[order(names.row)]
    return(identical(names.row, names.ordered) & identical(names.col, names.ordered))
}

check.all <- function(icio.trade.ind) prod(unlist(lapply(icio.trade.ind, function(dat.ind) lapply(dat.ind, function(x) check.order(x)))))

check.all(icio.trade.ind)

list.country.ordered <- icio.trade.ind[[1]][[1]]$country

## Function to calibrate tau.
## Given a trade matrix (for a year and a sector), theta (trade elasticity), this function returns trade cost matrix
cal.tau.hs <- function(trade, theta, i.ind){

    tau <- trade
    tau[, colnames(tau) != "country"] <- NA

    exp.own <- trade
    exp.own[, colnames(exp.own) != "country"] <- NA

    imp.own <- trade
    imp.own[, colnames(imp.own) != "country"] <- NA
    
    exp.imp <- trade
    exp.imp[, colnames(exp.imp) != "country"] <- NA

    imp.exp <- trade
    imp.exp[, colnames(imp.exp) != "country"] <- NA
    
    for (exp.country in list.country.ordered){
        for (imp.country in list.country.ordered){
            if (exp.country == imp.country){
                tau[tau$country == exp.country, imp.country] <- 1
                exp.own[exp.own$country == exp.country, imp.country] <- NA
                imp.own[imp.own$country == exp.country, imp.country] <- NA
                exp.imp[exp.imp$country == exp.country, imp.country] <- NA
                imp.exp[imp.exp$country == exp.country, imp.country] <- NA
            }
            else {

                pi.ii <- trade[trade$country == exp.country, exp.country]/sum(trade[, exp.country])
                pi.jj <- trade[trade$country == imp.country, imp.country]/sum(trade[, imp.country])
                pi.ij <- trade[trade$country == exp.country, imp.country]/sum(trade[, imp.country])
                pi.ji <- trade[trade$country == imp.country, exp.country]/sum(trade[, exp.country])
                
                theta.ind <- theta$theta[theta$ind == i.ind]

                tau[tau$country == exp.country, imp.country] <- (pi.ij*pi.ji/(pi.ii*pi.jj))^(-1/(2*theta.ind))
                exp.own[exp.own$country == exp.country, imp.country] <- pi.ii
                imp.own[imp.own$country == exp.country, imp.country] <- pi.jj
                exp.imp[exp.imp$country == exp.country, imp.country] <- pi.ij
                imp.exp[exp.imp$country == exp.country, imp.country] <- pi.ji               
            }
        }
    }
    return(list(tau = tau, exp.own = exp.own, imp.own = imp.own, exp.imp = exp.imp, imp.exp = imp.exp))
}

## Three sector: g, hts, and bts
generate.theta.mat <- function(list.reclass, list.theta, eta){
    res <- data.frame(ind = list.reclass, theta = list.theta)
    res$gamma <- gamma((res$theta + 1 - eta)/res$theta)^(1/(1-eta))
    return(res)
}

theta <- generate.theta.mat(list.reclass, c(4,4,4), 2)

## years.x <- paste0("X", 1995:2018)

cal.tau.all.hs <- function(list.reclass, icio.trade.ind, theta){
    tau <- exp.own <- imp.own <- exp.imp <- imp.exp <- list()
    for (j in 1:length(list.reclass)){
        i.ind <- list.reclass[j]
        temp.1 <- temp.2 <- temp.3 <- temp.4 <- temp.5 <- list()
        for (i in 1:24){
            temp <- cal.tau.hs(icio.trade.ind[[i.ind]][[i]], theta, i.ind)
            temp.1[[i]] <- temp[["tau"]]
            temp.2[[i]] <- temp[["exp.own"]]
            temp.3[[i]] <- temp[["imp.own"]]
            temp.4[[i]] <- temp[["exp.imp"]]
            temp.5[[i]] <- temp[["imp.exp"]]
        }
        tau[[j]] <- temp.1
        exp.own[[j]] <- temp.2
        imp.own[[j]] <- temp.3
        exp.imp[[j]] <- temp.4
        imp.exp[[j]] <- temp.5
    }

    names(tau) <- names(exp.own) <- names(imp.own) <- names(exp.imp) <- names(imp.exp) <- list.reclass
    return(list(tau = tau, exp.own = exp.own, imp.own = imp.own, exp.imp = exp.imp, imp.exp = imp.exp))
}

tt.hs <- cal.tau.all.hs(list.reclass, icio.trade.ind, theta)

tau.hs <- tt.hs[["tau"]]

check.below1 <- function(tau)
    lapply(tau, function(tau.ind) sum(unlist(lapply(tau.ind, function(dat) sum(dat[, list.country.ordered] < 1))))/(67*66*24)*100)

check.below1(tau.hs)

save(tau.hs, file = "../../output/estimated_params/tau_head_ries.RData")

