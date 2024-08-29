## preference.R
## Estimate preference parameters (substitution elasticity, sectoral non-homotheticity parameters)

## Remark 1: first difference estimator is not good. Since it only captures yearly differences, not long-term trend.
## Remark 2: regarding identification, what you are mostly using is with-in country cross-year differences.
## Remark 3: If one country's relative income or prices are very stable across time, this will cause collinearity issue with that country's fixed effect terms. This will make identification weak and standard error blow up. The prominent example is Brunei. This is why I use bootstrapping.

## Solving package dependency with Renv
renv::load("../")

## Packages and functions
library("gmm")

cons <- read.csv("../../output/cleaned_data/cons.csv")
cons.alt <- read.csv("../../output/cleaned_data/cons_alt.csv")
cons.gs <- read.csv("../../output/cleaned_data/cons_gs.csv")

## Reclassified industries
list.reclass <- c("g", "bts", "hts")
list.reclass.alt <- c("g", "cs", "ps")
list.reclass.gs <- c("g", "s")

## Other misc
years <- 1995:2018
years.x <- paste0("X", years)

## In the estimating equation, what matters is relative prices and relative income. Function to get relative prices.
rel.price <- function(dat, vars.nom, var.price.norm){
    dat[, vars.nom] <- dat[, vars.nom]/dat[, var.price.norm]
    return(dat)
}

## Log demean it to remove fixed effect term
log.demean <- function(dat, vars.non.num){
    dat[, ! colnames(dat) %in% vars.non.num] <- data.frame(scale(log(dat[, !colnames(dat) %in% vars.non.num]), scale = FALSE))
    return(dat)
}

## Take log deviation of relative prices and income from their mean.
log.demean.rel.p <- function(dat.cons, list.inds){

    icio.pref <- dat.cons[, c("country", "year", paste0("share.", list.inds), paste0("p.", list.inds), "pc.total")]

    ## Don't use ROW. You are imputing within country variation for ROW, since you are imputing prices.
    icio.pref <- subset(icio.pref, !country %in% c("TOT", "ROW"))
    
    icio.pref <- rel.price(icio.pref, c(paste0("p.", list.inds), "pc.total"), "p.g")
    
    icio.pref <- do.call("rbind", lapply(split(icio.pref, icio.pref$country), function(dat) log.demean(dat, c("country", "year"))))

    return(icio.pref)
}

## Clean the data set
cons.est <- log.demean.rel.p(cons, list.reclass)
cons.est.alt <- log.demean.rel.p(cons.alt, list.reclass.alt)
cons.est.gs <- log.demean.rel.p(cons.gs, list.reclass.gs)

## Since the parameters are all positive by restriction. I am estimating logs of the parameters.

## Moment condition
g.exp  <- function(param, x){
    x.exo.eps <- cbind(x[, c("p.bts", "pc.total")]*
                       ((x[, "share.bts"] - x[, "share.g"]) -
                        (1-exp(param[1]))*x[, "p.bts"] -
                        (1-exp(param[1]))*(exp(param[2])-1)*x[, "pc.total"]-
                        (exp(param[2])-1)*x[, "share.g"]),
                       x[, c("p.hts", "pc.total")]*
                       ((x[, "share.hts"] - x[, "share.g"]) -
                        (1-exp(param[1]))*x[, "p.hts"] -
                        (1-exp(param[1]))*(exp(param[3])-1)*x[, "pc.total"] -
                        (exp(param[3])-1)*x[, "share.g"])
                       )
    return(x.exo.eps)
}

g.exp.alt  <- function(param, x){
    x.exo.eps <- cbind(x[, c("p.cs", "pc.total")]*
                       ((x[, "share.cs"] - x[, "share.g"]) -
                        (1-exp(param[1]))*x[, "p.cs"] -
                        (1-exp(param[1]))*(exp(param[2])-1)*x[, "pc.total"]-
                        (exp(param[2])-1)*x[, "share.g"]),
                       x[, c("p.ps", "pc.total")]*
                       ((x[, "share.ps"] - x[, "share.g"]) -
                        (1-exp(param[1]))*x[, "p.ps"] -
                        (1-exp(param[1]))*(exp(param[3])-1)*x[, "pc.total"] -
                        (exp(param[3])-1)*x[, "share.g"])
                       )
    return(x.exo.eps)
}

g.exp.gs  <- function(param, x){
    x.exo.eps <- x[, c("p.s", "pc.total")]*
        ((x[, "share.s"] - x[, "share.g"]) -
         (1-exp(param[1]))*x[, "p.s"] -
         (1-exp(param[1]))*(exp(param[2])-1)*x[, "pc.total"]-
         (exp(param[2])-1)*x[, "share.g"])
    return(x.exo.eps)
}

## Gradient of moment condition
## Even without moment condition, the codes are still fast enought.

Dg.exp <- function(param,x){
    G <- matrix(
        c(mean(exp(param[1])*x[, "p.bts"]*(x[, "p.bts"] + (exp(param[2])-1)*x[, "pc.total"])), mean(exp(param[1])*x[, "pc.total"]*(x[, "p.bts"] + (exp(param[2])-1)*x[, "pc.total"])), mean(exp(param[1])*x[, "p.hts"]*(x[, "p.hts"] + (exp(param[3])-1)*x[, "pc.total"])), mean(exp(param[1])*x[, "pc.total"]*(x[, "p.hts"] + (exp(param[3])-1)*x[, "pc.total"])),  # exp(param[1])'s derivatives
          mean(exp(param[2])*x[, "p.bts"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"])), mean(exp(param[2])*x[, "pc.total"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"])),0,0,
          0,0, mean(exp(param[3])*x[, "p.hts"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"])), mean(exp(param[3])*x[, "pc.total"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"]))),
        nrow = 4, ncol = 3)
    return(G)
}

Dg.exp.alt <- function(param,x){
    G <- matrix(
        c(mean(exp(param[1])*x[, "p.cs"]*(x[, "p.cs"] + (exp(param[2])-1)*x[, "pc.total"])), mean(exp(param[1])*x[, "pc.total"]*(x[, "p.cs"] + (exp(param[2])-1)*x[, "pc.total"])), mean(exp(param[1])*x[, "p.ps"]*(x[, "p.ps"] + (exp(param[3])-1)*x[, "pc.total"])), mean(exp(param[1])*x[, "pc.total"]*(x[, "p.ps"] + (exp(param[3])-1)*x[, "pc.total"])),  # exp(param[1])'s derivatives
          mean(exp(param[2])*x[, "p.cs"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"])), mean(exp(param[2])*x[, "pc.total"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"])),0,0,
          0,0, mean(exp(param[3])*x[, "p.ps"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"])), mean(exp(param[3])*x[, "pc.total"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"]))),
        nrow = 4, ncol = 3)
    return(G)
}

Dg.exp.gs <- function(param,x){
    G <- matrix(
        c(mean(exp(param[1])*x[, "p.s"]*(x[, "p.s"] + (exp(param[2])-1)*x[, "pc.total"])), mean(exp(param[1])*x[, "pc.total"]*(x[, "p.s"] + (exp(param[2])-1)*x[, "pc.total"])),
          mean(exp(param[2])*x[, "p.s"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"])), mean(exp(param[2])*x[, "pc.total"]*((exp(param[1])-1)*x[, "pc.total"] - x[, "share.g"]))),
        nrow = 2, ncol = 2)
    return(G)
}

## Result
res.exp <- gmm(g.exp, data.matrix(cons.est), t0 = c(log.sigma = -0.5, log.eps.bts = 0.5, log.eps.hts = 0.5), gradv = Dg.exp, method = "BFGS")

summary(res.exp)

format.result <- function(res.exp, names.params){
    res <- data.frame(params = names.params, estimates = exp(res.exp$coef))
    return(res)
}

res <- format.result(res.exp, c("sigma", "eps.bts", "eps.hts"))
res

## Std error from delta method and gmm.
## This blows up because of multicolinearity with fixed effect terms.
covariance.mat <- diag(exp(res.exp$coef)) %*% res.exp$vcov %*% diag(exp(res.exp$coef))
diag(sqrt(covariance.mat))

## Example: estimation without Brunei.
dat.brn <- subset(cons.est, country == "BRN")
cor(dat.brn$p.hts, dat.brn$p.bts)

res.only.brn <- gmm(g.exp, data.matrix(subset(cons.est, country == "BRN")), t0 = res.exp$coef, gradv = Dg.exp, method = "BFGS")
exp(res.only.brn$coef)
covariance.mat <- diag(exp(res.only.brn$coef)) %*% res.only.brn$vcov %*% diag(exp(res.only.brn$coef))
sqrt(diag(covariance.mat))

res.brn <- gmm(g.exp, data.matrix(subset(cons.est, country != "BRN")), t0 = res.exp$coef, gradv = Dg.exp, method = "BFGS")
exp(res.brn$coef)
covariance.mat <- diag(exp(res.brn$coef)) %*% res.brn$vcov %*% diag(exp(res.brn$coef))
diag(sqrt(covariance.mat))

## Bootstrap standard errors.
bootstrap.stderr <- function(cons.est, g.exp, Dg.exp, res.exp, names.params, nboot, return.whole = FALSE){
    start.point <- res.exp$coef

    estimate.once <- function(x, dat.est){
        rows <- sample(1:nrow(dat.est), nrow(dat.est), replace = TRUE)
        res.temp <- gmm(g.exp, data.matrix(dat.est[rows, ]), t0 = start.point, gradv = Dg.exp, method = "BFGS")
        return(res.temp$coef)
    }

    bootstrap <- data.frame(do.call("rbind", lapply(1:nboot, function(x) estimate.once(x, cons.est))))

    res <- data.frame(do.call("rbind", lapply(1:ncol(bootstrap), function(i) exp(quantile(bootstrap[, i], c(0.025, 0.975))))))
    colnames(res) <- c("int.95.lower", "int.95.upper")

    res$params <- names.params
    res$estimates <- exp(res.exp$coef)

    res <- res[, c("params", "estimates", "int.95.lower", "int.95.upper")]
    if (return.whole){
        return(list(res, bootstrap))
    }
    else {
        return(res)
    }
}

set.seed(1004)

res <- bootstrap.stderr(cons.est, g.exp, Dg.exp, res.exp, c("sigma", "eps.bts", "eps.hts"), 1000)

## ## If you want to check the bootstrap distribution, do the following instead.
## res.whole <- bootstrap.stderr(cons.est, g.exp, Dg.exp, res.exp, c("sigma", "eps.cs", "eps.ps"), 1000)
## res <- res.whole[[1]]
## res.dist <- res.whole[[2]]
## plot(density(exp(res.dist[, 3])))

add.g <- function(res){
    res <- rbind(res, data.frame(params = "eps.g", estimates = 1, int.95.lower = NA, int.95.upper = NA))
    res <- res[c(1,nrow(res),2:(nrow(res)-1)), ]
    return(res)    
}

res <- add.g(res)

## Other sectoral specifications
res.exp.alt <- gmm(g.exp.alt, data.matrix(cons.est.alt), t0 = c(log.sigma = -0.5, log.eps.cs = 0.5, log.eps.ps = 0.5), gradv = Dg.exp.alt, method = "BFGS")

res.alt <- format.result(res.exp.alt, c("sigma", "eps.cs", "eps.ps"))
res.alt

res.alt <- bootstrap.stderr(cons.est.alt, g.exp.alt, Dg.exp.alt, res.exp.alt, c("sigma", "eps.cs", "eps.ps"), 1000)

res.alt <- add.g(res.alt)

res.exp.gs <- gmm(g.exp.gs, data.matrix(cons.est.gs), t0 = c(log.sigma = -0.5, log.eps.s = 0.5), gradv = Dg.exp.gs, method = "BFGS")

res.gs <- format.result(res.exp.gs, c("sigma", "eps.s"))
res.gs

res.gs <- bootstrap.stderr(cons.est.gs, g.exp.gs, Dg.exp.gs, res.exp.gs, c("sigma", "eps.s"), 1000)

res.gs <- add.g(res.gs) ## One remark!!!! EPS.S is greater than both EPS.PS and EPS.CS. I know why this is happening, compared to before you have higher relative productivity differences. So a lot is allocated to income effect. Hence epsilon is higher. My original conjecture is that \eps^s would lie between \eps^cs and \eps^ps

## Estimation without the US for the model check on non-targeted moments.
res.exp.usa <- gmm(g.exp, data.matrix(subset(cons.est, country != "USA")), t0 = c(log.sigma = -0.5, log.eps.bts = 0.5, log.eps.hts = 0.5), gradv = Dg.exp, method = "BFGS")

res.usa <- format.result(res.exp.usa, c("sigma", "eps.bts", "eps.hts"))
res.usa

res.usa <- bootstrap.stderr(subset(cons.est, country != "USA"), g.exp, Dg.exp, res.exp.usa, c("sigma", "eps.bts", "eps.hts"), 1000)

res.usa <- add.g(res.usa)

## Recovering phi's
## To get at sectoral weights, there are three possible ways!!! 1. average across years, 2. wedges approach, and 3. first-year matching (2 nests 3).

## Function to extract fixed effect terms in the estimating equations.
## Two fixed effect terms (log + weights + epsilon) for any years.

fe.term <- function(cons, res, list.reclass, ex.country, i.year){

    list.no.g <- setdiff(list.reclass, "g")
    
    sigma <- res[res$params == "sigma", "estimates"]
    epss <- res[match(paste0("eps.", list.reclass), res$params), "estimates"]
    names(epss) <- list.reclass

    dat <- subset(cons, country == ex.country & year == i.year)
    temp <- log(dat[, paste0("share.", list.no.g)]/dat[, "share.g"]) - (1-sigma)*log(dat[, paste0("p.",list.no.g)]/dat[, "p.g"]) - (1-sigma)*(epss[list.no.g]-1)*log(dat$pc.total/dat[,"p.g"]) - (epss[list.no.g]-1)*log(dat[, "share.g"])
    res <- cbind(data.frame(country = ex.country), temp)
    colnames(res) <- c("country", paste0("fe.term.", list.no.g))
    return(res)
}

## Recover phi from FE term for every year and average
years.x <- paste0("X", 1995:2018)

recover.phi <- function(cons, res, list.reclass, ex.country, i.year, method = "year"){

    list.no.g <- setdiff(list.reclass, "g")
    
    sigma <- res[res$params == "sigma", "estimates"]
    epss <- res[match(paste0("eps.", list.reclass), res$params), "estimates"]
    names(epss) <- list.reclass
    
    if (method == "year"){
        phi.terms <- fe.term(cons, res, list.reclass, ex.country, i.year)[, paste0("fe.term.", list.no.g)]
    }
    
    if (method == "average"){
        temp <- do.call("rbind", lapply(years.x, function(x) fe.term(cons, res, list.reclass, ex.country, x)))
        if (length(list.no.g) > 1){
            phi.terms <- colMeans(temp[,  paste0("fe.term.", list.no.g)])
        }
        else if (length(list.no.g) == 1){
            phi.terms <- mean(temp[, paste0("fe.term.", list.no.g)])
        }        
    }
    
    phi.term.gap <- function(phis){
        phi.g <- 1-sum(phis)
        cond <- sum(((phi.g)^(-sapply(c(list.no.g), function(x) epss[x]))*phis - exp(phi.terms))^2) # phi.term - phi.terms from guessed phi's
        return(cond)
    }

    if (length(list.no.g) == 2){
        res.phi <- optim(c(0.3, 0.3), phi.term.gap, control = list(reltol = 1e-9)) # Nelder-mead
        stopifnot(res.phi$convergence == 0)
        stopifnot(res.phi$objective < 1e-7)
        phis <- data.frame(ind = list.reclass, estimates = c(1-sum(res.phi$par), res.phi$par))
    }

    if (length(list.no.g) == 1){
        res.phi <- optimize(phi.term.gap,c(0,1), tol = 1e-10) # Brent
        stopifnot(res.phi$objective < 1e-7)
        phis <- data.frame(ind = list.reclass, estimates = c(1-res.phi$minimum, res.phi$minimum))
    }

    ## Add years here. it would be easier hehe. after woking on production.
    phis$year <- i.year
    return(phis)
}

## Save the result for two methods

list.country <- read.csv("../../output/cleaned_data/list_country.csv")

phis.country <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country$code, function(ex.country) data.frame(country = ex.country, recover.phi(cons, res, list.reclass, ex.country, i.year))))))

phis.country.average <-  do.call("rbind", lapply(list.country$code, function(ex.country) data.frame(country = ex.country, recover.phi(cons, res, list.reclass, ex.country, "avg", "average"))))

## ## You can check each country: they are pretty stable.
## ex.c <- "IND"
## subset(phis.country, country == ex.c)
## subset(phis.country.average, country == ex.c)

phis.country.alt <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country$code, function(ex.country) data.frame(country = ex.country, recover.phi(cons.alt, res.alt, list.reclass.alt, ex.country, i.year))))))

phis.country.average.alt <-  do.call("rbind", lapply(list.country$code, function(ex.country) data.frame(country = ex.country, recover.phi(cons.alt, res.alt, list.reclass.alt, ex.country, "avg", "average"))))

phis.country.gs <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country$code, function(ex.country) data.frame(country = ex.country, recover.phi(cons.gs, res.gs, list.reclass.gs, ex.country, i.year))))))

phis.country.average.gs <-  do.call("rbind", lapply(list.country$code, function(ex.country) data.frame(country = ex.country, recover.phi(cons.gs, res.gs, list.reclass.gs, ex.country, "avg", "average"))))

## Sanity check (whether shares are accurately predicted)

## Demand system
## Function for price index calculation
price.index <- function(prices.country, util, phis, list.epss, sigma) sum(phis*(prices.country)^(1-sigma)*util^((1-sigma)*(list.epss-1)))^(1/(1-sigma))


## Function to calculate utility for a country-year
## Inner function optimization
util.get <- function(expenditure, prices.country, phis, list.epss, sigma){
    util.inner <- function(util) (util - expenditure/price.index(prices.country, util, phis, list.epss, sigma))^2
    temp <- optimize(util.inner, c(0, 1e6), tol = 1e-10)
    stopifnot(temp$objective < 1e-6)
    return(temp$minimum)
}


## Predicting the shares
share.pred <- function(prices, util, phis, list.epss, sigma) phis*(prices/price.index(prices, util, phis, list.epss, sigma))^(1-sigma)*util^((1-sigma)*(list.epss -1))

share.pred.outer <- function(ex.country, i.year, dat.cons, dat.phis, list.reclass, res){

    sigma <- res$estimates[res$params == "sigma"]

    list.epss <- res[match(paste0("eps.", list.reclass), res$params), "estimates"]
    
    phis <- dat.phis[dat.phis$country == ex.country & dat.phis$year == i.year, c("ind", "estimates")]

    phis <- phis[match(list.reclass, phis$ind), "estimates"]

    dat <- subset(dat.cons, country == ex.country & year == i.year)
    
    prices.country <- dat[, paste0("p.", list.reclass)]

    return(share.pred(prices.country, util.get(dat$pc.total, prices.country, phis, list.epss, sigma), phis, list.epss, sigma))
}

## Sanity check: check whether countries' demand functions are well satisfied.
share.pred.country <- function(ex.country, i.year, dat.cons, dat.phis, list.reclass, res){
    dat.country.year <- subset(dat.cons, country == ex.country & year == i.year)
    return(max(abs(dat.country.year[, paste0("share.", list.reclass)] - share.pred.outer(ex.country, i.year, dat.cons, dat.phis, list.reclass, res))))    
}

max(do.call("rbind", lapply(years.x, function(ii.year) max(sapply(list.country$code, function(x) share.pred.country(x, ii.year, cons, phis.country, list.reclass, res))))))

max(do.call("rbind", lapply(years.x, function(ii.year) max(sapply(list.country$code, function(x) share.pred.country(x, ii.year, cons.alt, phis.country.alt, list.reclass.alt, res.alt))))))

max(do.call("rbind", lapply(years.x, function(ii.year) max(sapply(list.country$code, function(x) share.pred.country(x, ii.year, cons.gs, phis.country.gs, list.reclass.gs, res.gs))))))

## Save the result
write.csv(res, "../../output/estimated_params/sub_inc_elasticity.csv", row.names = FALSE)

write.csv(res.alt, "../../output/estimated_params/sub_inc_elasticity_alt.csv", row.names = FALSE)

write.csv(res.gs, "../../output/estimated_params/sub_inc_elasticity_gs.csv", row.names = FALSE)

write.csv(res.usa, "../../output/estimated_params/sub_inc_elasticity_wo_usa.csv", row.names = FALSE)

merge.phis <- function(phis.country, phis.country.average){
    phis.country.average$year <- "avg"
    phis.country <- rbind(phis.country, phis.country.average)
    return(phis.country)
}

phis.country <- merge.phis(phis.country, phis.country.average)

phis.country.alt <- merge.phis(phis.country.alt, phis.country.average.alt)

phis.country.gs <- merge.phis(phis.country.gs, phis.country.average.gs)

write.csv(phis.country, "../../output/estimated_params/weight_util.csv", row.names = FALSE)

write.csv(phis.country.alt, "../../output/estimated_params/weight_util_alt.csv", row.names = FALSE)

write.csv(phis.country.gs, "../../output/estimated_params/weight_util_gs.csv", row.names = FALSE)

## One note, for the US case, I will back out \phi's to match the initial year for USA-ROW cases.
