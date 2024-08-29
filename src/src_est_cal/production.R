## Production.R
## Estimate production parameters.
## Identification comes mostly from within-country cross-year differences.

## Solving package dependency with Renv
renv::load("../")

## Packages and functions
library("gmm")
library("tidyverse")

pasted <- function(x,y) paste0(x,".",y)

pro <- read.csv("../../output/cleaned_data/prod.csv")
pro.alt <- read.csv("../../output/cleaned_data/prod_alt.csv")
pro.gs <- read.csv("../../output/cleaned_data/prod_gs.csv")

## Three reclassified industries
list.reclass <- c("g", "bts", "hts")
list.reclass.alt <- c("g", "cs", "ps")
list.reclass.gs <- c("g", "s")

## Other misc
years <- 1995:2018
years.x <- paste0("X", years)

## Function to log demean variables.
log.demean <- function(dat, vars.non.num){
    dat[, ! colnames(dat) %in% vars.non.num] <- data.frame(scale(log(dat[, !colnames(dat) %in% vars.non.num]), scale = FALSE))
    return(dat)
}

## Clean data for production (add relative prices)
clean.prod.est <- function(dat.prod, list.ind.out, list.ind.in, base.ind){

    ## Input ratios
    
    list.ind.from <- setdiff(list.ind.in, base.ind)
    
    for (i.ind in list.ind.from){
        ## Input ratios
        dat.prod[, pasted(i.ind, base.ind)] <- dat.prod[, pasted(i.ind, "level")]/dat.prod[, pasted(base.ind, "level")]
        ## Price ratios
        dat.prod[, paste0("rel.p.", i.ind, ".", base.ind)] <- dat.prod[, pasted("p", i.ind)]/dat.prod[, pasted("p", base.ind)]
    }
   
    prod.est <- dat.prod[, c("country", "year", "ind", pasted(list.ind.from, base.ind), paste0("rel.p.", list.ind.from, ".", base.ind))]

    ## Exclude ROW, their prices are imputed.
    prod.est <- subset(prod.est, !country %in% c("ROW", "TOT"))

    ## For each of the producing sector, log-demean the variables.
    prod.est.ind <- lapply(list.ind.out, function(x) subset(prod.est, ind == x))
    
    prod.est.ind <- lapply(prod.est.ind, function(dat){
        dat.split <- split(dat, dat$country)
        res <- do.call("rbind", lapply(dat.split, function(x) log.demean(x, c("country", "year", "ind"))))
        return(res)
    })

    return(prod.est.ind)
}

## Data for estimation
prod.est <- clean.prod.est(pro, list.reclass, c("v", list.reclass), "g")

prod.est.alt <- clean.prod.est(pro.alt, list.reclass.alt, c("v", list.reclass.alt), "g")

prod.est.gs <- clean.prod.est(pro.gs, list.reclass.gs, c("v", list.reclass.gs), "g")

prod.est.usa <- lapply(prod.est, function(dat.ind) subset(dat.ind, country != "USA")) # Data without the US (for the model check)



## Moment condition
## Note: even without gradiant information, it is precisely estimated.

f.exp  <- function(param, x, list.ind.in, base.ind){
    list.ind.from <- setdiff(list.ind.in, base.ind)
    x.exo <- x[, paste0("rel.p.", list.ind.from, ".", base.ind)]
    if (length(list.ind.from) == 1){
        x.exo.eps <- x.exo*(x[, pasted(list.ind.from, base.ind)] - (1-exp(param[1]))*x[, paste0("rel.p.", list.ind.from, ".", base.ind)])
    }
    if (length(list.ind.from) == 2){
        x.exo.eps <-
            cbind(x.exo*(x[, pasted(list.ind.from[1], base.ind)] - (1-exp(param[1]))*x[, paste0("rel.p.", list.ind.from[1], ".", base.ind)]),
                  x.exo*(x[, pasted(list.ind.from[2], base.ind)] - (1-exp(param[1]))*x[, paste0("rel.p.", list.ind.from[2], ".", base.ind)])
            )
    }
    if (length(list.ind.from) == 3){
        x.exo.eps <-
            cbind(x.exo*(x[, pasted(list.ind.from[1], base.ind)] - (1-exp(param[1]))*x[, paste0("rel.p.", list.ind.from[1], ".", base.ind)]),
                  x.exo*(x[, pasted(list.ind.from[2], base.ind)] - (1-exp(param[1]))*x[, paste0("rel.p.", list.ind.from[2], ".", base.ind)]),
                  x.exo*(x[, pasted(list.ind.from[3], base.ind)] - (1-exp(param[1]))*x[, paste0("rel.p.", list.ind.from[3], ".", base.ind)])
                  )
    }
    return(x.exo.eps)
}

f.exp.base <- function(param, x) f.exp(param, x, c("v", list.reclass), "g")

res.rho <- lapply(prod.est, function(dat) gmm(f.exp.base, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100))

sum.result <- function(res.rho, list.ind.out){
    sum.res <-
        do.call("rbind", lapply(res.rho, function(gmm.res){
            rho <- exp(gmm.res$coef)
            stderr <- rho*sqrt(gmm.res$vcov)*rho # delta method
            temp <- data.frame(rho, stderr)
            colnames(temp) <- c("rho", "stderr")
            return(temp)
        }))
    sum.res$ind <- list.ind.out
    return(sum.res)
}

sum.rho <- sum.result(res.rho, list.reclass)

## Alternative cases

f.exp.alt <- function(param, x) f.exp(param, x, c("v", list.reclass.alt), "g")

res.rho.alt <- lapply(prod.est.alt, function(dat) gmm(f.exp.alt, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100))

sum.rho.alt <- sum.result(res.rho.alt, list.reclass.alt)

f.exp.gs <- function(param, x) f.exp(param, x, c("v", list.reclass.gs), "g")

res.rho.gs <- lapply(prod.est.gs, function(dat) gmm(f.exp.gs, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100))

sum.rho.gs <- sum.result(res.rho.gs, list.reclass.gs)

## Estimation without the US for the model check on non-targeted moments.

res.rho.usa <- lapply(prod.est.usa, function(dat) gmm(f.exp.base, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100))

sum.rho.usa <- sum.result(res.rho.usa, list.reclass)



## Getting alpha's back
## Fixed effect terms (log of ratios of alpha)
fe.term.prod <- function(ex.country, i.year, list.ind.out, list.ind.in, base.ind, res.rho, dat.prod){
    
    list.ind.in <- setdiff(list.ind.in, base.ind)

    res <- data.frame(ind.out = rep(list.ind.out, each = length(list.ind.in)), ind.in = rep(list.ind.in, length(list.ind.out)))
    res$log.alpha.ratio <- NA
    for (i.ind.out in list.ind.out){
        rho <- res.rho$rho[res.rho$ind == i.ind.out]
        dat <- subset(dat.prod, country == ex.country & year == i.year & ind == i.ind.out)
        for (i.ind.in in list.ind.in){
            temp <- log(dat[, pasted(i.ind.in, "level")]/dat[, pasted("g", "level")]) - (1-rho)*log(dat[, paste0("p.", i.ind.in)]/dat[, "p.g"])
            res$log.alpha.ratio[res$ind.out == i.ind.out & res$ind.in == i.ind.in] <- temp
        }
    }
    return(res)
}

## Recovering alpha
recover.alpha <- function(ex.country, i.year, list.ind.out, list.ind.in, base.ind, res.rho, dat.prod, method = "year"){
    list.ind.from <- setdiff(list.ind.in, base.ind)
    if (method == "year"){
        temp <- fe.term.prod(ex.country, i.year, list.ind.out, list.ind.in, base.ind, res.rho, dat.prod)
    }
    if (method == "average"){
        temp <- do.call("rbind", lapply(years.x, function(x) fe.term.prod(ex.country, x, list.ind.out, list.ind.in, base.ind, res.rho, dat.prod)))
        temp <- split(temp, paste(temp$ind.out, temp$ind.in))
        temp <- do.call("rbind", lapply(temp, function(dat) data.frame(ind.out = dat$ind.out[1], ind.in = dat$ind.in[1], log.alpha.ratio = mean(dat$log.alpha.ratio))))
    }
    
    temp$alpha.ratio <- exp(temp$log.alpha.ratio)
    temp$log.alpha.ratio <- NULL
    temp <- pivot_wider(temp, names_from = ind.in, values_from = alpha.ratio)
    temp[, pasted("alpha", base.ind)] <- 1/(1 + rowSums(temp[, list.ind.from]))
    for (i.ind in list.ind.from){
        temp[, pasted("alpha", i.ind)] <- temp[, pasted("alpha", base.ind)]*temp[, i.ind]
    }
    stopifnot(all.equal(rowSums(temp[, paste0("alpha.", list.ind.in)]),rep(1,length(list.ind.out))))
    temp$country <- ex.country
    temp <- temp[, c("country", "ind.out", pasted("alpha", list.ind.in))]
    colnames(temp)[2] <- "ind"
    temp$year <- i.year
    return(temp)
}

## Recover alpha's from 1) fixed effects terms 2) treating them as yearly wedges
list.country <- unique(pro$country)

alphas.year <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country, function(ex.country)
    recover.alpha(ex.country, i.year, list.reclass, c("v", list.reclass), "g", sum.rho, pro)))))

alphas.average <- do.call("rbind", lapply(list.country, function(ex.country)  recover.alpha(ex.country, "avg", list.reclass, c("v", list.reclass), "g", sum.rho, pro, method = "average")))

## Other specifications

alphas.year.alt <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country, function(ex.country)
    recover.alpha(ex.country, i.year, list.reclass.alt, c("v", list.reclass.alt), "g", sum.rho.alt, pro.alt)))))

alphas.average.alt <- do.call("rbind", lapply(list.country, function(ex.country)  recover.alpha(ex.country, "avg", list.reclass.alt, c("v", list.reclass.alt), "g", sum.rho.alt, pro.alt, method = "average")))

alphas.year.gs <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country, function(ex.country)
    recover.alpha(ex.country, i.year, list.reclass.gs, c("v", list.reclass.gs), "g", sum.rho.gs, pro.gs)))))

alphas.average.gs <- do.call("rbind", lapply(list.country, function(ex.country)  recover.alpha(ex.country, "avg", list.reclass.gs, c("v", list.reclass.gs), "g", sum.rho.gs, pro.gs, method = "average")))

## Sanity check: check whether countries' production function input shares are well satisfied.
prod.share.pred.year <- function(i.year, list.ind.out, list.ind.in, base.ind, dat.prod, dat.rhos, dat.alphas){
    list.ind.from <- setdiff(list.ind.in, base.ind)
    
    res <- merge(
        subset(dat.prod, year == i.year)[, c("country", "ind", pasted(list.ind.from, base.ind), pasted("p", list.ind.in))],
        dat.alphas[dat.alphas$year == i.year, c("country", "ind", paste0("alpha.", list.ind.in))],
        by = c("country", "ind"), all.x = TRUE)

    res[, pasted(pasted(list.ind.from, base.ind), "pred")] <- exp((1-dat.rhos$rho[match(res$ind, dat.rhos$ind)])*log(res[, paste0("p.", list.ind.from)]/res[, pasted("p", base.ind)]) + log(res[, pasted("alpha", list.ind.from)]/res[, pasted("alpha", base.ind)]))

    return(max(abs(res[,pasted(pasted(list.ind.from, base.ind), "pred")] - res[, pasted(list.ind.from, base.ind)])))
}

## Add relative input shares
add.rel.share <- function(dat.prod, list.ind.in, base.ind){
    
    list.ind.from <- setdiff(list.ind.in, base.ind)
    
    for (i.ind in list.ind.from){
        ## Input ratios
        dat.prod[, pasted(i.ind, base.ind)] <- dat.prod[, pasted(i.ind, "level")]/dat.prod[, pasted(base.ind, "level")]
    }
    
    return(dat.prod)
}

## Sanity check OK!
unlist(lapply(years.x, function(x) prod.share.pred.year(x, list.reclass, c("v", list.reclass), "g", add.rel.share(pro, c("v", list.reclass), "g"), sum.rho, alphas.year)))

unlist(lapply(years.x, function(x) prod.share.pred.year(x, list.reclass.gs, c("v", list.reclass.gs), "g", add.rel.share(pro.gs, c("v", list.reclass.gs), "g"), sum.rho.gs, alphas.year.gs)))

unlist(lapply(years.x, function(x) prod.share.pred.year(x, list.reclass.alt, c("v", list.reclass.alt), "g", add.rel.share(pro.alt, c("v", list.reclass.alt), "g"), sum.rho.alt, alphas.year.alt)))

## Save rho's and alpha's
## Note alpha's for the Without USA case, I will be backing it out from the first year for 2 country mode.
write.csv(sum.rho, "../../output/estimated_params/rho.csv", row.names = FALSE)
write.csv(sum.rho.alt, "../../output/estimated_params/rho_alt.csv", row.names = FALSE)
write.csv(sum.rho.gs, "../../output/estimated_params/rho_gs.csv", row.names = FALSE)
write.csv(sum.rho.usa, "../../output/estimated_params/rho_wo_usa.csv", row.names = FALSE)

alphas.country <- rbind(alphas.year, alphas.average)
alphas.country.alt <- rbind(alphas.year.alt, alphas.average.alt)
alphas.country.gs <- rbind(alphas.year.gs, alphas.average.gs)

write.csv(alphas.country, "../../output/estimated_params/alphas.csv", row.names = FALSE)
write.csv(alphas.country.alt, "../../output/estimated_params/alphas_alt.csv", row.names = FALSE)
write.csv(alphas.country.gs, "../../output/estimated_params/alphas_gs.csv", row.names = FALSE)

## Calculate unit costs for variety production

## Year varying wedges
calc.unit.cost <- function(dat.pro, dat.rho, dat.alpha, list.ind.in, method = "year"){
    dat <- merge(dat.pro, dat.rho, by = "ind", all.x = TRUE)
    if (method == "year"){
        dat <- merge(dat, dat.alpha, by = c("country", "ind", "year"), all.x = TRUE)
        dat$method <- "year"
    }
    if (method == "average"){
        dat.alpha <- subset(dat.alpha, year == "avg")
        dat <- merge(dat, dat.alpha[, colnames(dat.alpha) != "year"], by = c("country", "ind"), all.x = TRUE)
        dat$method <- "average"
    }
    dat$unit.cost <- rowSums(dat[, pasted("alpha", list.ind.in)]*dat[, pasted("p", list.ind.in)]^(1-dat$rho))^(1/(1-dat$rho))
    return(dat)
}

pro.t <- calc.unit.cost(pro, sum.rho, alphas.country, c("v", list.reclass))
pro.a <- calc.unit.cost(pro, sum.rho, alphas.country, c("v", list.reclass), method = "average")
uc <- rbind(pro.t, pro.a)

pro.t.alt <- calc.unit.cost(pro.alt, sum.rho.alt, alphas.country.alt, c("v", list.reclass.alt))
pro.a.alt <- calc.unit.cost(pro.alt, sum.rho.alt, alphas.country.alt, c("v", list.reclass.alt), method = "average")
uc.alt <- rbind(pro.t.alt, pro.a.alt)

pro.t.gs <- calc.unit.cost(pro.gs, sum.rho.gs, alphas.country.gs, c("v", list.reclass.gs))
pro.a.gs <- calc.unit.cost(pro.gs, sum.rho.gs, alphas.country.gs, c("v", list.reclass.gs), method = "average")
uc.gs <- rbind(pro.t.gs, pro.a.gs)

## Save unit costs
write.csv(uc, "../../output/estimated_params/unit_costs.csv", row.names = FALSE)

write.csv(uc.alt, "../../output/estimated_params/unit_costs_alt.csv", row.names = FALSE)

write.csv(uc.gs, "../../output/estimated_params/unit_costs_gs.csv", row.names = FALSE)

## Robustness: nesting in a different way
## Nested CES: inner nest for intermediate inputs outer nest for int inputs + value-added
est.inner <- clean.prod.est(pro, list.reclass, list.reclass, "g")

## Now do the estimation for the nested version of production function
## Inner nest          
f.exp.inner <- function(param, x) f.exp(param, x, list.reclass, "g")

res.rho.inner <- lapply(est.inner, function(dat) gmm(f.exp.inner, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100))

sum.rho.inner <- sum.result(res.rho.inner, list.reclass)

## Wedges version
alphas.inner.t <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country, function(ex.country)
    recover.alpha(ex.country, i.year, list.reclass, list.reclass, "g", sum.rho.inner, pro)))))

pro.inner.t <- calc.unit.cost(pro, sum.rho.inner, alphas.inner.t, list.reclass)

## Outer nest (Wedges)
colnames(pro.inner.t)[colnames(pro.inner.t) == "unit.cost"] <- "p.int"

pro.inner.t$int.level <- rowSums(pro.inner.t[, pasted(c(list.reclass), "level")])

est.outer.t <- clean.prod.est(pro.inner.t, list.reclass, c("v", "int"), "int")

f.exp.outer <- function(param, x) f.exp(param, x, c("v", "int"), "int")

res.rho.outer.t <- lapply(est.outer.t, function(dat) gmm(f.exp.outer, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100))

sum.rho.outer.t <- sum.result(res.rho.outer.t, list.reclass)

alphas.outer.t <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country, function(ex.country)
    recover.alpha(ex.country, i.year, list.reclass, c("v", "int"), "int", sum.rho.outer.t, pro.inner.t)))))

colnames(pro.inner.t)[colnames(pro.inner.t) == "rho"] <- "rho.inner"
colnames(pro.inner.t)[colnames(pro.inner.t) == "stderr"] <- "stderr.inner"

pro.outer.t <- calc.unit.cost(pro.inner.t, sum.rho.outer.t, alphas.outer.t, c("v", "int"))

colnames(pro.outer.t)[colnames(pro.outer.t) == "rho"] <- "rho.outer"
colnames(pro.outer.t)[colnames(pro.outer.t) == "stderr"] <- "stderr.outer"

## Average version

alphas.inner.a <- do.call("rbind", lapply(list.country, function(ex.country) recover.alpha(ex.country, "avg", list.reclass, list.reclass, "g", sum.rho.inner, pro, method = "average")))

pro.inner.a <- calc.unit.cost(pro, sum.rho.inner, alphas.inner.a, list.reclass, method = "average")

colnames(pro.inner.a)[colnames(pro.inner.a) == "unit.cost"] <- "p.int"

pro.inner.a$int.level <- rowSums(pro.inner.a[, pasted(c(list.reclass), "level")])

est.outer.a <- clean.prod.est(pro.inner.a, list.reclass, c("v", "int"), "int")

f.exp.outer <- function(param, x) f.exp(param, x, c("v", "int"), "int")

res.rho.outer.a <- lapply(est.outer.a, function(dat) gmm(f.exp.outer, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100))

sum.rho.outer.a <- sum.result(res.rho.outer.a, list.reclass)

alphas.outer.a <- do.call("rbind", lapply(list.country, function(ex.country) recover.alpha(ex.country, "avg", list.reclass, c("v", "int"), "int", sum.rho.outer.a, pro.inner.a, method = "average")))

colnames(pro.inner.a)[colnames(pro.inner.a) == "rho"] <- "rho.inner"
colnames(pro.inner.a)[colnames(pro.inner.a) == "stderr"] <- "stderr.inner"

pro.outer.a <- calc.unit.cost(pro.inner.a, sum.rho.outer.a, alphas.outer.a, c("v", "int"), method = "average")

colnames(pro.outer.a)[colnames(pro.outer.a) == "rho"] <- "rho.outer"
colnames(pro.outer.a)[colnames(pro.outer.a) == "stderr"] <- "stderr.outer"

## Save the result

write.csv(sum.rho.inner, "../../output/estimated_params/rho_nest_inner.csv", row.names = FALSE)

sum.rho.outer.t$method <- "year"
sum.rho.outer.a$method <- "average"
sum.rho.outer <- rbind(sum.rho.outer.t, sum.rho.outer.a)

write.csv(sum.rho.outer, "../../output/estimated_params/rho_nest_outer.csv", row.names = FALSE)

alphas.inner.t$method <- "year"
alphas.inner.a$method <- "average"

alphas.outer.t$method <- "year"
alphas.outer.a$method <- "average"

alphas.country.nest <- merge(rbind(alphas.inner.a, alphas.inner.t), rbind(alphas.outer.a, alphas.outer.t), by = c("country", "ind", "year", "method"))

write.csv(alphas.country.nest, "../../output/estimated_params/alphas_nest.csv", row.names = FALSE)

uc.nest <- rbind(pro.outer.t, pro.outer.a)

write.csv(uc.nest, "../../output/estimated_params/unit_costs_nest.csv", row.names = FALSE)

## One thing to note: Atalay (2017, AEJ Macro) and Cravino and Sotelo (2019) close to Leontieff. Why? US data.

## Check correlation of relative production shares and relative prices
check.cor.inner <- function(dat.ind, i.country, i.ind){
    dat.country.ind.out <- subset(dat.ind, country == i.country)
    var.y <- pasted(i.ind, "g")
    var.x <- paste0("rel.p.", i.ind, ".g")
    reg <- lm(dat.country.ind.out[, var.y] ~
                  dat.country.ind.out[, var.x])
    temp <- summary(reg)$coef
    return(data.frame(country = i.country, ind.out = dat.country.ind.out$ind[1], ind.in = i.ind, rho_inner = 1-temp[2,1], rho_inner_pvalue = temp[2,4]))
}

cor.share.price <- data.frame()
for (i in 1:3){
    for (i.country in setdiff(list.country, "ROW")){
        for (i.ind in c("bts", "hts")){
            cor.share.price <- rbind(cor.share.price, check.cor.inner(est.inner[[i]], i.country, i.ind))
        }
    }
}

## Example 
summary(cor.share.price$rho_inner[cor.share.price$ind.out == "g" & cor.share.price$ind.in == "hts"])

## US is an outlier
subset(cor.share.price, country == "USA")

prod.est.inner.usa <- clean.prod.est(subset(pro, country == "USA"), list.reclass, list.reclass, "g")

res.rho.inner.usa <- lapply(prod.est.inner.usa, function(dat) gmm(f.exp.inner, t0 = c(log.rho = 0), data.matrix(dat), method = "Brent", lower = -100, upper = 100)) ## Warning coming out because the relative shares and prices are highly co-linear. Think about Leontief.

(sum.rho.inner.usa <- sum.result(res.rho.inner.usa, list.reclass))

write.csv(sum.rho.inner.usa, "../../output/estimated_params/rho_nest_inner_only_usa.csv", row.names = FALSE)


