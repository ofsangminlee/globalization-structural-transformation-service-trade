## get_two_country.R
## Prepare data and parameters for the two-country (USA-ROW) model

## Solving package dependency with Renv
renv::load("../")

## Load functions and packages
source("../common_functions.R")
library("tidyverse")
library("directlabels")
library("RColorBrewer")

list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.row <- setdiff(list.country$code, "USA")

#############################################
## GDP, total labor force, GDP per capita. ##
#############################################

gdp.emp <- read.csv("../../output/cleaned_data/gdp_per_capita.csv")
gdp.emp <- do.call("rbind", lapply(split(gdp.emp, gdp.emp$year), function(dat) rbind(dat, data.frame(country = "TOT", year = dat$year[1], gdp = sum(dat$gdp), emp = sum(dat$emp), gdppc = sum(dat$gdp)/sum(dat$emp)),data.frame(country = "ZZZ", year = dat$year[1], gdp = sum(dat$gdp[dat$country != "USA"]), emp = sum(dat$emp[dat$country != "USA"]), gdppc = sum(dat$gdp[dat$country != "USA"])/sum(dat$emp[dat$country != "USA"]))))) # add information for total and ROW except USA.

gdp.emp <- subset(gdp.emp, country %in% c("USA", "ZZZ", "TOT"))


##################################
## Condense ICIO and load price ##
##################################

load("../../output/cleaned_data/cleaned_icio.RData", verbose = TRUE)

icio.two <- lapply(icio.years, function(dat.year){
    dat.year <- row.agg.country(dat.year, "ZZZ", list.row)
    dat.year <- col.agg.country(dat.year, "ZZZ", list.row)
    return(dat.year)
})

icio.two.fy <- icio.two[1]

list.re <- c("g", "bts", "hts")

list.country.two <- c("USA", "ZZZ")

## Load price data (ASSUMPTION: ROW's prices = those of USA, following KRS)
gopl <- read.csv("../../output/cleaned_data/gopl.csv")
gopl <- gopl[, c("country", "year", "ind", "pl")]
gopl <- pivot_wider(gopl, names_from = "ind", values_from = "pl", names_prefix = "p.")

gopl <- subset(gopl, country == "USA" & year == "X1995")
gopl <- rbind(gopl, gopl)
gopl$country <- c("USA", "ZZZ")


#####################################
## Plot nx term per sector and GDP ##
#####################################

## What US's sectoral net export over year? (To check specialization patterns)
## It is also possible to do sectoral net export / sectoral GDP.

nx.ind <- function(country, dat, i.ind){
    x.country <- sum(dat[(!dat$country.ind %in% c("VALU", "OUTPUT")) & (name.country(dat$country.ind) == country) & (name.ind(dat$country.ind) == i.ind), (colnames(dat) != "country.ind") & (name.country(colnames(dat)) != country) &  (colnames(dat) != "TOTAL")])
    i.country <- sum(dat[(!dat$country.ind %in% c("VALU", "OUTPUT")) & (name.country(dat$country.ind) != country) & (name.ind(dat$country.ind) == i.ind), (colnames(dat) != "country.ind") & (name.country(colnames(dat)) == country) &  (colnames(dat) != "TOTAL")])
    return(c(x.country, i.country, x.country - i.country))
}

gdp.ind <- function(country, dat, i.ind) dat[dat$country.ind == "VALU", name.country(colnames(dat)) == country & name.ind(colnames(dat)) == i.ind]

gdp.tot <- function(country, dat) sum(dat[dat$country.ind == "VALU", name.country(colnames(dat)) == country])

nx.gdp <- function(country, dat){
    res <- data.frame()
    for (i.ind in list.re){
        temp <- data.frame(country = country, ind = i.ind, gdp = gdp.ind(country, dat, i.ind), gdp.tot = gdp.tot(country, dat))
        temp2 <- data.frame(t(nx.ind(country, dat, i.ind)))
        colnames(temp2) <- c("export", "import", "nx")
        temp3 <- cbind(temp, temp2)
        res <- rbind(res, temp3)
    }
    return(res)
}

nx.gdp.usa <- lapply(icio.two, function(x) nx.gdp("USA", x))
for (i in 1:24){
    nx.gdp.usa[[i]]$year <- i + 1994
}

nx.gdp.usa <- do.call("rbind", nx.gdp.usa)

res <- nx.gdp.usa

nx.gdp.usa$nx.gdp <- nx.gdp.usa$nx/nx.gdp.usa$gdp.tot
nx.gdp.usa$nx <- nx.gdp.usa$nx/1000
nx.gdp.usa$ind <- ifelse(nx.gdp.usa$ind == "g", "Goods",
                  ifelse(nx.gdp.usa$ind == "bts", "Barely tradable services", "Highly tradable services"))
nx.gdp.usa$ind <- factor(nx.gdp.usa$ind, levels = c("Goods", "Highly tradable services", "Barely tradable services"))

pdf(file = "../../doc/figures/nx_usa_sector.pdf", width = 10, height = 6)
ggplot(nx.gdp.usa, aes(x = year, y = nx, by = ind)) +
    geom_line(aes(color = ind, linetype = ind)) +
    geom_point(aes(color = ind, shape = ind)) + 
    scale_color_manual(values = c("Goods" = "black", "Highly tradable services" = "red", "Barely tradable services" = "blue")) +
    scale_linetype_manual(values = c("Goods" = "solid", "Highly tradable services" = "dashed", "Barely tradable services" = "dotted")) +
    scale_shape_manual(values = c("Goods" = 17, "Highly tradable services" = 15, "Barely tradable services" = 16)) +
    scale_x_continuous(limits = c(1995, 2023), breaks = seq(1995, 2015, 5)) +
    geom_dl(aes(label = ind, color = ind), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
    theme(text = element_text(size = 15), legend.position = "none") +
    xlab("Year") +
    ylab("Net export (billion USD)")
dev.off()

pdf(file = "../../doc/figures/nx_usa_sector_paper.pdf", width = 10, height = 6)
ggplot(nx.gdp.usa, aes(x = year, y = nx, by = ind)) +
    geom_line(aes(color = ind, linetype = ind)) +
    geom_point(aes(color = ind, shape = ind), size = 2.0) + 
    scale_color_manual(values = c("Goods" = "black", "Highly tradable services" = "red", "Barely tradable services" = "blue")) +
    scale_linetype_manual(values = c("Goods" = "solid", "Highly tradable services" = "dashed", "Barely tradable services" = "dotted")) +
    scale_shape_manual(values = c("Goods" = 17, "Highly tradable services" = 15, "Barely tradable services" = 16)) +
    scale_x_continuous(limits = c(1995, 2023), breaks = seq(1995, 2015, 5)) +
    geom_dl(aes(label = ind, color = ind), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
    theme_bw() +
    theme(text = element_text(size = 15), axis.title = element_text(size = 15), legend.position = "none") +
    xlab("Year") +
    ylab("Net export (billion USD)")
dev.off()

## But the problem with above, you have to think about borrowing and lending choices (i.e. total net export of a country)
## Doing goods fraction might be a better idea.

res.export <- res[, c("country", "export", "year", "ind")]
res.export <- pivot_wider(res.export, values_from = export, names_from = ind)

res.import <- res[, c("country", "import", "year", "ind")]
res.import <- pivot_wider(res.import, values_from = import, names_from = ind)

clean.res <- function(res){
    res$total <- rowSums(res[, list.re])
    res[, paste0(list.re, ".frac")] <- res[, list.re]/res$total
    res <- res[, c("country", "year", paste0(list.re, ".frac"))]

    res.g <- res[, c("country", "year", "g.frac")]
    res.g <- pivot_wider(res.g, values_from = "g.frac", names_from = "year")
    return(res.g)
}

res.g.export <- clean.res(res.export)
res.g.import <- clean.res(res.import)

res.g.export$type <- "f.exports"
res.g.import$type <- "f.imports"

res.g <- rbind(res.g.export, res.g.import)
res.g <- pivot_longer(res.g, cols = as.character(c(1995:2018)), names_to = "year")

res.g <- pivot_wider(res.g, values_from = "value", names_from = "type")
res.g$year <- as.integer(res.g$year)

gap.f <- res.g$f.imports[res.g$year == 1995] - res.g$f.exports[res.g$year == 1995]

## Get colors from the Dark2 palette
dark2.colors <- brewer.pal(name = "Dark2", n = 3)

## Create label data
last.year <- 2018

label.data <- data.frame(
  year = last.year,
  y = c(res.g$f.imports[res.g$year == last.year], 
        res.g$f.exports[res.g$year == last.year] + gap.f),
  label = c("Fraction of imports\n(left axis)", "Fraction of exports\n(right axis)"),
  group = 1:2,
  color = dark2.colors[c(2,1)]
)

pdf(file = "../../doc/figures/specialization_usa_years_paper.pdf", width = 10, height = 6)
ggplot(res.g, aes(x = year)) +
    geom_line(aes(y = f.imports, group = 1), linetype = 1, linewidth = 0.8, color = dark2.colors[1]) +
    geom_line(aes(y = f.exports + gap.f, group = 2), linetype = 2, linewidth = 0.8, color = dark2.colors[2]) +
    geom_point(aes(y = f.imports, group = 1), size = 2.5, shape = 17, color = dark2.colors[1]) +
    geom_point(aes(y = f.exports + gap.f, group = 2), size = 2.5, shape = 15, color = dark2.colors[2]) +
    scale_y_continuous(
        "Goods fraction of US imports",
        sec.axis = sec_axis(~.-gap.f, name = "Goods fraction of US exports")
    ) +
    geom_dl(aes(label = label, y = y, color = color), data = label.data, method = list(dl.trans(x = x + 2.0), "last.points", cex = 1, hjust = 0.5)) +
    plot.time.options(x.limits = c(1995, 2022))
dev.off()

pdf(file = "../../doc/figures/specialization_usa_years_beamer.pdf", width = 10, height = 6)
ggplot(res.g, aes(x = year)) +
    geom_line(aes(y = f.imports, group = 1), linetype = 1, linewidth = 0.8, color = dark2.colors[1]) +
    geom_line(aes(y = f.exports + gap.f, group = 2), linetype = 2, linewidth = 0.8, color = dark2.colors[2]) +
    geom_point(aes(y = f.imports, group = 1), size = 2.5, shape = 17, color = dark2.colors[1]) +
    geom_point(aes(y = f.exports + gap.f, group = 2), size = 2.5, shape = 15, color = dark2.colors[2]) +
    scale_y_continuous(
        "Goods fraction of US imports",
        sec.axis = sec_axis(~.-gap.f, name = "Goods fraction of US exports")
    ) +
    geom_dl(aes(label = label, y = y, color = color), data = label.data, method = list(dl.trans(x = x + 2.0), "last.points", cex = 1, hjust = 0.5)) +
    plot.time.options(x.limits = c(1995, 2022), bw = FALSE)
dev.off()


################
## Get \phi's ##
################

## Get consumption data
clean.cons <- function(dat.icio.years, list.inds){

    ## Sectoral consumption by country
    icio.cons <- lapply(dat.icio.years, function(dat){
        dat <- row.agg.country(dat, "ALL", list.country.two)
        dat <- dat[, c("country.ind", pasted(list.country.two, "c"))]
        dat <- subset(dat, country.ind %in% pasted("ALL", list.inds))
        return(dat)
    })
    
    icio.cons <- lapply(icio.cons, function(dat){
        dat <- pivot_longer(dat, cols = pasted(list.country.two, "c"), names_to = "country", values_to = "value")
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
    for (i in 1:1){
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

cons.fy <- clean.cons(icio.two.fy, list.re)
cons.fy <- merge(cons.fy, gopl, by = c("country", "year"), all.x = TRUE)
cons.fy <- subset(cons.fy, country != "TOT")

## Get preference params (that were estimated without US data)
params.pref <- read.csv("../../output/estimated_params/sub_inc_elasticity_wo_usa.csv")
sigma <- params.pref$estimates[params.pref$params == "sigma"]
epss <- c("g" = 1, "bts" = params.pref$estimates[params.pref$params == "eps.bts"], "hts" = params.pref$estimates[params.pref$params == "eps.hts"])

## Two fixed effect terms (log + weights + epsilon) for any years.
list.no.g <- setdiff(list.re, "g")

fe.term <- function(ex.country, i.year){
    dat <- subset(cons.fy, country == ex.country & year == i.year)
    temp <- log(dat[, paste0("share.", list.no.g)]/dat[, "share.g"]) - (1-sigma)*log(dat[, paste0("p.",list.no.g)]/dat[, "p.g"]) - (1-sigma)*(epss[list.no.g]-1)*log(dat$pc.total/dat[,"p.g"]) - (epss[list.no.g]-1)*log(dat[, "share.g"])
    res <- cbind(data.frame(country = ex.country), temp)
    colnames(res) <- c("country", paste0("fe.term.", list.no.g))
    return(res)
}

## Recover phi from FE term for each year.
years.x <- paste0("X", 1995)

recover.phi <- function(ex.country, i.year, method = "year"){
    if (method == "year"){
        phi.terms <- fe.term(ex.country, i.year)[, paste0("fe.term.", list.no.g)]
    }
    if (method == "average"){
        temp <- do.call("rbind", lapply(years.x, function(x) fe.term(ex.country, x)))
        phi.terms <- colMeans(temp[,  paste0("fe.term.", list.no.g)])
    }
    
    phi.term.gap <- function(phis){
        phi.g <- 1-sum(phis)
        cond <- sum(((phi.g)^(-sapply(c(list.no.g), function(x) epss[x]))*phis - exp(phi.terms))^2) # phi.term - phi.terms from guessed phi's
        return(cond)
    }
    res.phi <- optim(c(0.3, 0.3), phi.term.gap, control = list(reltol = 1e-9))
    stopifnot(res.phi$convergence == 0)
    stopifnot(res.phi$value < 1e-7)
    phi.g <- 1-sum(res.phi$par)
    phi.bts <- res.phi$par[1]
    phi.hts <- res.phi$par[2]
    phis <- c(1-sum(res.phi$par), res.phi$par)
    names(phis) <- c("g", list.no.g)
    return(phis)
}

recover.phi("USA", "X1995")

phis.usa.zzz <- do.call("rbind", lapply(years.x, function(i.year) do.call("rbind", lapply(list.country.two, function(ex.country) data.frame(country = ex.country, year = i.year, t(recover.phi(ex.country, i.year)))))))

## Note: I use ZZZ instead of ROW because in the USA-ROW model, ROW = ROW + 65 countries.
write.csv(phis.usa.zzz, file = "../../output/estimated_params/weight_util_zzz.csv", row.names = FALSE)

##################
## Get \alpha's ##
##################

clean.prod <- function(dat.icio, list.inds){

    list.country.ind <- unlist(lapply(list.country.two, function(x) pasted(x, list.inds)))

    ## Sectoral input consumption by country-industry.
    icio.prod <- lapply(dat.icio, function(dat){
        dat <- row.agg.country(dat, "ALL", list.country.two)
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

    for (i in 1:1){
        icio.prod[[i]][, "year"] <- paste0("X", 1994 + i)
    }

    icio.prod <- do.call("rbind", icio.prod)

    ## Relative share information
    for (i.ind in setdiff(c("v", list.inds), "g")){
        icio.prod[, pasted(i.ind, "g")] <- icio.prod[, pasted(i.ind, "level")]/icio.prod[, "g.level"]
    }
        return(icio.prod)
}

icio.prod <- clean.prod(icio.two.fy, list.re)

## Add price data
icio.prod <- merge(icio.prod, gopl, by = c("country", "year"), all.x = TRUE)
## Add price of value-added (average wage term)
icio.prod <- merge(icio.prod, gdp.emp, by = c("country", "year"), all.x = TRUE)
icio.prod$p.v <- icio.prod$gdppc
icio.prod$gdppc <- NULL

## Load rho's
sum.rho <- read.csv("../../output/estimated_params/rho_wo_usa.csv")

fe.term.prod <- function(ex.country, i.year, list.inds, res.rho, dat.prod){
    list.ind.from <- setdiff(c("v", list.inds), "g")
    res <- data.frame(ind.to = rep(list.inds, each = length(list.ind.from)), ind.from = rep(list.ind.from, length(list.ind.from)))
    res$log.alpha.ratio <- NA
    for (i.ind.to in list.inds){
        rho <- res.rho$rho[res.rho$ind == i.ind.to]
        dat <- subset(dat.prod, country == ex.country & year == i.year & ind == i.ind.to)
        for (i.ind.from in list.ind.from){
            temp <- log(dat[, paste0(i.ind.from, ".g")]) - (1-rho)*log(dat[, paste0("p.", i.ind.from)]/dat[, "p.g"])
            res$log.alpha.ratio[res$ind.to == i.ind.to & res$ind.from == i.ind.from] <- temp
        }
    }
    return(res)
}

recover.alpha <- function(ex.country, i.year, list.inds, res.rho, dat.prod, method = "year"){
    list.ind.from <- setdiff(c("v", list.inds), "g")
    if (method == "year"){
        temp <- fe.term.prod(ex.country, i.year, list.inds, res.rho, dat.prod)
    }
    if (method == "average"){
        temp <- do.call("rbind", lapply(years.x, function(x) fe.term.prod(ex.country, x, list.inds, res.rho, dat.prod)))
        temp <- split(temp, paste(temp$ind.to, temp$ind.from))
        temp <- do.call("rbind", lapply(temp, function(dat) data.frame(ind.to = dat$ind.to[1], ind.from = dat$ind.from[1], log.alpha.ratio = mean(dat$log.alpha.ratio))))
    }
    temp$alpha.ratio <- exp(temp$log.alpha.ratio)
    temp$log.alpha.ratio <- NULL
    temp <- pivot_wider(temp, names_from = ind.from, values_from = alpha.ratio)
    temp$alpha.g <- 1/(1 + rowSums(temp[, list.ind.from]))
    for (i.ind in list.ind.from){
        temp[, paste0("alpha.", i.ind)] <- temp$alpha.g*temp[, i.ind]
    }
    stopifnot(all.equal(rowSums(temp[, paste0("alpha.", c("v", list.inds))]),rep(1,length(list.inds))))
    temp$country <- ex.country
    temp <- temp[, c("country", "ind.to", paste0("alpha.", c("g", list.ind.from)))]
    colnames(temp)[2] <- "ind"
    temp$year <- i.year
    return(temp)
}

res.usa.zzz <- rbind(recover.alpha("USA", "X1995", list.re, sum.rho, icio.prod), recover.alpha("ZZZ", "X1995", list.re, sum.rho, icio.prod))

write.csv(res.usa.zzz, file = "../../output/estimated_params/alphas_zzz.csv", row.names = FALSE)

## Unit cost for productivity estimation
## Input cost for variety
pro <- merge(icio.prod, sum.rho, by = "ind", all.x = TRUE)
pro.t <- merge(pro, res.usa.zzz, by = c("country", "ind", "year"), all.x = TRUE)
pro.t$unit.cost <- rowSums(pro.t[, paste0("alpha.", c("v", list.re))]*pro.t[, paste0("p.", c("v", list.re))]^(1-pro.t$rho))^(1/(1-pro.t$rho))

#################
## Trade costs ##
#################

list.country.ordered <- list.country.two

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

list.re <- c("g", "bts", "hts")
icio.trade.ind <- clean.trade(icio.two, list.re)

theta = data.frame(ind = list.re, theta = c(4,4,4))
eta <- 2
theta$gamma <- gamma((theta$theta + 1 - eta)/theta$theta)^(1/(1-eta))

cal.tau <- function(trade, theta, i.ind, i.year){

    tau <- trade
    tau[, colnames(tau) != "country"] <- NA

    own.share <- trade
    own.share[, colnames(own.share) != "country"] <- NA

    trade.share <- trade
    trade.share[, colnames(trade.share) != "country"] <- NA
    
    for (exp.country in list.country.ordered){
        for (imp.country in list.country.ordered){
            if (exp.country == imp.country){
                tau[tau$country == exp.country, imp.country] <- 1
                own.share[own.share$country == exp.country, imp.country] <- NA
                trade.share[trade.share$country == exp.country, imp.country] <- NA
            }
            else {
                pi.ii <- trade[trade$country == exp.country, exp.country]/sum(trade[, exp.country])
                pi.ij <- trade[trade$country == exp.country, imp.country]/sum(trade[, imp.country])
                theta.ind <- theta$theta[theta$ind == i.ind]

                tau[tau$country == exp.country, imp.country] <- (pi.ij/pi.ii)^(-1/theta.ind)
                own.share[own.share$country == exp.country, imp.country] <- pi.ii
                trade.share[trade.share$country == exp.country, imp.country] <- pi.ij
            }
        }
    }
    return(list(tau = tau,own.share = own.share, trade.share = trade.share))
}

tau <- own.share <- trade.share <- list()

years.x <- paste0("X", 1995:2018)

for (j in 1:3){
    i.ind <- list.re[j]
    temp.1 <- temp.3 <- temp.4 <- list()
    for (i in 1:24){
        temp <- cal.tau(icio.trade.ind[[i.ind]][[i]], theta, i.ind, years.x[i])
        temp.1[[i]] <- temp[["tau"]]
        temp.3[[i]] <- temp[["own.share"]]
        temp.4[[i]] <- temp[["trade.share"]]
    }
    tau[[j]] <- temp.1
    own.share[[j]] <- temp.3
    trade.share[[j]] <- temp.4
}

names(tau) <- names(own.share) <- names(trade.share) <- list.re

save(tau, file = "../../output/estimated_params/tau_zzz.RData")

tau$g[1]
tau$g[24]

tau$hts[1]
tau$hts[24]

tau$bts[1]
tau$bts[24]

## trade.share$ps[1]
## trade.share$ps[24]

##################
## Productivity ##
##################

prod.all <- data.frame(country = character(), year = integer(), ind = character(), productivity = numeric())

unit.cost <- pro.t

for (ex.country in list.country.ordered){
    for (i.year in 1995){
        for (i.ind in list.re){
            gamma.ind <- theta$gamma[theta$ind == i.ind]
            theta.ind <- theta$theta[theta$ind == i.ind]
            p.ind <- gopl[gopl$country == ex.country & gopl$year == paste0("X", i.year), paste0("p.", i.ind)]
            r.ind <- unit.cost$unit.cost[unit.cost$country == ex.country & unit.cost$year == paste0("X", i.year) & unit.cost$ind == i.ind]
            trade <- icio.trade.ind[[i.ind]][[i.year - 1994]]
            pi.ii <- trade[trade$country == ex.country, ex.country]/sum(trade[, ex.country])
            temp <- gamma.ind*(1/p.ind)*r.ind*(pi.ii^(1/theta.ind))
            names(temp) <- NULL
            prod.all <- rbind(prod.all, data.frame(country = ex.country, year = i.year, ind = i.ind, productivity = temp))
        }
    }
}

## Save productivities for 1995 ##
write.csv(prod.all, "../../output/estimated_params/productivity_zzz.csv", row.names = FALSE)

## Productivity (DATA)
prod.usa <- subset(prod.all, country == "USA")
prod.growth <- read.csv("../../output/cleaned_data/us_prod.csv")

for (i.year in 1996:2018){
    for (i.ind in list.re){
        prod.usa <- rbind(prod.usa, data.frame(country = "USA", year = i.year, ind = i.ind, productivity = prod.usa$productivity[prod.usa$year == 1995 & prod.usa$ind == i.ind]*prod.growth[prod.growth$ind == i.ind, paste0("X", i.year)]))
    } 
}

write.csv(prod.usa, "../../output/estimated_params/prod_usa_data.csv", row.names = FALSE)


## Two country labor force
pwt <- read.csv("../../output/cleaned_data/pwt_variables.csv")
pwt <- pwt[, c("country", "year", "emp")]
pwt <- do.call("rbind", lapply(split(pwt, pwt$year), function(dat){
    temp.usa <- dat[dat$country == "USA", ]
    temp.not <- dat[dat$country != "USA", ]
    temp.not <- data.frame(country = "ZZZ", year = dat$year[1], emp = sum(temp.not$emp))
    return(rbind(temp.usa, temp.not))
}))

write.csv(pwt, file = "../../output/cleaned_data/pwt_zzz.csv", row.names = FALSE)

## For plotting
clean.prod <- function(dat.icio, list.inds){

    list.country.ind <- unlist(lapply(list.country.two, function(x) pasted(x, list.inds)))

    ## Sectoral input consumption by country-industry.
    icio.prod <- lapply(dat.icio, function(dat){
        dat <- row.agg.country(dat, "ALL", list.country.two)
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

    ## Relative share information
    for (i.ind in setdiff(c("v", list.inds), "g")){
        icio.prod[, pasted(i.ind, "g")] <- icio.prod[, pasted(i.ind, "level")]/icio.prod[, "g.level"]
    }
        return(icio.prod)
}

icio.prod <- clean.prod(icio.two, list.re)
va.base <- icio.prod[, c("country", "ind", "v.level", "year")]
va.base <- pivot_wider(va.base, values_from = "v.level", names_from = "ind")
va.base$tot <- rowSums(va.base[, list.re])
for (i.ind in list.re){
    va.base[, pasted("va", i.ind)] <- va.base[, i.ind]/va.base$tot
}

write.csv(va.base, "../../output/cleaned_data/usa_va_share.csv", row.names = FALSE)
