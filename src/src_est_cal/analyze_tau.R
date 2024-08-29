## analyze_tau.R
## Analyze the globalization pattern (i.e., changes in tau)

## Solving package dependency with Renv
renv::load("../")

## Load libraries
library("tidyverse")
source("../common_functions.R")

library("directlabels")
library("cowplot")
library("ggpattern")
library("RColorBrewer")
## ## To see the color palettes
## display.brewer.all()

## Load trade matrix and calculated trade costs
## Trade matrix: used as weights
load("../../output/estimated_params/tau.RData", verbose = TRUE)
load("../../output/cleaned_data/trade_matrix.RData", verbose = TRUE)
list.country <- tau[[1]][[1]]$country
identical(list.country, list.country[order(list.country)])
list.country.ordered <- list.country
list.reclass <- c("g", "bts", "hts")

## Head and Ries case
load("../../output/estimated_params/tau_head_ries.RData", verbose = TRUE)
list.country.hs <- tau.hs[[1]][[1]]$country
identical(list.country.hs, list.country.hs[order(list.country.hs)])

## Cases for robustness
load("../../output/estimated_params/tau_alt.RData", verbose = TRUE)
load("../../output/cleaned_data/trade_matrix_alt.RData", verbose = TRUE)
list.reclass.alt <- c("g", "cs", "ps")

load("../../output/estimated_params/tau_gs.RData", verbose = TRUE)
load("../../output/cleaned_data/trade_matrix_gs.RData", verbose = TRUE)
list.reclass.gs <- c("g", "s")

load("../../output/estimated_params/tau_low.RData", verbose = TRUE)
load("../../output/estimated_params/tau_high.RData", verbose = TRUE)

load("../../output/estimated_params/tau_orig.RData", verbose = TRUE)


## Domestic trade: Diagonal 1 -> NA
diag.na <- function(tau){
    for (i in 1:length(tau)){
        tau.ind  <- tau[[i]]
        for (j in 1:length(tau.ind)){
            tau.ind.year <- tau.ind[[j]]
            for (i.country in list.country.ordered){
                tau.ind.year[tau.ind.year$country == i.country, i.country] <- NA
            }
            tau.ind[[j]] <- tau.ind.year            
        }
        tau[[i]] <- tau.ind
    }
    return(tau)
}

tau <- diag.na(tau)
tau.hs <- diag.na(tau.hs)
tau.low <- diag.na(tau.low)
tau.high <- diag.na(tau.high)

#################################################
## Box-plot for general trend for 1995 vs 2018 ##
#################################################

## General trend: trade cost asymmetry is decreasing over time.
## HERE YOU CAN HAVE GRAVITY EQUATION VERSION.

## Load a list for developing and advanced countries
list.hl <- read.csv("../../data/others/low_high_income.csv", skip = 3)

## BOX PLOTS for country-group pairs

## Matrix into long format
## First add year and sector
clean.tau <- function(dat.tau, method = "wedge"){

    if (method == "wedge"){
        list.c <- list.country.ordered
        list.r <- list.reclass
    }
    
    else if (method == "gravity"){
        list.c <- setdiff(list.country.ordered, "ROW")
        list.r <- setdiff(list.reclass, "cs")
    }    
    
    tau.2 <- dat.tau
    for (i.ind in list.r){
        for (i.year in 1995:2018){
            temp <- dat.tau[[i.ind]][[i.year-1994]]
            temp$ind <- i.ind
            temp$year <- i.year
            tau.2[[i.ind]][[i.year-1994]] <- temp
        }
    }
    
    tau.2 <- do.call("rbind", lapply(tau.2, function(x) do.call("rbind", x)))
    tau.2 <- pivot_longer(tau.2, cols = all_of(list.c), names_to = "destination")
    tau.2$origin <- tau.2$country
    tau.2$country <- NULL
    
    list.hl <- list.hl[, c("Country", "IMF")]
    colnames(list.hl) <- c("country", "hl")
    
    tau.2 <- merge(tau.2, list.hl, by.x = "origin", by.y = "country", all.x = TRUE)
    tau.2$origin.hl <- tau.2$hl
    tau.2$hl <- NULL
    
    tau.2 <- merge(tau.2, list.hl, by.x = "destination", by.y = "country", all.x = TRUE)
    tau.2$destination.hl <- tau.2$hl
    tau.2$hl <- NULL
    
    tau.2$origin.destination <- ifelse(tau.2$origin.hl == 0 & tau.2$destination.hl == 0, "DV-DV",
                                ifelse(tau.2$origin.hl == 0 & tau.2$destination.hl == 1, "DV-AD",
                                ifelse(tau.2$origin.hl == 1 & tau.2$destination.hl == 0, "AD-DV", "AD-AD")))
    
    tau.2$origin.destination.year <- paste0(tau.2$origin.destination, "-", substr(tau.2$year,3,4))
    
    return(tau.2)
}

prepare.plot <- function(tau, method = "wedge", is.diff = FALSE){
    tau.plot <- clean.tau(tau, method = method)
    if (is.diff){
        tau.plot <- subset(tau.plot, year == 2018)
    }
    else {
        tau.plot <- subset(tau.plot, year %in% c(1995, 2018))
    }
    tau.plot$ind <- toupper(tau.plot$ind)
    tau.plot <- add.title(tau.plot)
    tau.plot <- subset(tau.plot, ind %in% c("G", "HTS"))
    tau.plot <- subset(tau.plot, !is.na(value)) ## Remove domestic trade
    return(tau.plot)
}

tau.plot <- prepare.plot(tau)
tau.plot.hs <- prepare.plot(tau.hs)
tau.plot.low <- prepare.plot(tau.low)
tau.plot.high <- prepare.plot(tau.high)

## Adding ALL-ALL pairs AND add factor levels
add.all <- function(tau.plot){
    tau.plot.2 <- tau.plot
    tau.plot.2$origin.destination <- "ALL"
    tau.plot.2$origin.destination.year <- paste0(tau.plot.2$origin.destination, "-", substr(tau.plot.2$year,3,4))
    tau.plot <- rbind(tau.plot, tau.plot.2)
    tau.plot$origin.destination <- factor(tau.plot$origin.destination, levels = c("ALL", "AD-AD", "AD-DV", "DV-AD", "DV-DV"))
    return(tau.plot)
}

tau.plot <- add.all(tau.plot)
tau.plot.hs <- add.all(tau.plot.hs)
tau.plot.low <- add.all(tau.plot.low)
tau.plot.high <- add.all(tau.plot.high)


## ## Plain-vanilla plotting
## plot.boxes <- function(tau.plot, num.ylim, bw2 = TRUE)
##     ggplot(tau.plot, aes(x = origin.destination, y = value, fill = as.factor(year))) +
##         geom_boxplot(notch = TRUE) +
##         xlab("Origin-destination")+
##         ylab("Trade costs") + 
##         scale_fill_manual(name = "Year", values = c("1995" = "grey90", "2018" = "grey40")) + 
##         coord_cartesian(ylim = num.ylim) + # YOU NEED THIS. This does not change underlying data unlike ylim(c(0,20)). If you do ylim(c(0,20)),only values within (0,20) will be included.
##         plot.scatter.options(bw = bw2)

plot.boxes.options <- function(bw = TRUE){
    res <- list(theme(axis.text = element_text(size = 13), text = element_text(size = 15), strip.text = element_text(size =16))# If I don't set sizes for  axis.text separately, due to the incompleteness of the theme, 15*0.8 (default rel size) will be the default size for axis.text. & strip.text size would affect the size of the subtitle.
                )
    if (bw){
        res <- append(list(theme_bw(), theme(strip.background = element_blank())), res)
        return(res)
    }
    else {
        return(res)
    }
}

plot.boxes.color <- function(tau.plot, num.ylim)
    ggplot(tau.plot, aes(x = origin.destination, y = value, fill = origin.destination, pattern = as.factor(year))) +
        geom_boxplot_pattern(alpha = 0.6, pattern_angle = 45, pattern_density = 0.01, pattern_spacing = 0.03, pattern_fill = "black", notch = TRUE) +
        scale_pattern_manual(name = "Year", values = c("1995" = "none", "2018" = "stripe")) +
        xlab("Origin-destination")+
        ylab("Trade costs") +
        scale_fill_brewer(palette = "Dark2") +
        coord_cartesian(ylim = num.ylim) # YOU NEED THIS. This does not change underlying data unlike ylim(c(0,20)). If you do ylim(c(0,20)),only values within (0,20) will be included.

pdf(file = "../../doc/figures/box_paper.pdf", width = 10, height = 6)
plot.boxes.color(tau.plot, c(0,24)) + plot.boxes.options() + guides(fill = guide_none()) + facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/box_beamer.pdf", width = 10, height = 6)
plot.boxes.color(tau.plot, c(0,24)) + plot.boxes.options(FALSE) + guides(fill = guide_none()) + facet_wrap(~title)
dev.off()

pdf(file = "../../doc/figures/box_paper_hs.pdf", width = 10, height = 6)
plot.boxes.color(tau.plot.hs, c(0,24)) + plot.boxes.options() + guides(fill = guide_none()) + facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/box_beamer_hs.pdf", width = 10, height = 6)
plot.boxes.color(tau.plot.hs, c(0,24)) + plot.boxes.options(FALSE) + guides(fill = guide_none()) + facet_wrap(~title)
dev.off()

## pdf(file = "../../doc/figures/box_paper_low.pdf", width = 10, height = 6)
## plot.boxes.color(tau.plot.low, c(0,300)) + plot.boxes.options() + guides(fill = guide_none()) + facet_wrap(~title.alp)
## dev.off()

## pdf(file = "../../doc/figures/box_beamer_low.pdf", width = 10, height = 6)
## plot.boxes.color(tau.plot.low, c(0,300)) + plot.boxes.options(FALSE) + guides(fill = guide_none()) + facet_wrap(~title)
## dev.off()

## pdf(file = "../../doc/figures/box_paper_high.pdf", width = 10, height = 6)
## plot.boxes.color(tau.plot.high, c(0,24)) + plot.boxes.options() + guides(fill = guide_none()) + facet_wrap(~title.alp)
## dev.off()

## pdf(file = "../../doc/figures/box_beamer_high.pdf", width = 10, height = 6)
## plot.boxes.color(tau.plot.high, c(0,24)) + plot.boxes.options(FALSE) + guides(fill = guide_none()) + facet_wrap(~title)
## dev.off()


##########################################
## LINE PLOTS for across-time variation ##
##########################################

## Function to calculate statistics (weighted mean, simple mean, 1Q, 2Q, 3Q) given origin and destination country sets.
summary.tau <- function(trade.mat, tau.mat, list.from, list.to, method = "wedge"){
    if (method == "wedge"){
        list.c <- list.country.ordered
    }
    else if (method == "gravity"){
        list.c <- setdiff(list.country.ordered, "ROW")
        list.from <- setdiff(list.from, "ROW")
        list.to <- setdiff(list.to, "ROW")
    }
    for (i.country in list.c){
        trade.mat[trade.mat$country == i.country, i.country] <- 0
    }
    ## Trade-weighted mean
    res.wm <- sum(tau.mat[tau.mat$country %in% list.from, list.to]*trade.mat[trade.mat$country %in% list.from, list.to], na.rm = TRUE)/sum(trade.mat[trade.mat$country %in% list.from, list.to], na.rm = TRUE)
        
    for (i.country in list.c){
        tau.mat[tau.mat$country == i.country, i.country] <- NA # Turn diagonal into NAs
    }
    temp <- tau.mat[tau.mat$country %in% list.from, list.to]
    temp <- unlist(temp)
    temp <- temp[!is.na(temp)]
    ## Mean
    res.m <- mean(temp)
    ## 1Q, 2Q, 3Q
    res.q <- quantile(temp, c(0.25, 0.5, 0.75))
    res <- data.frame(cbind(res.m, res.wm, t(res.q)))
    colnames(res) <- c("mean", "w.mean", "Q1", "Q2", "Q3")
    return(res)
}

## Trade cost statistics for developing and advanced countries
list.dev <- list.hl$Country[list.hl$IMF == 0]
list.adv <- list.hl$Country[list.hl$IMF == 1]

do.across.ind.year <- function(list.reclass, num.years, fn, ...)
    do.call("rbind", lapply(list.reclass, function(i.ind) do.call("rbind", lapply(1:num.years, function(i.year) fn(i.ind, i.year, ...)))))

add.names <- function(res, origin, destination, i.ind, num.year){
    res$origin <- origin
    res$destination <- destination
    res$origin.destination <- paste0(origin,"-",destination)
    res$ind <- i.ind
    res$year <- num.year
    return(res)
}

summary.22 <- function(i.ind, i.year, dat.trade, dat.tau){
    trade.mat <- dat.trade[[i.ind]][[i.year]]
    tau.mat <- dat.tau[[i.ind]][[i.year]]
    num.year <- i.year + 1994
    ## 2 by 2
    res.all <- summary.tau(trade.mat, tau.mat, list.country, list.country)
    res.all <- add.names(res.all, "ALL", "ALL", i.ind, num.year)
    
    res.aa <- summary.tau(trade.mat, tau.mat, list.adv, list.adv)
    res.aa <- add.names(res.aa, "AD", "AD", i.ind, num.year)
    
    res.ad <- summary.tau(trade.mat, tau.mat, list.adv, list.dev)
    res.ad <- add.names(res.ad, "AD", "DV", i.ind, num.year)
    
    res.da <- summary.tau(trade.mat, tau.mat, list.dev, list.adv)
    res.da <- add.names(res.da, "DV", "AD", i.ind, num.year)
    
    res.dd <- summary.tau(trade.mat, tau.mat, list.dev, list.dev)
    res.dd <- add.names(res.dd, "DV", "DV", i.ind, num.year)
    
    return(rbind(res.all, res.aa, res.ad, res.da, res.dd))
}

summary.year <- do.across.ind.year(list.reclass, (2018-1995+1), summary.22, icio.trade.ind, tau)
summary.year.hs <- do.across.ind.year(list.reclass, (2018-1995+1), summary.22, icio.trade.ind, tau.hs)

write.csv(summary.year, "../../doc/figures/trade_cost_22_num.csv", row.names = FALSE)
write.csv(summary.year.hs, "../../doc/figures/trade_cost_22_num_hs.csv", row.names = FALSE)

summary.plot <- subset(summary.year, ind != "bts")
summary.plot$origin.destination <- ifelse(summary.plot$origin.destination == "ALL-ALL", "ALL", summary.plot$origin.destination)
summary.plot$origin.destination <- factor(summary.plot$origin.destination, levels = c("ALL", "AD-AD", "AD-DV", "DV-AD", "DV-DV"))
summary.plot$ind <- toupper(summary.plot$ind)
summary.plot <- add.title(summary.plot)

summary.plot.hs <- subset(summary.year.hs, ind != "bts")
summary.plot.hs$origin.destination <- ifelse(summary.plot.hs$origin.destination == "ALL-ALL", "ALL", summary.plot.hs$origin.destination)
summary.plot.hs$origin.destination <- factor(summary.plot.hs$origin.destination, levels = c("ALL", "AD-AD", "AD-DV", "DV-AD", "DV-DV"))
summary.plot.hs$ind <- toupper(summary.plot.hs$ind)
summary.plot.hs <- add.title(summary.plot.hs)

plot.median.22 <- function(dat.plot)
    ggplot(data = dat.plot, aes(x = year, y = Q2)) +
        geom_point(aes(shape = origin.destination, color = origin.destination), size = 2)+
        geom_line(aes(linetype = origin.destination, color = origin.destination)) + 
        scale_color_brewer(palette = "Dark2") +
        geom_dl(aes(label = origin.destination, color = origin.destination), method = list(dl.trans(x = x - 0.2), "first.points", "bumpup", cex = 1)) +
        labs(y = "Median trade cost", x = "Year")

pdf(file = "../../doc/figures/trade_cost_22_paper.pdf", width = 10, height = 6)
plot.median.22(summary.plot) + plot.time.options(x.limits = c(1992, 2018), bw = TRUE) + facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/trade_cost_22_beamer.pdf", width = 10, height = 6)
plot.median.22(summary.plot) + plot.time.options(x.limits = c(1992, 2018), bw = FALSE) + facet_wrap(~title)
dev.off()

pdf(file = "../../doc/figures/trade_cost_22_paper_hs.pdf", width = 10, height = 6)
plot.median.22(summary.plot.hs) + plot.time.options(x.limits = c(1992, 2018), bw = TRUE) + facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/trade_cost_22_beamer_hs.pdf", width = 10, height = 6)
plot.median.22(summary.plot.hs) + plot.time.options(x.limits = c(1992, 2018), bw = FALSE) + facet_wrap(~title)
dev.off()

#################################
## Country-by-country plotting ##
#################################

## Calculate trade weighted export and import trade costs

calc.w.ex.im <- function(dat.trade, dat.tau, list.reclass, initial.year = TRUE){

    tau.country.weighted <- data.frame()

    for (i.ind in list.reclass){
        ## Weights = initial year trade
        if (initial.year){
            temp.trade <- dat.trade[[i.ind]][[1]]
        }
        for (i.year in 1995:2018){
            ## Weights = yearly trade data
            if (!initial.year){
                temp.trade <- icio.trade.ind[[i.ind]][[i.year-1994]]
            }            
            temp.tau <- dat.tau[[i.ind]][[i.year-1994]]
            for (i.country in list.country){
                temp <- data.frame(i.country, i.year, i.ind, sum(temp.tau[temp.tau$country == i.country, setdiff(list.country,i.country)]*temp.trade[temp.trade$country == i.country, setdiff(list.country,i.country)])/sum(temp.trade[temp.trade$country == i.country, setdiff(list.country,i.country)]), sum(temp.tau[temp.tau$country != i.country, i.country]*temp.trade[temp.trade$country != i.country, i.country])/sum(temp.trade[temp.trade$country != i.country, i.country]))
                colnames(temp) <- c("country", "year", "ind", "tau.exp", "tau.imp")
                tau.country.weighted <- rbind(tau.country.weighted, temp)
            }
        }
    }
    tau.country.weighted$tau.exp.imp <- tau.country.weighted$tau.exp/tau.country.weighted$tau.imp
    return(tau.country.weighted)
}

tau.country.weighted <- calc.w.ex.im(icio.trade.ind, tau, list.reclass)
tau.country.weighted.hs <- calc.w.ex.im(icio.trade.ind, tau.hs, list.reclass)

## Plotting for countries

## Plotting
prepare.plot <- function(tau.country.weighted){
    dat.plot.tau <- pivot_longer(tau.country.weighted, cols = c("tau.exp", "tau.imp", "tau.exp.imp"), names_to = "type", values_to = "value")
    
    dat.plot.tau$ind <- toupper(dat.plot.tau$ind)
    dat.plot.tau$type <- ifelse(dat.plot.tau$type == "tau.exp", "Export cost",
                         ifelse(dat.plot.tau$type == "tau.imp", "Import cost",
                         ifelse(dat.plot.tau$type == "tau.exp.imp", "Relative export cost", "")))
    dat.plot.tau$labs <- paste0(dat.plot.tau$type, " (", dat.plot.tau$ind,")")
    return(dat.plot.tau)
}

dat.plot.tau.weighted <- prepare.plot(tau.country.weighted)
dat.plot.tau.weighted.hs <- prepare.plot(tau.country.weighted.hs)

## Save data
write.csv(dat.plot.tau.weighted, "../../doc/figures/tau_weighted_by_country.csv", row.names = FALSE)
write.csv(dat.plot.tau.weighted.hs, "../../doc/figures/tau_weighted_by_country_hs.csv", row.names = FALSE)

## Plotting

custom.linetype <- c("Export cost" = "solid", "Import cost" = "dotted")

plot.ex.im <- function(dat.plot.tau, ex.country, list.inds, default.color.base, default.linetype.base, default.shape.base, plot.title, bw2 = TRUE)
    ggplot(subset(dat.plot.tau, country == ex.country & ind %in% list.inds & type %in% c("Export cost", "Import cost")), aes(x = year, y = value)) +
        geom_line(aes(color = ind, linetype = type)) +    
        geom_point(aes(color = ind, shape = ind)) + 
        scale_color_manual(name = "Sector", values = default.color.base) +
        scale_linetype_manual(name = "Type", values = default.linetype.base) +
        scale_shape_manual(name = "Sector", values = default.shape.base) +
        geom_dl(aes(label = labs, color = ind), method = list(dl.trans(x = x - 0.2), "first.points", "bumpup", cex = 1)) +
        xlab("Year") +
        ylab("Trade costs") +
        ggtitle(plot.title) +
        plot.time.options(x.limits = c(1984, 2018), bw = bw2)

plot.rel <- function(dat.plot.tau, ex.country, list.inds, default.color.base, default.linetype.base, default.shape.base, plot.title, bw2 = TRUE)
    ggplot(subset(dat.plot.tau, country == ex.country & ind %in% list.inds & type %in% c("Relative export cost")), aes(x = year, y = log(value))) +
        geom_line(aes(color = ind, linetype = ind)) +    
        geom_point(aes(color = ind, shape = ind)) +
        scale_color_manual(name = "Sector", values = default.color.base) +
        scale_linetype_manual(name = "Type", values = default.linetype.base) +
        scale_shape_manual(name = "Sector", values = default.shape.base) +
        geom_dl(aes(label = ind, color = ind), method = list(dl.trans(x = x - 0.2), "first.points", "bumpup", cex = 1)) +
        xlab("Year") +
        ylab("Log(export cost/import cost)") +
        ggtitle(plot.title) +
        plot.time.options(x.limits = c(1993, 2018), bw = bw2)

plot.one.country <- function(dat.plot.tau.weighted, ex.country, list.inds, default.color.base, default.linetype.base, default.shape.base, title.i, title.ii, bw3 = TRUE, adjust.grid = FALSE, y.limits.1 = c(-10,10), y.limits.2 = c(-10,10)){

    p1 <- plot.ex.im(dat.plot.tau.weighted, ex.country, list.inds, default.color.base, custom.linetype, default.shape.base, title.i, bw2 = bw3) + theme(plot.title = element_text(hjust = 0.5, size = 15))

    p2 <- plot.rel(dat.plot.tau.weighted, ex.country, list.inds, default.color.base, default.linetype.base, default.shape.base, title.ii, bw2 = bw3) + theme(plot.title = element_text(hjust = 0.5, size = 15))

    if(adjust.grid){
        p1 <- p1 + scale_y_continuous(limits = y.limits.1)
        p2 <- p2 + scale_y_continuous(limits = y.limits.2)
    }
    
    return(plot_grid(p1, p2, nrow = 1))
}

plot.one.paper <- function(ex.country, adjust.grid.b = FALSE, y.limits.1.b = c(-10,10), y.limits.2.b = c(-10,10))
    plot.one.country(dat.plot.tau.weighted, ex.country, c("G", "HTS"), default.color.base, default.linetype.base, default.shape.base, "(i) Trade costs (weighted means)", "(ii) Log(export cost/import cost)", adjust.grid = adjust.grid.b, y.limits.1 = y.limits.1.b, y.limits.2 = y.limits.2.b)

plot.one.beamer <- function(ex.country, adjust.grid.b = FALSE, y.limits.1.b = c(-10,10), y.limits.2.b = c(-10,10))
    plot.one.country(dat.plot.tau.weighted, ex.country, c("G", "HTS"), default.color.base, default.linetype.base, default.shape.base, "Trade costs (weighted means)", "(ii) Log(export cost/import cost)", bw3 = FALSE, adjust.grid = adjust.grid.b, y.limits.1 = y.limits.1.b, y.limits.2 = y.limits.2.b)

plot.one.paper.hs <- function(ex.country, adjust.grid.b = FALSE, y.limits.1.b = c(-10,10), y.limits.2.b = c(-10,10))
    plot.one.country(dat.plot.tau.weighted.hs, ex.country, c("G", "HTS"), default.color.base, default.linetype.base, default.shape.base, "(i) Trade costs (weighted means)", "(ii) Log(export cost/import cost)", adjust.grid = adjust.grid.b, y.limits.1 = y.limits.1.b, y.limits.2 = y.limits.2.b)

plot.one.beamer.hs <- function(ex.country, adjust.grid.b = FALSE, y.limits.1.b = c(-10,10), y.limits.2.b = c(-10,10))
    plot.one.country(dat.plot.tau.weighted.hs, ex.country, c("G", "HTS"), default.color.base, default.linetype.base, default.shape.base, "Trade costs (weighted means)", "(ii) Log(export cost/import cost)", bw3 = FALSE, adjust.grid = adjust.grid.b, y.limits.1 = y.limits.1.b, y.limits.2 = y.limits.2.b)

## Plot (China)
pdf(file = "../../doc/figures/tau_country_chn_paper.pdf", width = 10, height = 5)
plot.one.paper("CHN", adjust.grid.b = TRUE, y.limits.1.b = c(1,14), y.limits.2.b = c(-0.4, 2.2))
dev.off()

pdf(file = "../../doc/figures/tau_country_chn_beamer.pdf", width = 10, height = 6)
plot.one.beamer("CHN", adjust.grid.b = TRUE, y.limits.1.b = c(1,14), y.limits.2.b = c(-0.4, 2.2))
dev.off()

pdf(file = "../../doc/figures/tau_country_chn_paper_hs.pdf", width = 10, height = 5)
plot.one.paper.hs("CHN", adjust.grid.b = TRUE, y.limits.1.b = c(1,14), y.limits.2.b = c(-0.4, 2.2))
dev.off()

pdf(file = "../../doc/figures/tau_country_chn_beamer_hs.pdf", width = 10, height = 6)
plot.one.beamer.hs("CHN", adjust.grid.b = TRUE, y.limits.1.b = c(1,14), y.limits.2.b = c(-0.4, 2.2))
dev.off()

## China result print out
write.csv(subset(dat.plot.tau.weighted.hs, country == "CHN" & year %in% c(1995, 2018) & ind %in% c("G", "HTS")), "../../doc/nums/china_tau_hs.csv", row.names = FALSE)

## Plot (India)
pdf(file = "../../doc/figures/tau_country_ind_paper.pdf", width = 10, height = 6)
plot.one.paper("IND")
dev.off()

pdf(file = "../../doc/figures/tau_country_ind_beamer.pdf", width = 10, height = 6)
plot.one.beamer("IND")
dev.off()

## Plot (Vietnam)
pdf(file = "../../doc/figures/tau_country_vnm_paper.pdf", width = 10, height = 6)
plot.one.paper("VNM")
dev.off()

pdf(file = "../../doc/figures/tau_country_vnm_beamer.pdf", width = 10, height = 6)
plot.one.beamer("VNM")
dev.off()

## Plot (Lithuania)
pdf(file = "../../doc/figures/tau_country_ltu_paper.pdf", width = 10, height = 6)
plot.one.paper("LTU")
dev.off()

pdf(file = "../../doc/figures/tau_country_ltu_beamer.pdf", width = 10, height = 6)
plot.one.beamer("LTU")
dev.off()

###########################
## Proportionality index ##
###########################

## Construct the index

tau.country.weighted <- calc.w.ex.im(icio.trade.ind, tau, list.reclass)
tau.country.weighted.hs <- calc.w.ex.im(icio.trade.ind, tau.hs, list.reclass)

construct.index <- function(tau.country.weighted, list.inds){
    tau.index <- subset(tau.country.weighted, ind %in% list.inds & year %in% c(1995, 2018))
    tau.index <- tau.index[, c("country", "year", "ind", "tau.exp", "tau.imp")]
    tau.index <- pivot_wider(tau.index, names_from = c("year", "ind"), values_from = c("tau.exp", "tau.imp"))
    
    tau.index$growth.exp.g <- tau.index$tau.exp_2018_g/tau.index$tau.exp_1995_g
    tau.index$growth.imp.g <- tau.index$tau.imp_2018_g/tau.index$tau.imp_1995_g
    tau.index$glre.g <- log(tau.index$growth.exp.g) - log(tau.index$growth.imp.g)

    tau.index[, paste0("growth.exp.", list.inds[2])] <- tau.index[, paste0("tau.exp_2018_", list.inds[2])]/tau.index[, paste0("tau.exp_1995_", list.inds[2])]
    tau.index[, paste0("growth.imp.", list.inds[2])] <- tau.index[, paste0("tau.imp_2018_", list.inds[2])]/tau.index[, paste0("tau.imp_1995_", list.inds[2])]
    tau.index[, paste0("glre.", list.inds[2])] <- log(tau.index[, paste0("growth.exp.", list.inds[2])]) - log(tau.index[, paste0("growth.imp.", list.inds[2])])
    
    tau.index[, "index"] <- tau.index$glre.g - tau.index[, paste0("glre.", list.inds[2]), drop = TRUE]
    return(tau.index)
}

tau.index <- construct.index(tau.country.weighted, c("g", "hts"))

tau.index.hs <- construct.index(tau.country.weighted.hs, c("g", "hts")) # NOTE: even in the H-S setup, the index changes, because of the weights used for exports and imports are different.

tau.country.weighted.alt <- calc.w.ex.im(icio.trade.ind.alt, tau.alt, list.reclass.alt)
tau.index.alt <- construct.index(tau.country.weighted.alt, c("g", "ps"))

tau.country.weighted.gs <- calc.w.ex.im(icio.trade.ind.gs, tau.gs, list.reclass.gs)
tau.index.gs <- construct.index(tau.country.weighted.gs, c("g", "s"))

tau.country.weighted.low <- calc.w.ex.im(icio.trade.ind, tau.low, list.reclass)
tau.index.low <- construct.index(tau.country.weighted.low, c("g", "hts"))

tau.country.weighted.high <- calc.w.ex.im(icio.trade.ind, tau.high, list.reclass)
tau.index.high <- construct.index(tau.country.weighted.high, c("g", "hts"))

tau.country.weighted.alt <- calc.w.ex.im(icio.trade.ind.alt, tau.alt, list.reclass.alt)
tau.index.alt <- construct.index(tau.country.weighted.alt, c("g", "ps"))

tau.country.weighted.orig <- calc.w.ex.im(icio.trade.ind, tau.orig, list.reclass)
tau.index.orig <- construct.index(tau.country.weighted.orig, c("g", "hts"))   

write.csv(tau.index, "../../output/estimated_params/index.csv", row.names = FALSE)
write.csv(tau.index.hs, "../../output/estimated_params/index_hs.csv", row.names = FALSE)


write.csv(tau.index.alt, "../../output/estimated_params/index_alt.csv", row.names = FALSE)
write.csv(tau.index.gs, "../../output/estimated_params/index_gs.csv", row.names = FALSE)
write.csv(tau.index.low, "../../output/estimated_params/index_low.csv", row.names = FALSE)
write.csv(tau.index.high, "../../output/estimated_params/index_high.csv", row.names = FALSE)
write.csv(tau.index.orig, "../../output/estimated_params/index_orig.csv", row.names = FALSE)

## Relationship between income per capita and glre (log growth of export costs relative to import costs)

gdp <- read.csv("../../output/cleaned_data/gdp_per_capita.csv")
gdp <- subset(gdp, year == "X1995")[, c("country", "gdppc")]

## HERE make it into function ## DODO.
asym.dyn <- function(tau.index){
    plot.ind <- merge(tau.index, gdp, by = "country", all.x = TRUE)
    plot.ind <- plot.ind[, c("country", "glre.g", "glre.hts", "index", "gdppc")]
    colnames(plot.ind)[4] <- "glre.index"
    plot.ind <- pivot_longer(plot.ind, cols = c("glre.g", "glre.hts", "glre.index"), names_to = "ind")
    plot.ind$ind <- toupper(substr(plot.ind$ind, 6, nchar(plot.ind$ind)))
    plot.ind <- add.title(plot.ind)
    return(plot.ind)
}

plot.ind <- asym.dyn(tau.index)
plot.ind.low <- asym.dyn(tau.index.low)
plot.ind.high <- asym.dyn(tau.index.high)

asym.dyn.plot <- function(plot.ind) ggplot(data = subset(plot.ind, ind %in% c("G", "HTS")), aes(x = log(gdppc), y = value)) +
    geom_text(aes(label = country, color = ind)) +
    geom_smooth(method = "lm", alpha = 0.3, linewidth = 0.5, aes(color = ind)) +
    xlab("Log(GDP per capita) in 1995") +
    ylab("Log growth rate of (export trade costs)/(import trade costs)\n from 1995 to 2018") +
        scale_color_manual(values = default.color.base)

pp <- asym.dyn.plot(plot.ind)

pdf(file = "../../doc/figures/asymmetry_dynamics_paper.pdf", width = 10, height = 6)
pp + plot.scatter.options(bw = TRUE) +
    facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/asymmetry_dynamics_beamer.pdf", width = 10, height = 6)
pp + plot.scatter.options(bw = FALSE) +
    facet_wrap(~title)
dev.off()

pp.low <- asym.dyn.plot(plot.ind.low)

pdf(file = "../../doc/figures/asymmetry_dynamics_low_paper.pdf", width = 10, height = 6)
pp.low + plot.scatter.options(bw = TRUE) +
    facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/asymmetry_dynamics_low_beamer.pdf", width = 10, height = 6)
pp.low + plot.scatter.options(bw = FALSE) +
    facet_wrap(~title)
dev.off()

pp.high <- asym.dyn.plot(plot.ind.high)

pdf(file = "../../doc/figures/asymmetry_dynamics_high_paper.pdf", width = 10, height = 6)
pp.high + plot.scatter.options(bw = TRUE) +
    facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/asymmetry_dynamics_high_beamer.pdf", width = 10, height = 6)
pp.high + plot.scatter.options(bw = FALSE) +
    facet_wrap(~title)
dev.off()

clean.elas <- function(plot.ind.low, t.alp = "(a) Low Elasticity", t.orig = "Low Elasticity"){
    p.low <- plot.ind.low
    p.low <- subset(p.low, ind == "HTS")
    p.low$title.alp <- t.alp
    p.low$title <- t.orig
    return(p.low)
}

p.low <- clean.elas(plot.ind.low)
p.med <- clean.elas(plot.ind, "(b) Medium Elasticity", "Medium Elasticity")
p.high <- clean.elas(plot.ind.high, "(c) High Elasticity", "High Elasticity")

p.elas <- rbind(p.low, p.med, p.high)
p.elas$title <- factor(p.elas$title, levels = c("Low Elasticity", "Medium Elasticity", "High Elasticity"))

pp.elas <- ggplot(data = p.elas, aes(x = log(gdppc), y = value)) +
    geom_text(aes(label = country), color = "red") +
    geom_smooth(method = "lm", alpha = 0.3, linewidth = 0.5, aes(color = ind)) +
    xlab("Log(GDP per capita) in 1995") +
    ylab("Log growth rate of (export trade costs)/(import trade costs)\n from 1995 to 2018") +
    scale_color_manual(values = default.color.base) 

pdf(file = "../../doc/figures/asymmetry_dynamics_elasticity_paper.pdf", width = 10, height = 6)
pp.elas + plot.scatter.options(bw = TRUE) +
    facet_wrap(~title.alp)
dev.off()

pdf(file = "../../doc/figures/asymmetry_dynamics_elasticity_beamer.pdf", width = 10, height = 6)
pp.elas + plot.scatter.options(bw = FALSE) +
    facet_wrap(~title)
dev.off()
