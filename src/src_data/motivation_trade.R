## motivation_trade.R
## Trade-related movitation plots

## Solving package dependency with Renv
renv::load("../")

#############################
## Specialization patterns ##
#############################

## Packages
library("tidyverse")
library("directlabels")
library("cowplot")

source("../common_functions.R")

## Load data
load(file = "../../output/cleaned_data/trade_matrix.RData", verbose = TRUE)

list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.country <- list.country$code # Note: country is not ordered

list.reclass <- c("g", "bts", "hts")

## For a sector, get each country's gross output, total export, and total import. Also add world total.
get.export.import.go <- function(dat){
    res <- dat[ ,"country", drop = FALSE]
    res$dom <- unlist(lapply(1:nrow(dat), function(i) dat[i, dat$country[i]]))
    res$go <- rowSums(dat[, dat$country])
    res$abso <- colSums(dat[, dat$country])
    res$export <- res$go - res$dom
    res$import <- res$abso - res$dom
    res <- rbind(res, data.frame(country = "TOT", dom = sum(res$dom), go = sum(res$go), abso = sum(res$abso), export = sum(res$export), import = sum(res$import)))
    return(res)
}

go.export.import.ind <- lapply(icio.trade.ind, function(dat.ind) lapply(dat.ind, get.export.import.go))

go.export.import.ind <- do.call("rbind", lapply(list.reclass, function(i.ind){
    dat <- go.export.import.ind[[i.ind]]
    res <- do.call("rbind", lapply(1:length(dat), function(i.year){
        dat.inner <- dat[[i.year]]
        dat.inner$year <- i.year + 1994
        return(dat.inner)
    }))
    res$ind <- i.ind
    return(res)
}))

## First and last year, derive (sectoral export / total export)
## If you divide by (sectoral e/total e) of world aggregate then it's RCA.

res <- subset(go.export.import.ind, year %in% c(1995, 2018))
res.export <- res[, c("country", "export", "year", "ind")]
res.export <- pivot_wider(res.export, values_from = export, names_from = ind)

res.import <- res[, c("country", "import", "year", "ind")]
res.import <- pivot_wider(res.import, values_from = import, names_from = ind)

clean.res <- function(res){
    res$total <- rowSums(res[, list.reclass])
    res[, paste0(list.reclass, ".frac")] <- res[, list.reclass]/res$total
    res <- res[, c("country", "year", paste0(list.reclass, ".frac"))]

    res.g <- res[, c("country", "year", "g.frac")]
    res.g <- pivot_wider(res.g, values_from = "g.frac", names_from = "year")
    return(res.g)
}

res.g.export <- clean.res(res.export)
res.g.import <- clean.res(res.import)

res.g.export$type <- "Goods Fraction of Total Exports"
res.g.import$type <- "Goods Fraction of Total Imports"

res.g.export$type.alp <- "(a) Goods Fraction of Total Exports"
res.g.import$type.alp <- "(b) Goods Fraction of Total Imports"

res.g <- rbind(res.g.export, res.g.import)

plot.frac.g <- function(res.g, num.limits, num.breaks, bw = TRUE){
    xint <- as.numeric(res.g[res.g$country == "TOT", "1995", drop = TRUE][1])
    yint <- as.numeric(res.g[res.g$country == "TOT", "2018", drop = TRUE][1])
      
    pp <- ggplot(res.g, aes(x = `1995`, y = `2018`)) +
        geom_text(aes(label = country)) +
        geom_abline(intercept = 0, slope = 1) +
        geom_vline(xintercept = xint, linetype = "dashed", alpha = 0.3) +
        geom_hline(yintercept = yint, linetype = "dashed", alpha = 0.3) +
        xlab("Fraction of goods (1995)") + ylab("Fraction of goods (2018)") +
        plot.scatter.options(bw) +
        scale_x_continuous(limits = num.limits, breaks = num.breaks) +
        scale_y_continuous(limits = num.limits, breaks = num.breaks)

    if (bw){
        pp + facet_wrap(~type.alp)
    }
    else {
        pp + facet_wrap(~type)
    }
    
}

num.limits <- c(0.078,0.921)
num.breaks <- seq(0,1,0.2)

pdf(file = "../../doc/figures/goods_fraction_paper.pdf", width = 10, height = 6)
plot.frac.g(res.g, num.limits, num.breaks, bw = TRUE)
dev.off()

pdf(file = "../../doc/figures/goods_fraction_beamer.pdf", width = 10, height = 6)
plot.frac.g(res.g, num.limits, num.breaks, bw = FALSE)
dev.off()

write.csv(res.g, "../../doc/figures/goods_fraction.csv", row.names = FALSE)

## China and US case print to txt
res.g.chn.usa.export <- subset(res.g.export, country %in% c("CHN", "USA"))
res.g.chn.usa.import <- subset(res.g.import, country %in% c("CHN", "USA"))
res.g.chn.usa.export$type <- "export"
res.g.chn.usa.import$type <- "import"
res.g.chn.usa <- rbind(res.g.chn.usa.export, res.g.chn.usa.import)
res.g.chn.usa <- res.g.chn.usa[, c("country", "1995", "2018", "type")]
write.csv(res.g.chn.usa, "../../doc/nums/goods_fraction_chn_usa.csv", row.names = FALSE)

1

## Share of trade (export) by ROW (I mention this in the appendix)

export.2018 <- subset(res.export, year == 2018)
export.2018$total <- rowSums(export.2018[, c("g", "bts", "hts")])
writeLines(paste("ROW share of the world total exports is", export.2018$total[export.2018$country == "ROW"]/export.2018$total[export.2018$country == "TOT"]), "../../doc/nums/row_share_exports.txt")

#######################
## Trade / GDP trend ##
#######################

res <- subset(go.export.import.ind, country == "TOT")[, c("year", "ind", "export")]
res <- pivot_wider(res, values_from = "export", names_from = "ind")
res$tot <- rowSums(res[, list.reclass])
res <- pivot_longer(res, cols = all_of(c(list.reclass, "tot")), names_to = "ind", values_to = "trade")

gdp <- read.csv("../../output/cleaned_data/gdp_per_capita.csv")

gdp <- gdp %>%
    group_by(year) %>%
    summarize(gdp = sum(gdp))

gdp$year <- as.numeric(substr(gdp$year, 2,5))

res <- merge(res, gdp, by = "year", all.x = TRUE)

res$trade.share <- res$trade/res$gdp

write.csv(res, "../../doc/tables/trade_share_gdp.csv", row.names = FALSE)

tr.sh.1995.g <- res$trade.share[res$year == 1995 & res$ind == "g"]
tr.sh.1995.s <- res$trade.share[res$year == 1995 & res$ind == "hts"] + res$trade.share[res$year == 1995 & res$ind == "bts"]

tr.sh.2018.g <- res$trade.share[res$year == 2018 & res$ind == "g"]
tr.sh.2018.s <- res$trade.share[res$year == 2018 & res$ind == "hts"] + res$trade.share[res$year == 2018 & res$ind == "bts"]

writeLines(paste0("Goods and services trade as a share of GDP in 1995 were ", tr.sh.1995.g, " and ", tr.sh.1995.s, ", respectively. For 2018, the numbers were ", tr.sh.2018.g, " and ", tr.sh.2018.s, "."), "../../doc/nums/trade_gdp_share_1995_2018.txt")

##################################
## Tradedness by year by sector ##
##################################

go.tr.ind <- subset(go.export.import.ind, country == "TOT")
go.tr.ind$tr <- go.tr.ind$export/go.tr.ind$go
go.tr.ind <- go.tr.ind[, c("year", "ind", "go", "export", "tr")]
go.tr.ind$ind <- toupper(go.tr.ind$ind)
go.tr.ind <- add.title(go.tr.ind)
go.tr.ind$log.export <- log(go.tr.ind$export)

plot.inner <- function(dat, y.var, plot.title, y.lab, bw.2 = TRUE){
    pp <- ggplot(dat, aes(x = year, y = !!sym(y.var), color = ind, linetype = ind)) +
        geom_line() +
        geom_point(aes(shape = ind), size = 2) +     
        scale_color_manual(values = default.color.base) +
        scale_shape_manual(values = default.shape.base) +
        scale_linetype_manual(values = default.linetype.base) +
        ylab(y.lab) +
        geom_dl(aes(label = ind), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
        ggtitle(plot.title) +
        append(plot.time.options(x.limits = c(1995, 2021), bw = bw.2), list(theme(plot.title = element_text(hjust = 0.5, size = 15))))
    return(pp)
}

p1 <- plot.inner(go.tr.ind, "tr", "(a) Total exports / total gross output", "Total exports / total gross output")

p2 <- plot.inner(go.tr.ind, "log.export", "(b) Log(total exports)", "Log(total exports in million current USDs)")

pdf(file = "../../doc/figures/trade_pattern.pdf", width = 10, height = 6)
plot_grid(p1,p2, nrow = 1)
dev.off()

p1 <- plot.inner(go.tr.ind, "tr", "Total exports / total gross output", "Total exports / total gross output", bw.2 = FALSE)

p2 <- plot.inner(go.tr.ind, "log.export", "Log(total exports)", "Log(total exports in million current USDs)", bw.2 = FALSE)

pdf(file = "../../doc/figures/trade_pattern_beamer.pdf", width = 10, height = 6)
plot_grid(p1,p2, nrow = 1)
dev.off()

write.csv(go.tr.ind, "../../doc/tables/trade_share_sectoral_gross_output.csv", row.names = FALSE)

trd.sector.2018 <- c(go.tr.ind$tr[go.tr.ind$year == 2018 & go.tr.ind$ind == "G"],
                     go.tr.ind$tr[go.tr.ind$year == 2018 & go.tr.ind$ind == "HTS"],
                     go.tr.ind$tr[go.tr.ind$year == 2018 & go.tr.ind$ind == "BTS"])

writeLines(paste0("In 2018, tradedness for G, HTS, and BTS were as follows:", paste(trd.sector.2018, collapse = ", "), "."), "../../doc/nums/tradedness_sector_2018.txt")

## Services share of trade in 2018
dat.18 <- subset(go.tr.ind, year == 2018)[, c("ind", "export")]
num.ser.share <- sum(dat.18$export[dat.18$ind %in% c("HTS", "BTS")])/sum(dat.18$export)
writeLines(paste0("Services share of world total exports is ", num.ser.share, "."), "../../doc/nums/service_share_export.txt")



