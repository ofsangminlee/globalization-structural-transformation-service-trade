## Motivation_str_tran.R
## Plots for structural transformation (both expenditure-side and production-side)

## Solving package dependency with Renv
renv::load("../")

## Load packages/cleaned data
library("tidyverse")
library("directlabels")
source("../common_functions.R")

dat.cons <- read.csv("../../output/cleaned_data/cons.csv")
dat.prod <- read.csv("../../output/cleaned_data/prod.csv")

pwt <- read.csv("../../output/cleaned_data/pwt_variables.csv")

list.reclass<- c("g", "bts", "hts")


########################################
## Expenditure-side structural change ##
########################################

dat.cons <- merge(dat.cons[, c("country", "year", paste0("share.", list.reclass))], pwt[, c("country", "year" ,"rgdpe.pc")], by = c("country", "year"), all.x = TRUE)

dat.cons <- subset(dat.cons, !country %in% c("ROW", "TOT"))

dat.cons <- pivot_longer(dat.cons, cols = all_of(pasted("share", list.reclass)), names_to = "ind")

clean.ind.name <- function(dat.cons){
    dat.cons$ind <- substr(dat.cons$ind, 7, nchar(dat.cons$ind))

    dat.cons$ind <- toupper(dat.cons$ind)

    dat.cons <- add.title(dat.cons)

    return(dat.cons)
}

dat.cons <- clean.ind.name(dat.cons)

str.plot <- function(dat, lab.y, default.color.base, default.shape.base, bw2 = TRUE)
    ggplot(data = dat, aes(x = log(rgdpe.pc), y = value)) +
        geom_point(aes(color = ind, shape = ind), alpha = 0.5) +
        geom_smooth(method = "lm", aes(color = ind)) +
        scale_color_manual(values = default.color.base) +
        scale_shape_manual(values = default.shape.base) +
        labs(x = "Log real GDP per capita (PPP, 2017US$)", y = lab.y) +
        plot.scatter.options(bw = bw2)

pdf(file = "../../doc/figures/str_tran_expenditure_beamer.pdf", width = 10, height = 6)
str.plot(dat.cons, "Share of final expenditure", default.color.base, default.shape.base, bw2 = FALSE) + facet_wrap(~title)
dev.off()

pdf(file = "../../doc/figures/str_tran_expenditure_paper.pdf", width = 10, height = 6)
str.plot(dat.cons, "Share of final expenditure", default.color.base, default.shape.base) + facet_wrap(~title.alp)
dev.off()

## Country names
add.name <- function(dat.prod){
    dat.prod.name <- dat.prod
    dat.prod.name$country.year <- paste0(dat.prod.name$country, "_", substr(dat.prod.name$year, 2,5))
    dat.prod.name$country.year <- ifelse(substr(dat.prod.name$country.year,5,8) %in% c("1995", "2018"), dat.prod.name$country.year, "")
    dat.prod.name$country.year <- ifelse(substr(dat.prod.name$country.year,5,8) == "1995", paste0(substr(dat.prod.name$country.year,1,4), "95"),
                                  ifelse(substr(dat.prod.name$country.year,5,8) == "2018", paste0(substr(dat.prod.name$country.year,1,4), "18"), ""))
    return(dat.prod.name)
}

dat.cons.name <- add.name(dat.cons)

str.plot.name <- function(dat, lab.y, default.color.base, default.shape.base, bw2 = TRUE)
    ggplot(data = dat, aes(x = log(rgdpe.pc), y = value)) +
        geom_text(aes(color = ind, label = country.year), alpha = 0.5) +
        geom_smooth(method = "lm", aes(color = ind)) +
        scale_color_manual(values = default.color.base) +
        scale_shape_manual(values = default.shape.base) +
        labs(x = "Log real GDP per capita (PPP, 2017US$)", y = lab.y) +
        plot.scatter.options(bw = bw2)

pdf(file = "../../doc/figures/str_tran_expenditure_country_name_beamer.pdf", width = 10, height = 6)
str.plot.name(dat.cons.name, "Share of final expenditure", default.color.base, default.shape.base, bw2 = FALSE) + facet_wrap(~title)
dev.off()

pdf(file = "../../doc/figures/str_tran_expenditure_country_name_paper.pdf", width = 10, height = 6)
str.plot.name(dat.cons.name, "Share of final expenditure", default.color.base, default.shape.base) + facet_wrap(~title.alp)
dev.off()

## Plot in terms of differences
add.diff <- function(dat.cons){
    dat.cons.diff <- dat.cons[, c("country", "year", "rgdpe.pc", "ind", "value")]
    colnames(dat.cons.diff) <- c("country", "year", "gdppc", "ind", "share")
    dat.cons.diff <- subset(dat.cons.diff, year %in% c("X1995", "X2018"))
    dat.cons.diff <- pivot_wider(dat.cons.diff, values_from = c("gdppc", "share"), names_from = "year")
    dat.cons.diff$lg.gdppc <- log(dat.cons.diff$gdppc_X2018) - log(dat.cons.diff$gdppc_X1995)
    dat.cons.diff$lg.share <- dat.cons.diff$share_X2018 - dat.cons.diff$share_X1995
    dat.cons.diff <- add.title(dat.cons.diff)
    return(dat.cons.diff)
}

str.plot.diff <- function(dat, lab.y, default.color.base, default.shape.base, bw2 = TRUE)
    ggplot(data = dat, aes(x = lg.gdppc, y = lg.share)) +
        geom_text(aes(color = ind, label = country), alpha = 1) +
        scale_color_manual(values = default.color.base) +
        scale_shape_manual(values = default.shape.base) +
        labs(x = "Log growth rate of real GDP per capita (PPP, 2017US$) from 1995 to 2018", y = lab.y) +
        plot.scatter.options(bw = bw2) +
        geom_hline(yintercept = 0, linetype = "dotted", alpha = 1)

dat.cons.diff <- add.diff(dat.cons)

pdf(file = "../../doc/figures/str_tran_expenditure_country_diff_beamer.pdf", width = 10, height = 6)
str.plot.diff(dat.cons.diff, "Change in the final expenditure shares from 1995 to 2018", default.color.base, default.shape.base, bw2 = FALSE) + facet_wrap(~title)
dev.off()

pdf(file = "../../doc/figures/str_tran_expenditure_country_diff_paper.pdf", width = 10, height = 6)
str.plot.diff(dat.cons.diff, "Change in the final expenditure shares from 1995 to 2018", default.color.base, default.shape.base) + facet_wrap(~title.rom)
dev.off()

above.zero <- function(dat.cons.diff, i.ind)
    sum(dat.cons.diff$lg.share[dat.cons.diff$ind == i.ind] > 0)

above.zero(dat.cons.diff, "G")
above.zero(dat.cons.diff, "HTS")
above.zero(dat.cons.diff, "BTS")

all.correct <- function(dat.cons.diff)
    sum(dat.cons.diff$lg.share[dat.cons.diff$ind == "G"] < 0 &
        dat.cons.diff$lg.share[dat.cons.diff$ind == "HTS"] > 0 &
        dat.cons.diff$lg.share[dat.cons.diff$ind == "BTS"] > 0)

all.correct(dat.cons.diff)

all.correct.2 <- function(dat.cons.diff)
    sum(dat.cons.diff$lg.share[dat.cons.diff$ind == "G"] < 0 &
        dat.cons.diff$lg.share[dat.cons.diff$ind == "HTS"] > 0)

all.correct.2(dat.cons.diff)

## dat.cons.reg <- dat.cons.diff[, c("country", "ind", "lg.gdppc", "lg.share")]
## summary(lm(lg.share ~ lg.gdppc, data = subset(dat.cons.reg, ind == "G")))
## summary(lm(lg.share ~ lg.gdppc, data = subset(dat.cons.reg, ind == "PS")))
## summary(lm(lg.share ~ lg.gdppc, data = subset(dat.cons.reg, ind == "CS")))


#######################################
## Production-side structural change ##
#######################################

dat.prod <- dat.prod[, c("country", "year", "ind", "v.level")]

dat.prod <- subset(dat.prod, !country %in% c("ROW", "TOT"))

dat.prod <- pivot_wider(dat.prod, names_from = "ind", values_from = "v.level")

dat.prod$tot <- rowSums(dat.prod[, list.reclass])

list.share.reclass <- paste0("share.", list.reclass)

dat.prod[, list.share.reclass] <- dat.prod[, list.reclass]/dat.prod$tot

dat.prod <- dat.prod[, c("country", "year", list.share.reclass)]

dat.prod <- pivot_longer(dat.prod, cols = all_of(list.share.reclass), names_to = "ind")

dat.prod <- clean.ind.name(dat.prod)

dat.prod <- merge(dat.prod, pwt[, c("country", "year" ,"rgdpe.pc")], by = c("country", "year"), all.x = TRUE)

pdf(file = "../../doc/figures/str_tran_prd_beamer.pdf", width = 10, height = 6)

str.plot(dat.prod, "Share of GDP", default.color.base, default.shape.base, bw2 = FALSE) + facet_wrap(~title)

dev.off()

pdf(file = "../../doc/figures/str_tran_prd_paper.pdf", width = 10, height = 6)
str.plot(dat.prod, "Share of GDP", default.color.base, default.shape.base) + facet_wrap(~title.alp)
dev.off()

## Plot for first page
pdf(file = "../../doc/figures/str_tran_prd_beamer_first.pdf", width = 10, height = 5.5)
str.plot(dat.prod, "Share of GDP", default.color.base, default.shape.base, bw2 = FALSE) + facet_wrap(~title) + labs(caption = "Note: 66 countries from 1995 to 2018\nSource: OECD ICIO (2021) and Penn World Table 10.0") + theme(plot.caption = element_text(hjust = 0))
dev.off()

## Country names
dat.prod.name <- add.name(dat.prod)

pdf(file = "../../doc/figures/str_tran_prd_country_name_beamer.pdf", width = 10, height = 6)
str.plot.name(dat.prod.name, "Share of GDP", default.color.base, default.shape.base, bw2 = FALSE) + facet_wrap(~title)
dev.off()

pdf(file = "../../doc/figures/str_tran_prd_country_name_paper.pdf", width = 10, height = 6)
str.plot.name(dat.prod.name, "Share of GDP", default.color.base, default.shape.base) + facet_wrap(~title.alp)
dev.off()

## Differences

dat.prod.diff <- add.diff(dat.prod)

pdf(file = "../../doc/figures/str_tran_prd_country_diff_beamer.pdf", width = 10, height = 6)
str.plot.diff(dat.prod.diff, "Change in the GDP shares from 1995 to 2018", default.color.base, default.shape.base, bw2 = FALSE) + facet_wrap(~title)
dev.off()

pdf(file = "../../doc/figures/str_tran_prd_country_diff_paper.pdf", width = 10, height = 6)
str.plot.diff(dat.prod.diff, "Change in the GDP shares from 1995 to 2018", default.color.base, default.shape.base) + facet_wrap(~title.alp)
dev.off()

above.zero(dat.prod.diff, "G")
above.zero(dat.prod.diff, "HTS")
above.zero(dat.prod.diff, "BTS")

all.correct(dat.prod.diff)

writeLines(paste("For consumption, numbers of exceptions are", above.zero(dat.cons.diff, "G"), "for G,", 66 - above.zero(dat.cons.diff, "HTS"), "for HTS,", 66 - above.zero(dat.cons.diff, "BTS"), "for BTS."), "../../doc/nums/str_chg_expenditure_change_exception.txt")

writeLines(paste("For production, numbers of exceptions are", above.zero(dat.prod.diff, "G"), "for G,", 66 - above.zero(dat.prod.diff, "HTS"), "for HTS,", 66 - above.zero(dat.prod.diff, "BTS"), "for BTS."), "../../doc/nums/str_chg_prd_change_exception.txt")

## dat.prod.reg <- dat.prod.diff[, c("country", "ind", "lg.gdppc", "lg.share")]
## summary(lm(lg.share ~ lg.gdppc, data = subset(dat.prod.reg, ind == "G")))
## summary(lm(lg.share ~ lg.gdppc, data = subset(dat.prod.reg, ind == "PS")))
## summary(lm(lg.share ~ lg.gdppc, data = subset(dat.prod.reg, ind == "CS")))

## Services share of GDP (number I mention on the introduction)
dat.18 <- read.csv("../../output/cleaned_data/prod.csv")[, c("country", "year", "ind", "v.level")] # It doesn't have TOT
dat.18 <- subset(dat.18, year == "X2018")
dat.18 <- dat.18 %>%
    group_by(ind) %>%
    summarize(tot.va = sum(v.level))

writeLines(paste0("Services share of world total GDP is ", sum(dat.18$tot.va[dat.18$ind %in% c("hts", "bts")])/sum(dat.18$tot.va), "."), "../../doc/nums/service_share_gdp.txt")
