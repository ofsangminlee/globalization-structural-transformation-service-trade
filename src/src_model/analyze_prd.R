## analyze_prd.R
## Trends for TFP productivity, labor productivity, and relative prices

## Package dependency with renv
renv::load("../")

## Load packages and functions
library("tidyverse")
library("directlabels")
source("../common_functions.R")


## The figures are smaller in the paper, so I put larger font sizes here.
plot.time.options.big <- function(x.limits = c(1995, 2018), bw = TRUE, ...){
    res <- list(xlab("Year"),
                scale_x_continuous(limits = x.limits, breaks = seq(1995, 2018, 5)),
                theme(legend.position = "none", axis.text = element_text(size = 15), text = element_text(size = 19), strip.text = element_text(size =20), panel.grid.minor.x = element_blank())# If I don't set sizes for  axis.text separately, due to the incompleteness of the theme, 15*0.8 (default rel size) will be the default size for axis.text.
                )
    if (bw){
        res <- append(list(theme_bw(), theme(strip.background = element_blank())), res)
        return(res)
    }
    else {
        return(res)
    }
}

## TFP
a <- read.csv("../../output/estimated_params/prd.csv")
a$year <- as.numeric(substr(a$year, 2, 5))

## Growth factor of tfp
g.a <- merge(a, subset(a, year == 1995)[, c("country", "ind", "prd")], by = c("country", "ind"))
g.a$value <- g.a$prd.x/g.a$prd.y

a <- pivot_wider(a, names_from = "ind", values_from = "prd")

clean.a <- function(a){
    a$g <- a$g/median(a$g[a$year == 1995])
    a$hts <- a$hts/median(a$hts[a$year == 1995])
    a$bts <- a$bts/median(a$bts[a$year == 1995])
    
    a  <- a %>%
        group_by(year) %>%
        reframe(G = quantile(g, c(0.25, 0.5, 0.75)), BTS = quantile(bts, c(0.25, 0.5, 0.75)), HTS = quantile(hts, c(0.25, 0.5, 0.75)), type = c("Q1", "Q2", "Q3"))
    
    a <- pivot_longer(a, cols = c("G", "BTS", "HTS"), names_to = "ind")

    a <- add.title(a)
    
    return(a)
}

c.a <- clean.a(a)

shorten.title <- function(x){ ## Abbreviated title for paper
    x <- ifelse(x == "(ii) Highly Tradable Services", "(ii) HTS", ifelse(x == "(iii) Barely Tradable Services", "(iii) BTS", "(i) Goods"))
    x <- factor(x, levels = c("(i) Goods", "(ii) HTS", "(iii) BTS"))
    return(x)
}

c.a.paper <- c.a
c.a.paper$title.rom <- shorten.title(c.a.paper$title.rom)

clean.ga <- function(g.a){
    g.a$prd.x <- g.a$prd.y <- NULL
    g.a <- pivot_wider(g.a, names_from = "ind", values_from = "value")

    g.a <- g.a %>%
        group_by(year) %>%
        reframe(G = quantile(g, c(0.25, 0.5, 0.75)), BTS = quantile(bts, c(0.25, 0.5, 0.75)), HTS = quantile(hts, c(0.25, 0.5, 0.75)), type = c("Q1", "Q2", "Q3"))

    g.a <- pivot_longer(g.a, cols = c("G", "BTS", "HTS"), names_to = "ind")
    
    g.a <- add.title(g.a)
    return(g.a)
}

c.g.a <- clean.ga(g.a)
c.g.a.paper <- c.g.a
c.g.a.paper$title.rom <- shorten.title(c.g.a.paper$title.rom)

## Labor productivity

la <- read.csv("../../output/model/lprd_base.csv")
la$type <- NULL
colnames(la)[3:5] <- substr(colnames(la)[3:5], 6, nchar(colnames(la)[3:5]))

g.la <- la
g.la <- pivot_longer(g.la, cols = c("g", "bts", "hts"), names_to = "ind", values_to = "prd")

g.la <- merge(g.la, subset(g.la, year == 1995)[, c("country", "ind", "prd")], by = c("country", "ind"))
g.la$value <- g.la$prd.x/g.la$prd.y

c.g.la <- clean.ga(g.la)
c.g.la.paper <- c.g.la
c.g.la.paper$title.rom <- shorten.title(c.g.la.paper$title.rom)

c.la <- clean.a(la)
c.la.paper <- c.la
c.la.paper$title.rom <- shorten.title(c.la.paper$title.rom)

## Plotting

plot.prd <- function(a, bw.2 = TRUE, y.lab = "Quartiles", type.title = "title")
    ggplot(data = a, aes(x = year, y = value, by = type)) +
        geom_point(aes(shape = type, color = ind), size = 2) +
        geom_line(aes(linetype = type, color = ind)) +
        scale_color_manual(values = default.color.base) +
        facet_wrap(as.formula(paste0("~", type.title)))  + 
        geom_dl(aes(label = type, color = ind), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.5)) +
        labs(y = y.lab, x = "Year") +
        plot.time.options.big(x.limits = c(1995,2022), bw = bw.2)

plot.prd.beamer <- function(a, bw.2 = TRUE, y.lab = "Quartiles", type.title = "title")
    ggplot(data = a, aes(x = year, y = value, by = type)) +
        geom_point(aes(shape = type, color = ind), size = 1.5) +
        geom_line(aes(linetype = type, color = ind)) +
        scale_color_manual(values = default.color.base) +
        facet_wrap(as.formula(paste0("~", type.title)))  + 
        geom_dl(aes(label = type, color = ind), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
        labs(y = y.lab, x = "Year") +
        plot.time.options(x.limits = c(1995,2020), bw = bw.2)



pdf(file = "../../doc/figures/tfp_trends_paper.pdf", width = 10, height = 6)
plot.prd(c.a.paper, y.lab = "Quartiles (TFP)", type.title = "title.rom")
dev.off()

pdf(file = "../../doc/figures/lp_trends_paper.pdf", width = 10, height = 6)
plot.prd(c.la.paper, y.lab = "Quartiles (LP)", type.title = "title.rom")
dev.off()

pdf(file = "../../doc/figures/tfp_trends_beamer.pdf", width = 10, height = 6)
plot.prd.beamer(c.a, bw.2 = FALSE)
dev.off()

pdf(file = "../../doc/figures/lp_trends_beamer.pdf", width = 10, height = 6)
plot.prd.beamer(c.la, bw.2 = FALSE)
dev.off()
    
pdf(file = "../../doc/figures/tfp_gr_trends_paper.pdf", width = 10, height = 6)
plot.prd(c.g.a.paper, y.lab = "Quartiles (growth factor of TFP)", type.title = "title.rom")
dev.off()

pdf(file = "../../doc/figures/lp_gr_trends_paper.pdf", width = 10, height = 6)
plot.prd(c.g.la.paper, y.lab = "Quartiles (growth factor of LP)", type.title = "title.rom")
dev.off()

pdf(file = "../../doc/figures/tfp_gr_trends_beamer.pdf", width = 10, height = 6)
plot.prd.beamer(c.g.a, bw.2 = FALSE, y.lab = "Quartiles (growth factor of TFP)")
dev.off()

pdf(file = "../../doc/figures/lp_gr_trends_beamer.pdf", width = 10, height = 6)
plot.prd.beamer(c.g.la, bw.2 = FALSE, y.lab = "Quartiles (growth factor of LP)")
dev.off()

## PRICES ##
## Load price data
gopl <- read.csv("../../output/cleaned_data/gopl.csv")
gopl <- gopl[, c("country", "ind", "year", "pl")]
gopl$year <- as.integer(substr(gopl$year, 2,5))
gopl <- pivot_wider(gopl, values_from = "pl", names_from = "ind", names_prefix = "p.")
gopl$relp.hts <- gopl$p.hts/gopl$p.g
gopl$relp.bts <- gopl$p.bts/gopl$p.g
relp <- gopl[, c("country", "year", "relp.hts", "relp.bts")]

sum.relp  <- relp %>%
    group_by(year) %>%
    reframe(relp.hts = quantile(relp.hts, c(0.25, 0.5, 0.75)), relp.bts = quantile(relp.bts, c(0.25, 0.5, 0.75)), type = c("Q1", "Q2", "Q3"))

relp.2 <- pivot_longer(relp, cols = c("relp.hts", "relp.bts"))
relp.2$ind <- relp.2$name
relp.2$name <- NULL
relp.2$ind <- substr(relp.2$ind, 6,8)
relp.2 <- merge(relp.2, subset(relp.2, year == 1995)[, c("country", "ind", "value")], by = c("country", "ind"), all.x = TRUE, suffixes = c("", ".init"))
relp.2$g.relp <- relp.2$value/relp.2$value.init
g.relp <- relp.2[, c("country", "year", "ind", "g.relp")]
g.relp <- pivot_wider(g.relp, names_from = "ind", values_from = "g.relp")

sum.g.relp <- g.relp %>%
    group_by(year) %>%
    reframe(HTS = quantile(hts, c(0.25, 0.5, 0.75)), BTS = quantile(bts, c(0.25, 0.5, 0.75)), type = c("Q1", "Q2", "Q3"))

sum.g.relp <- pivot_longer(sum.g.relp, cols = c("HTS", "BTS"), names_to = "ind", values_to = "value")

two.sec.title <- data.frame(ind = c("HTS", "BTS"), title = c("Highly Tradable Services", "Barely Tradable Services"), title.rom = c("(i) HTS", "(ii) BTS"))

sum.g.relp <- merge(sum.g.relp, two.sec.title, by = "ind", all.x = TRUE)

sum.g.relp$title <- factor(sum.g.relp$title, levels = c("Highly Tradable Services", "Barely Tradable Services"))

plot.relp <- function(sum.relp, bw.2 = TRUE, y.lab = "Quartiles", type.title = "title")
    ggplot(data = sum.relp, aes(x = year, y = value, by = type)) +
        geom_point(aes(shape = type, color = ind), size = 2) +
        geom_line(aes(linetype = type, color = ind)) +
        scale_color_manual(values = default.color.base) +
        facet_wrap(as.formula(paste0("~", type.title)), scale = "free") + 
        geom_dl(aes(label = type, color = ind), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.5)) +
        labs(y = y.lab, x = "Year") +
        plot.time.options.big(x.limits = c(1995,2020), bw = bw.2)

pdf(file = "../../doc/figures/price_gr_trends.pdf", width = 10, height = 6)
plot.relp(sum.g.relp, y.lab = "Quartiles (growth factor of relative prices)", type.title = "title.rom")
dev.off()

plot.relp.beamer <- function(sum.relp, bw.2 = TRUE, y.lab = "Quartiles", type.title = "title")
    ggplot(data = sum.relp, aes(x = year, y = value, by = type)) +
        geom_point(aes(shape = type, color = ind), size = 1.5) +
        geom_line(aes(linetype = type, color = ind)) +
        scale_color_manual(values = default.color.base) +
        facet_wrap(as.formula(paste0("~", type.title)), scale = "free") + 
        geom_dl(aes(label = type, color = ind), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.2)) +
        labs(y = y.lab, x = "Year") +
        plot.time.options(x.limits = c(1995,2020), bw = bw.2)

pdf(file = "../../doc/figures/price_gr_trends_beamer.pdf", width = 10, height = 6)
plot.relp.beamer(sum.g.relp, y.lab = "Quartiles (growth factor of relative prices)", bw.2 = FALSE)
dev.off()

## Count the countries
g.relp.18 <- subset(g.relp, year == 2018 & country != "ROW")

sum(g.relp.18$bts < 1)
sum(g.relp.18$hts < 1)

g.rela.18 <- g.la[g.la$year == 2018, c("country", "ind", "value")]
g.rela.18 <- pivot_wider(g.rela.18, values_from = "value", names_from = "ind")

sum(g.rela.18$bts > g.rela.18$g)
sum(g.rela.18$hts > g.rela.18$g)

writeLines(paste0("The number of countries, where prices of BTS and HTS relative to G fell is ", sum(g.relp.18$bts < 1), " and ", sum(g.relp.18$hts < 1), ". ",
                 "The number of countries, where labor productivity was faster in BTS and HTS compared to G, is ", sum(g.rela.18$bts > g.rela.18$g), " and ", sum(g.rela.18$hts > g.rela.18$g), "."), "../../doc/nums/productivity_counts.txt")
