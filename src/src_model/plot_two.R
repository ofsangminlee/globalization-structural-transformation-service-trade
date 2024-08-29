## plot_two.R
## Plot result for two country model (USA-ROW)

## Package dependency with renv
renv::load("../")

## Load packages
library("tidyverse")
library("ggrepel")
library("directlabels")
library("ggh4x")

no.s <- read.csv("../../output/model/usa_result_no_s.csv")
base <- read.csv("../../output/model/usa_result_tr.csv")
dat <- read.csv("../../output/cleaned_data/usa_va_share.csv")

clean.result <- function(dat, i.type){
    dat <- dat[, c("year", "country", paste0("va.", c("g", "bts", "hts")))]
    dat$type <- i.type
    dat <- pivot_longer(dat, cols = paste0("va.", c("g", "bts", "hts")), names_to = "ind", values_to = "share")
    dat$ind <- substr(dat$ind, 4, nchar(dat$ind))
    return(dat)
}

dat <- clean.result(dat, "Data")

dat$year <- as.numeric(substr(dat$year,2,5))

base <- clean.result(base, "T")
no.s <- clean.result(no.s, "NT")

res <- rbind(dat, base, no.s)

res.plot <- subset(res, country == "USA")
res.plot$ind <- ifelse(res.plot$ind == "g", "(a) Goods", ifelse(res.plot$ind == "hts", "(b) Highly Tradable Services", "(c) Barely Tradable Services"))
res.plot$ind <- factor(res.plot$ind, levels = c("(a) Goods", "(b) Highly Tradable Services", "(c) Barely Tradable Services"))

res.plot$ind.2 <- substr(as.character(res.plot$ind), 5, nchar(as.character(res.plot$ind)))
res.plot$ind.2 <- factor(res.plot$ind.2, levels = c("Goods", "Highly Tradable Services", "Barely Tradable Services"))

pp <- ggplot(data = res.plot, aes(x = year, y = share, by = type, color = ind.2)) +
    geom_point(aes(shape = type), size = 2) +
    geom_line(aes(linetype = type), linewidth = 1) +
    geom_dl(aes(label = type), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
    scale_x_continuous(limits = c(1995, 2022), breaks = seq(1995, 2015, 5)) +
    scale_color_manual(values = c("Goods" = "black", "Highly Tradable Services" = "red", "Barely Tradable Services" = "blue")) + 
    labs(x = "Year", y = "Share of GDP") +
    theme(legend.position = "none", text = element_text(size = 15)) +
    facet_wrap(~ind.2, scales = "free")

pp.paper <- ggplot(data = res.plot, aes(x = year, y = share, by = type, color = ind)) +
    geom_point(aes(shape = type), size = 2) +
    geom_line(aes(linetype = type), linewidth = 1) +
    geom_dl(aes(label = type), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
    scale_x_continuous(limits = c(1995, 2022), breaks = seq(1995, 2015, 5)) +
    scale_color_manual(values = c("(a) Goods" = "black", "(b) Highly Tradable Services" = "red", "(c) Barely Tradable Services" = "blue")) + 
    labs(x = "Year", y = "Share of GDP") +
    facet_wrap(~ind, scales = "free") +
    theme_bw() +
    theme(legend.position = "none", strip.text = element_text(size = 13), text = element_text(size = 13), axis.text = element_text(size = 9), strip.background = element_blank())

## summary(res.plot$share[res.plot$ind == "Goods"])
## summary(res.plot$share[res.plot$ind == "Highly Tradable services"])
## summary(res.plot$share[res.plot$ind == "Barely Tradable services"])



pdf("../../doc/figures/usa_horserace_beamer.pdf", width = 10, height = 6)
pp +
    facetted_pos_scales(
        y = list(ind.2 == "Goods" ~ scale_y_continuous(limits = c(0.15, 0.23), breaks = seq(0.15, 0.23, 0.01)),
                 ind.2 == "Highly Tradable Services" ~ scale_y_continuous(limits = c(0.38, 0.44), breaks = seq(0.38, 0.44, 0.01)),
                 ind.2 == "Barely Tradable Services" ~ scale_y_continuous(limits = c(0.37, 0.42), breaks = seq(0.37, 0.42, 0.01))
                 )
    )
dev.off()


pdf("../../doc/figures/usa_horserace_paper.pdf", width = 10, height = 6)
pp.paper +
    facetted_pos_scales(
        y = list(ind == "(a) Goods" ~ scale_y_continuous(limits = c(0.15, 0.23), breaks = seq(0.15, 0.23, 0.01)),
                 ind == "(b) Highly Tradable Services" ~ scale_y_continuous(limits = c(0.38, 0.44), breaks = seq(0.38, 0.44, 0.01)),
                 ind == "(c) Barely Tradable Services" ~ scale_y_continuous(limits = c(0.37, 0.42), breaks = seq(0.37, 0.42, 0.01))
                 )
    )
dev.off()
