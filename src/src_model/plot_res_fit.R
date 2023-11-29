## plot_res_fit.R
## Plot the main results and plot the fit of the model

## Package dependency with renv
renv::load("../")

## Loading required libraries and functions
library("tidyverse")
library("latex2exp")
library("grid")
library("gridExtra")
library("cowplot")
library("xtable")
source("../common_functions.R")

## library("ggrepel")
## library("directlabels")

## Misc.

list.inds <- c("g", "cs", "ps")
list.levels <- c("CS", "PS", "G") # for plotting, this is the order that I want.

list.inds.alt <- c("g", "bts", "hts")
list.levels.alt <- c("G", "HTS", "BTS")

list.inds.gs <- c("g", "s")
list.levels.gs <- c("G", "S")

## Load the results

clean.res <- function(loc.file, list.inds, list.levels){
    res <- read.csv(file = loc.file)
    res <- res[, c("year", "country", "type", paste0("va.", list.inds))]
    res <- pivot_longer(res, cols = paste0("va.", list.inds), names_to = "ind", values_to = "share")
    res$ind <- toupper(substr(res$ind, 4, nchar(res$ind)))
    res$ind <- factor(res$ind, levels = list.levels)
    return(res)
}

## Baseline
res <- clean.res("../../output/model/str_chg.csv", list.inds, list.levels)

## Head and Ries trade costs
res.hs <- clean.res("../../output/model/str_chg_hs.csv", list.inds, list.levels)

## Model without services trade
res.no.s.trade <- clean.res("../../output/model/str_chg_no_s_trade.csv", list.inds, list.levels)

## All the other cases
res.alt <- clean.res("../../output/model/str_chg_alt.csv", list.inds.alt, list.levels.alt)
res.gs <- clean.res("../../output/model/str_chg_gs.csv", list.inds.gs, list.levels.gs)
res.cs <- clean.res("../../output/model/str_chg_cs.csv", list.inds, list.levels)
res.high <- clean.res("../../output/model/str_chg_high.csv", list.inds, list.levels)
res.low <- clean.res("../../output/model/str_chg_low.csv", list.inds, list.levels)
res.nest <- clean.res("../../output/model/str_chg_nest.csv", list.inds, list.levels)
res.nx <- clean.res("../../output/model/str_chg_nx.csv", list.inds, list.levels)
res.wedge <- clean.res("../../output/model/str_chg_wedge.csv", list.inds, list.levels)
res.orig <- clean.res("../../output/model/str_chg_orig.csv", list.inds, list.levels)

## Common aesthetics
customfill <- c("Goods" = "grey50", "Producer services" = "red", "Consumer services" = "blue")

## Plot one country's str change for one equilibrium
## Note: labels, scales, themes, you have to manually add.

plot.one <- function(res, ex.country, ex.type, text_where, ex.title)
    ggplot() +
        geom_area(data = subset(res, country == ex.country & type == ex.type), aes(x = year, y = share, fill = ind), alpha = 0.5, color = "black", outline.type = "full") + 
        scale_fill_manual(values = c("G" = "grey50", "PS" = "red", "CS" = "blue")) +
        geom_text(data = text_where, aes(x = xpos, y = ypos, label = name), size = 5) +
        ggtitle(ex.title)

## Text position for China
xpos.mid <- rep(2006.5,3)

text.where.china.two <- data.frame(xpos = xpos.mid, ypos = c(0.9, 0.62, 0.2), name = c("Consumer services", "Producer services", "Goods"))

## For beamer, I am plotting two equilibria
plot.two <- function(res, ex.country, text_where, i.type.1, i.type.2, name.type.1, name.type.2){

    bb <- plot.one(res, ex.country, i.type.1, text_where, name.type.1) +
        scale_y_continuous(breaks = seq(0, 1, 0.1)) +
        xlab("Year") + ylab("Production shares") +
        theme(legend.position = "none", text = element_text(size = 15), plot.title = element_text(hjust = 0.5), panel.grid.minor.x = element_blank())
    
    cc <- plot.one(res, ex.country, i.type.2, text_where, name.type.2) +
        scale_y_continuous(breaks = seq(0, 1, 0.1), sec.axis = dup_axis()) +
        xlab("Year") + ylab("Production shares") +
        theme(legend.position = "none", text = element_text(size = 15), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5), panel.grid.minor.x = element_blank()) +
        ggtitle(name.type.2)
    
    return(plot_grid(bb, cc))
}

tex.baseline <- TeX(r"(Baseline: $\Delta(\tau^g, \tau^{ps}, \tau^{cs})$)")
tex.data <- TeX(r"(Data: $\Delta(\tau^g, \tau^{ps}, \tau^{cs})$)")
tex.count.s <-  TeX(r"(C#1: only $\Delta(\tau^{ps}, \tau^{cs})$)")
tex.count.g <- TeX(r"(C#2: only $\Delta(\tau^g)$)")
tex.count.no <- TeX(r"(C#3: no $\Delta(\tau^g, \tau^{ps}, \tau^{cs})$)")

pdf(file = "../../doc/figures/china_base_data.pdf", width = 10, height = 6)
plot.two(res, "CHN", text.where.china.two, "baseline", "data", tex.baseline , tex.data)
dev.off()

pdf(file = "../../doc/figures/china_base_s.pdf", width = 10, height = 6)
plot.two(res, "CHN", text.where.china.two, "baseline", "count_only_s", tex.baseline, tex.count.s)
dev.off()

pdf(file = "../../doc/figures/china_base_g.pdf", width = 10, height = 6)
plot.two(res, "CHN", text.where.china.two, "baseline", "count_only_g", tex.baseline,  tex.count.g)
dev.off()

pdf(file = "../../doc/figures/china_base_no.pdf", width = 10, height = 6)
plot.two(res, "CHN", text.where.china.two, "baseline", "count_no", tex.baseline, tex.count.no)
dev.off()

## All four plots together for CHN, IND, VNM, LTU

plot.inner <- function(res, ex.country, text_where, inner.type, inner.name, loc){
    p <- plot.one(res, ex.country, inner.type, text_where, inner.name) +
        theme(axis.title.x = element_blank(), legend.position = "none", axis.text.x = element_text(size = 10), text = element_text(size = 15), plot.title = element_text(hjust = 0.5, size = 11))

    if (loc == "l"){
        p <- p +
            ylab("Production shares") +
            scale_y_continuous(breaks = seq(0, 1, 0.1))  
    }

    if (loc == "m"){
        p <- p +
            scale_y_continuous(breaks = seq(0, 1, 0.1)) +
            theme(axis.title.y = element_blank())
    }

    if (loc == "r"){
        p <- p +
            scale_y_continuous(breaks = seq(0, 1, 0.1), sec.axis = dup_axis()) +
            theme(axis.title.y = element_blank())
    }
    return(p)
}


plot.all <- function(res, ex.country, list.text.where, list.titles)
    plot_grid(plot.inner(res, ex.country, list.text.where[[1]], "baseline", list.titles[[1]], "l"),
              plot.inner(res, ex.country, list.text.where[[2]], "count_only_s", list.titles[[2]], "m"),
              plot.inner(res, ex.country, list.text.where[[3]], "count_only_g", list.titles[[3]], "m"),
              plot.inner(res, ex.country, list.text.where[[4]], "count_no", list.titles[[4]], "r"),
              nrow = 1, rel_widths = c(1.05,1,1,1.07))

short.name.inds <- c("CS", "PS", "G")

list.titles.beamer <- list(tex.baseline, tex.count.s, tex.count.g, tex.count.no)

list.titles.paper <- list("Baseline (G, PS, CS)", "Counterfactual 1 (PS & CS)", "Counterfactual 2 (G)", "Counterfactual 3 (none)")

text.where.china <- rep(list(data.frame(xpos = xpos.mid, ypos = c(0.9, 0.62, 0.2), name = short.name.inds)),4)

plot.all.country <- function(res, ex.country, text.where, list.titles){
    pp <- plot.all(res, ex.country, text.where, list.titles)
    x.grob <- textGrob("Year", gp = gpar(fontsize = 15))
    grid.arrange(arrangeGrob(pp, bottom = x.grob))
}

pdf(file = "../../doc/figures/china_all_beamer.pdf", width = 10, height = 6)
plot.all.country(res, "CHN", text.where.china, list.titles.beamer)
dev.off()

pdf(file = "../../doc/figures/china_all_paper.pdf", width = 10, height = 6)
plot.all.country(res, "CHN", text.where.china, list.titles.paper)
dev.off()

## HS? 
pdf(file = "../../doc/figures/china_all_beamer_hs.pdf", width = 10, height = 6)
plot.all.country(res.hs, "CHN", text.where.china, list.titles.beamer)
dev.off()

pdf(file = "../../doc/figures/china_all_paper_hs.pdf", width = 10, height = 6)
plot.all.country(res.hs, "CHN", text.where.china, list.titles.paper)
dev.off()

## Plot for india
text.where.india <- rep(list(data.frame(xpos = xpos.mid, ypos = c(0.85, 0.57, 0.2), name = short.name.inds)), 4)

pdf(file = "../../doc/figures/india_all_beamer.pdf", width = 10, height = 6)
plot.all.country(res, "IND", text.where.india, list.titles.beamer)
dev.off()

pdf(file = "../../doc/figures/india_all_paper.pdf", width = 10, height = 6)
plot.all.country(res, "IND", text.where.india, list.titles.paper)
dev.off()

## Plots for Vietnam and Lithuania

## Function to denote locations of labels
make.diff.loc.inner <- function(loc) data.frame(xpos = xpos.mid, ypos = loc, name = short.name.inds)
make.diff.loc <- function(list.locs) lapply(list.locs, make.diff.loc.inner)

text.where.vietnam <- make.diff.loc(list(c(0.87, 0.65, 0.3),
                                         c(0.87, 0.55, 0.2),
                                         c(0.87, 0.7, 0.3),
                                         c(0.87, 0.62, 0.3)))

pdf(file = "../../doc/figures/vietnam_all_beamer.pdf", width = 10, height = 6)
plot.all.country(res, "VNM", text.where.vietnam, list.titles.beamer)
dev.off()

pdf(file = "../../doc/figures/vietnam_all_paper.pdf", width = 10, height = 6)
plot.all.country(res, "VNM", text.where.vietnam, list.titles.paper)
dev.off()

text.where.lithuania <- make.diff.loc(list(c(0.85, 0.5, 0.15),
                                           c(0.85, 0.37, 0.05),
                                           c(0.85, 0.61, 0.3),
                                           c(0.85, 0.5, 0.2)))

pdf(file = "../../doc/figures/lithuania_all_beamer.pdf", width = 10, height = 6)
pp <- plot.all.country(res, "LTU", text.where.lithuania, list.titles.beamer)
dev.off()

pdf(file = "../../doc/figures/lithuania_all_paper.pdf", width = 10, height = 6)
pp <- plot.all.country(res, "LTU", text.where.lithuania, list.titles.paper)
dev.off()

##############################
## Results for 66 countries ##
##############################

## Load proportionality index
index <- read.csv(file = "../../output/estimated_params/index.csv")

index.hs <- read.csv(file = "../../output/estimated_params/index_hs.csv")

index.alt <- read.csv(file = "../../output/estimated_params/index_alt.csv")
index.gs <- read.csv(file = "../../output/estimated_params/index_gs.csv")
index.high <- read.csv(file = "../../output/estimated_params/index_high.csv")
index.low <- read.csv(file = "../../output/estimated_params/index_low.csv")
index.orig <- read.csv(file = "../../output/estimated_params/index_orig.csv")

## Full result in a table
res.full <- index

## Developing or advnaced countries?
list.hl <- read.csv("../../data/others/low_high_income.csv", skip = 3)
list.hl <- list.hl[, c("Country", "IMF")]
list.hl$Dev <- ifelse(list.hl$IMF == 1, "N", "Y")
list.hl$IMF <- NULL
colnames(list.hl) <- tolower(colnames(list.hl))

add.dev <- function(res.full) merge(res.full, list.hl, by = "country", all.x = TRUE)

res.full <- add.dev(res.full)

res.full <- res.full[, c("country", "dev", "glre.g", "glre.ps", "index")]

## Results for data, baseline, and three counterfactuals
res.2 <- res
res.2$ind <- factor(res.2$ind, levels = c("G", "PS", "CS"))
res.2 <- res.2[, c("year", "country", "type", "ind", "share")]
res.2 <- subset(res.2, year %in% c(1995, 2018))
res.2$type <- factor(res.2$type, levels = c("data", "baseline", "count_only_s", "count_only_g", "count_no"))

res.2 <- pivot_wider(res.2, values_from = "share", names_from = c("type", "year", "ind"), names_sort = TRUE)

res.full <- merge(res.full, res.2, by = "country", all.x = TRUE)

## Rounding the digits for printing
res.table <- res.full
res.table[, c("glre.g", "glre.ps", "index")] <- round(res.table[, c("glre.g", "glre.ps", "index")],2)
ind.eqs <- grepl("data|baseline|count_", colnames(res.table))
res.table[, ind.eqs] <- round(res.table[, ind.eqs]*100, 0)
cols.cs <- grepl("CS", colnames(res.table))
res.table[, cols.cs] <- NULL
## res.table[, ]

## Remove 1995 info for counterfactuals (they are the same with the baseline anyway.)
cols.rm <- unlist(lapply(paste0("count_", c("only_s", "only_g", "no"), "_1995"), function(x) paste0(x, c("_G", "_PS"))))
prod(cols.rm %in% colnames(res.table))
res.table[, cols.rm] <- NULL
res.table[, 6:19] <- lapply(res.table[, 6:19], as.integer)

sink(file = "../../doc/tables/full_result.tex")
print(xtable(res.table, floating = FALSE))
sink()

res.intro <- subset(res.full, country %in% c("CHN", "IND", "VNM", "LTU", "USA"))
## res.intro <- subset(res.table, country %in% c("CHN", "IND", "VNM", "LTU"))[, c("country", "glre.g", "glre.ps", "index", colnames(res.table)[grepl("baseline|data|count_no", colnames(res.table))])]

write.csv(res.intro, "../../doc/tables/five_country_result.csv", row.names = FALSE)

## Get effect measured in percentage points!!

get.abs.effect <- function(res, index, name.eq = "count_no", name.index = "index"){
    res.18 <- subset(res, type %in% c("baseline", name.eq) & year == 2018)
    res.18 <- pivot_wider(res.18, names_from = "type", values_from = "share")
    res.18$abs.effect <- (res.18$baseline - res.18[, name.eq, drop = TRUE]) * 100 # In percentage points.    
    res.18 <- merge(res.18, index[, c("country", name.index)], by = "country", all = TRUE)
    res.18 <- res.18[, c("country", "ind", "abs.effect", name.index)]
    return(res.18)
}

res.18 <- get.abs.effect(res, index)

res.18.s <- get.abs.effect(res, index, name.eq = "count_only_g", name.index = "glre.ps")
res.18.s <- add.dev(res.18.s)

res.18.g <- get.abs.effect(res, index, name.eq = "count_only_s", name.index = "glre.g")
res.18.g <- add.dev(res.18.g)

## Table for top 10 countries

## New version
gdp <- read.csv("../../output/cleaned_data/gdp_per_capita.csv")
gdp <- subset(gdp, year == "X2018")

res.top10 <- merge(res.table, gdp[, c("country", "gdp")], by = "country", all.x = TRUE)
res.top10$gdp <- round(res.top10$gdp/1e6,1) # unit: trillion dollars
res.top10 <- res.top10[order(-res.top10$gdp), ]
res.top10 <- subset(res.top10, country != "ROW")

cols.kp <- unlist(lapply(paste0("count_", c("only_s", "only_g", "no"), "_2018"), function(x) paste0(x, c("_G", "_PS"))))

res.top10 <- res.top10[1:10, c("country", "gdp", "dev", "glre.g", "glre.ps", "index", 
                               "baseline_1995_G", "baseline_1995_PS", "baseline_2018_G", "baseline_2018_PS", cols.kp)]
row.names(res.top10) <- NULL

sink(file = "../../doc/tables/result_top10.tex")
print(xtable(res.top10, floating = FALSE, digits = c(0,0,1,0,2,2,2,rep(0,10))), include.rownames = FALSE)
sink()

## ## Previous version
## gdp <- read.csv("../../output/cleaned_data/gdp_per_capita.csv")
## gdp <- subset(gdp, year == "X2018")

## res.18.wide <- pivot_wider(res.18, names_from = "ind", values_from = "abs.effect")

## cols.1995 <- c("baseline_1995_G", "baseline_1995_PS", "baseline_1995_CS")
## cols.2018 <- c("baseline_2018_G", "baseline_2018_PS", "baseline_2018_CS")

## res.top10 <- res.full[, c("country", "dev", "index", cols.1995, cols.2018)]

## cols.change <- c("change_95_18_G", "change_95_18_PS", "change_95_18_CS")

## res.top10[, cols.change] <- res.top10[, cols.2018] - res.top10[, cols.1995]

## res.top10[, cols.2018] <- NULL

## res.top10 <- merge(gdp[, c("country", "gdp")], merge(res.top10, res.18.wide[, c("country", "G", "PS", "CS")], by = "country"), by = "country")
## res.top10 <- res.top10[order(-res.top10$gdp), ]

## res.top10$gdp <- res.top10$gdp/1e6 # unit into trillion dollar
## res.top10[, c(cols.1995, cols.change)] <- res.top10[, c(cols.1995, cols.change)]*100 # units into % and p.p.
## res.top10 <- subset(res.top10, country != "ROW")
## res.top10 <- res.top10[1:10, ]
## rownames(res.top10) <- NULL

## sink(file = "../../doc/tables/summary_top10.tex")
## print(xtable(res.top10, floating = FALSE, digits = 1))
## sink()






## MAIN PLOTS for the paper

## Xlim Ylim for main plots
x.lim <- c(-1.1, 1.1)
y.lim <- c(-20.5, 20.5)

## Outliers
res.18$country[abs(res.18$index) > 1.0] #Brunei, Luxemburg, Saudi

plot.index.no.arrow <- function(res.18, x, i.color, x.lim, y.lim, paper = FALSE, x.lab = "Index", x.var = "index", lm.line = TRUE, outlier.countries = c("SAU", "BRN", "LUX")){

    themes <- list(theme(text = element_text(size = 15), plot.margin = unit(c(5.5, 5.5, 30, 80), "pt")))
    if (paper == TRUE){
        themes <- append(list(theme_bw(), theme(plot.title = element_text(hjust = 0.5, size = 15))), themes)
    }
    
    pp <- ggplot(data = subset(res.18, ind == x & !(country %in% outlier.countries)), aes(x = !!sym(x.var), y = abs.effect)) +
        geom_text(aes(label = country), color = i.color) +
        coord_cartesian(xlim = x.lim, ylim = y.lim, clip = "off") +
        scale_x_continuous(limits = x.lim, expand = c(0,0)) +
        scale_y_continuous(limits = y.lim, expand = c(0,0)) +
        xlab(x.lab) +
        ylab("Effect (p.p.)") +
        themes +
        geom_vline(xintercept = 0, linetype = "dotted") +
        geom_hline(yintercept = 0, linetype = "dotted")

    if (lm.line){
        pp <- pp +
            geom_smooth(method = "lm", alpha=0.3, linewidth = 0.5, color = i.color, fullrange = TRUE)
    }
    
    return(pp)
}

## Draw arrows
draw.arrows <- function(start.x, start.y, finish.x, finish.y)
    annotation_custom(segmentsGrob(x0 = start.x, y0 = start.y, x1 = finish.x, y1 = finish.y, gp = gpar(lwd = 1, fill = "black"),arrow = arrow(angle = 15, length = unit(5, 'mm'), type = "closed")))

draw.arrows.four <- function(x.dist.mid, x.dist.end, y.loc, y.dist.mid, y.dist.end, x.loc)
    list(draw.arrows(0.5 + x.dist.mid, y.loc, 1 - x.dist.end, y.loc),
         draw.arrows(0.5 - x.dist.mid, y.loc, 0 + x.dist.end, y.loc),
         draw.arrows(x.loc, 0.5 + y.dist.mid, x.loc, 1 - y.dist.end),
         draw.arrows(x.loc, 0.5 - y.dist.mid, x.loc, 0 + y.dist.end))

## Arrow location for beamer plots
bb.x.dist.mid <- 0.06
bb.x.dist.end <- 0.05
bb.y.loc <- -0.07

bb.y.dist.mid <- 0.12
bb.y.dist.end <- 0.05
bb.x.loc <- -0.06

draw.arrows.beamer <- draw.arrows.four(bb.x.dist.mid, bb.x.dist.end, bb.y.loc, bb.y.dist.mid, bb.y.dist.end, bb.x.loc)

## Annotate arrows for x-axis
annotate.arrow.x <- function(x.left, x.right, ffsize = 14)
    list(annotation_custom(textGrob(TeX(r"($\Delta(\tau)$ \textbf{weakens} $CA^g$)"), x = x.right[1], y = x.right[2], hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(r"($\Delta(\tau)$ \textbf{strengthens} $CA^g$)"), x = x.left[1], y = x.left[2], hjust = 0, gp = gpar(fontsize = ffsize))))

## X arrow annotation location for beamer plots
bb.xarrow.left <- c(0.15, -0.12)
bb.xarrow.right <- c(0.62, -0.12)

## Annotate arrows for y-axis
annotate.arrow.y <- function(x.loc, y.loc, da.v.ad, from.v.to, i.sec, ffsize = 14){
    dd <- "decelerates"
    aa <- "accelerates"
    if (da.v.ad == "da"){
        da.1 <- dd
        da.2 <- aa
    }
    else {
        da.1 <- aa
        da.2 <- dd
    }

    arrow.inner <- function(da)
        paste0("r\"(\\overset{\\normalsize{$\\Delta(\\tau)$}}{\\overset{\\normalsize{\\textbf{",
               da,
               "}}}{\\overset{\\normalsize{str. trans.}}{\\normalsize{",
               from.v.to,
               " $",
               i.sec,
               "$}}}})\"")

    arrow.up <- arrow.inner(da.1)
    arrow.down <- arrow.inner(da.2)

    list.arrows <- list(annotation_custom(textGrob(TeX(eval(parse(text = arrow.up))), x = x.loc, y = 0.5 + y.loc, hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(eval(parse(text = arrow.down))), x = x.loc, y = 0.5 - y.loc, hjust = 0, gp = gpar(fontsize = ffsize))))

    return(list.arrows) 
}

## Y arrow annotation location for beamer plots
bb.yarrow.x.loc <- -0.2
bb.yarrow.y.loc <- 0.27

## Plots for the beamer
pdf(file = "../../doc/figures/index_abs_g_arrow.pdf", width = 10, height = 6)
plot.index.no.arrow(res.18, "G", "black", x.lim, y.lim) +
    draw.arrows.beamer +
    annotate.arrow.x(bb.xarrow.left, bb.xarrow.right) +
    annotate.arrow.y(bb.yarrow.x.loc, bb.yarrow.y.loc, "da", "from", "G")
dev.off()

pdf(file = "../../doc/figures/index_abs_ps_arrow.pdf", width = 10, height = 6)
plot.index.no.arrow(res.18, "PS", "red", x.lim, y.lim) +
    draw.arrows.beamer +
    annotate.arrow.x(bb.xarrow.left, bb.xarrow.right) +
    annotate.arrow.y(bb.yarrow.x.loc, bb.yarrow.y.loc, "ad", "to", "PS")
dev.off()

pdf(file = "../../doc/figures/index_abs_cs_arrow.pdf", width = 10, height = 6)
plot.index.no.arrow(res.18, "CS", "blue", x.lim, y.lim) +
    draw.arrows.beamer +
    annotate.arrow.x(bb.xarrow.left, bb.xarrow.right) +
    annotate.arrow.y(bb.yarrow.x.loc, bb.yarrow.y.loc, "ad", "to", "CS")
dev.off()

write.csv(res.18, "../../doc/figures/index_effect_num.csv", row.names = FALSE)

## Plots for the paper
## Arrow location for paper plots
pp.x.dist.mid <- 0.06
pp.x.dist.end <- 0.05
pp.y.loc <- -0.115

pp.y.dist.mid <- 0.2
pp.y.dist.end <- 0.05
pp.x.loc <- -0.06

## X arrow annotation location for paper plots
pp.xarrow.left <- c(0.15, -0.16)
pp.xarrow.right <- c(0.66, -0.16)

## Y arrow annotation location for paper plots
pp.yarrow.x.loc <- -0.19
pp.yarrow.y.loc <- 0.27

draw.arrows.paper <- draw.arrows.four(pp.x.dist.mid, pp.x.dist.end, pp.y.loc, pp.y.dist.mid, pp.y.dist.end, pp.x.loc)

base.plot <- function(res.18, lm.line.2 = TRUE, outlier.countries.2 = c("SAU", "BRN", "LUX")){
    p1 <- plot.index.no.arrow(res.18, "G", "black", x.lim, y.lim, paper = TRUE, lm.line = lm.line.2, outlier.countries = outlier.countries.2) + ggtitle("(a) Goods") +
        draw.arrows.paper +
        annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
        annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "da", "from", "G", ffsize = 12)

    p2 <- plot.index.no.arrow(res.18, "PS", "red", x.lim, y.lim, paper = TRUE, lm.line = lm.line.2, outlier.countries = outlier.countries.2) + ggtitle("(b) Producer Services") +
        draw.arrows.paper +
        annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
        annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "PS", ffsize = 12)
    
    p3 <- plot.index.no.arrow(res.18, "CS", "blue", x.lim, y.lim, paper = TRUE, lm.line = lm.line.2, outlier.countries = outlier.countries.2) + ggtitle("(c) Consumer Services") +
        draw.arrows.paper +
        annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
        annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "CS", ffsize = 12)

    plot_grid(p1,p2,p3, ncol = 1)
}

pdf(file = "../../doc/figures/index_abs_all_paper.pdf", width = 10, height = 13)
base.plot(res.18)
dev.off()

## Plots for Head and Ries trade costs

res.18.hs <- get.abs.effect(res.hs, index.hs)

## Plots for the beamer
pdf(file = "../../doc/figures/index_abs_g_arrow_hs.pdf", width = 10, height = 6)
plot.index.no.arrow(res.18.hs, "G", "black", x.lim, y.lim, lm.line = FALSE, outlier.countries = c("KHM")) +
    draw.arrows.beamer +
    annotate.arrow.x(bb.xarrow.left, bb.xarrow.right) +
    annotate.arrow.y(bb.yarrow.x.loc, bb.yarrow.y.loc, "da", "from", "G")
dev.off()

pdf(file = "../../doc/figures/index_abs_ps_arrow_hs.pdf", width = 10, height = 6)
plot.index.no.arrow(res.18.hs, "PS", "red", x.lim, y.lim, lm.line = FALSE, outlier.countries = c("KHM")) +
    draw.arrows.beamer +
    annotate.arrow.x(bb.xarrow.left, bb.xarrow.right) +
    annotate.arrow.y(bb.yarrow.x.loc, bb.yarrow.y.loc, "ad", "to", "PS")
dev.off()

pdf(file = "../../doc/figures/index_abs_cs_arrow_hs.pdf", width = 10, height = 6)
plot.index.no.arrow(res.18.hs, "CS", "blue", x.lim, y.lim, lm.line = FALSE, outlier.countries = c("KHM")) +
    draw.arrows.beamer +
    annotate.arrow.x(bb.xarrow.left, bb.xarrow.right) +
    annotate.arrow.y(bb.yarrow.x.loc, bb.yarrow.y.loc, "ad", "to", "CS")
dev.off()

write.csv(res.18.hs, "../../doc/figures/index_effect_num_hs.csv", row.names = FALSE)

## Plot for the paper

pdf(file = "../../doc/figures/index_abs_all_paper_hs.pdf", width = 10, height = 13)
base.plot(res.18.hs, lm.line.2 = FALSE, outlier.countries.2 = c("KHM"))
dev.off()

## For the BASELINE, HS Model, Non-tradable-services model, get the summary statistics.
get.sum.table <- function(res.18){
    res.18$ind <- factor(res.18$ind, levels = c("G", "PS", "CS"))
    sum.table.res <- res.18 %>%
        group_by(ind) %>%
        reframe(stats = c("Mean", "Q1", "Q2", "Q3"),
                value = c(mean(abs(abs.effect)), quantile(abs(abs.effect), c(0.25, 0.5, 0.75)))
                )
    sum.table.res <- pivot_wider(sum.table.res, names_from = "stats", values_from = "value")
    sum.table.res$Sector <- c("Goods", "Producer services", "Consumer services")[match(sum.table.res$ind, c("G", "PS", "CS"))]
    return(sum.table.res)
}

sum.table.res <- get.sum.table(res.18)
sum.table.res$Observations <- as.integer(67)

list.sum.stats <- c("Mean", "Q1", "Q2", "Q3")

sum.table.res.print <- sum.table.res[, c("Sector", "Observations", list.sum.stats)]
sum.table.res.print <- data.frame(sum.table.res.print)

## Print the result
xt <- xtable(sum.table.res.print, align = "rlccccc", digits = 1)
add_to_row <- list(pos = list(-1), command = c("\\hline ")) # Define where to add the extra \hline

sink(file = "../../doc/tables/summary_effect.tex")
print(xt, include.rownames = FALSE, floating = FALSE, add.to.row = add_to_row) # Print the table with the extra \hline
sink()

## Table with 3 specifications
sum.table.res.hs <- get.sum.table(res.18.hs)

res.18.no.s.trade <- get.abs.effect(res.no.s.trade, index)
res.18.no.s.trade$index <- NULL
sum.table.res.no.s <- get.sum.table(res.18.no.s.trade)

sum.table.all <- merge(sum.table.res.print, merge(sum.table.res.no.s, sum.table.res.hs, by = "Sector", suffixes = c(".no.s", ".hs")), by = "Sector")
sum.table.all <- sum.table.all[c(2,3,1),]
sum.table.all[, c("ind.hs", "ind.no.s")] <- NULL

sink(file = "../../doc/tables/summary_effect_all.tex")
print(xtable(sum.table.all, digits = 1), include.rownames = FALSE, floating = FALSE)
sink()

## Model without services trade discussion
## What countries are the exceptions?
dat.exception <- subset(res.18.no.s.trade, (abs.effect > 0 & ind == "G") | (abs.effect < 0 & ind == "PS") | (abs.effect < 0 & ind == "CS"))

writeLines(paste0("Excluding", paste(unique(dat.exception$country), sep = ", "), "in the model without tradable services, globalization accelerated structural transformation of all countries. Furthermore, the max gap between the baseline and no globalization scenario in p.p. was", max(abs(dat.exception$abs.effect))), "../../doc/nums/model_without_tradable_services_exception.txt")

## Plotting
temp.plot <- res.18.no.s.trade
temp.plot$Sector <- c("Goods", "Consumer services", "Producer services")[match(temp.plot$ind, c("G", "CS", "PS"))]
temp.plot$Sector <- factor(temp.plot$Sector, levels = c("Goods", "Producer services", "Consumer services"))

dat.top.y <- data.frame(unlist(lapply(split(temp.plot$abs.effect, temp.plot$country), max)))
dat.top.y$country <- rownames(dat.top.y)
colnames(dat.top.y)[1] <- "top.y"

dat.bottom.y <- data.frame(unlist(lapply(split(temp.plot$abs.effect, temp.plot$country), min)))
dat.bottom.y$country <- rownames(dat.bottom.y)
colnames(dat.bottom.y)[1] <- "bottom.y"

pdf(file = "../../doc/figures/result_model_no_services_trade.pdf", width = 10, height = 6)
ggplot(temp.plot) +
    theme_bw() +
    geom_bar(aes(x = country, y = abs.effect, fill = Sector), position = "dodge", stat = "identity", alpha = 0.5) +
    geom_vline(xintercept = seq(0.5, length(unique(temp.plot$country)) + 0.5, by = 1), linetype = "dotted", alpha = 0.4, linewidth = 0.4) +
    geom_text(aes(x = country, y = top.y, label = country), hjust = -0.3, position = position_dodge(width = 0.9), data = dat.top.y, size = 2.7, angle = 90) +
    geom_text(aes(x = country, y = bottom.y, label = country), hjust = 1.1, position = position_dodge(width = 0.9), data = dat.bottom.y, size = 2.7, angle = 90) +
    theme(axis.text = element_text(size = 13), text = element_text(size = 15), strip.text = element_text(size =16), legend.position = "bottom", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    xlab("Country") + ylab("Effect (p.p.)") +
    scale_y_continuous(limits = c(-2.6, 2.2)) +
    scale_fill_manual(values = c("Goods" = "black", "Consumer services" = "blue", "Producer services" = "red"))
dev.off()

pdf(file = "../../doc/figures/result_model_no_services_beamer.pdf", width = 10, height = 6)
ggplot(temp.plot) +
    geom_bar(aes(x = country, y = abs.effect, fill = Sector), position = "dodge", stat = "identity", alpha = 0.5) +
    geom_vline(xintercept = seq(0.5, length(unique(temp.plot$country)) + 0.5, by = 1), linetype = "dotted", alpha = 0.4, linewidth = 0.4) +
    geom_text(aes(x = country, y = top.y, label = country), hjust = -0.3, position = position_dodge(width = 0.9), data = dat.top.y, size = 2.7, angle = 90) +
    geom_text(aes(x = country, y = bottom.y, label = country), hjust = 1.1, position = position_dodge(width = 0.9), data = dat.bottom.y, size = 2.7, angle = 90) +
    theme(axis.text = element_text(size = 13), text = element_text(size = 15), strip.text = element_text(size =16), legend.position = "bottom", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    xlab("Country") + ylab("Effect (p.p.)") +
    scale_y_continuous(limits = c(-2.6, 2.2)) +
    scale_fill_manual(values = c("Goods" = "black", "Consumer services" = "blue", "Producer services" = "red"))
dev.off()
   

## Plots for other specifications of the model
## 1. Alternative sectoral definition

res.18.alt <- get.abs.effect(res.alt, index.alt)

p1 <- plot.index.no.arrow(res.18.alt, "G", "black", x.lim, y.lim, paper = TRUE) + ggtitle("(a) Goods") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "da", "from", "G", ffsize = 12)

p2 <- plot.index.no.arrow(res.18.alt, "HTS", "orange", x.lim, y.lim, paper = TRUE) + ggtitle("(b) Highly tradable services") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "HTS", ffsize = 12)

p3 <- plot.index.no.arrow(res.18.alt, "BTS", "green4", x.lim, y.lim, paper = TRUE) + ggtitle("(c) Barely tradable services") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "BTS", ffsize = 12)

pdf(file = "../../doc/figures/index_abs_all_paper_alt.pdf", width = 10, height = 13)
plot_grid(p1,p2,p3, ncol = 1)
dev.off()

## 2. Two-sector version

res.18.gs <- get.abs.effect(res.gs, index.gs)
p1 <- plot.index.no.arrow(res.18.gs, "G", "black", x.lim, y.lim, paper = TRUE) + ggtitle("(a) Goods") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "da", "from", "G", ffsize = 12)

p2 <- plot.index.no.arrow(res.18.gs, "S", "purple", x.lim, y.lim, paper = TRUE) + ggtitle("(b) Services") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "S", ffsize = 12)

pdf(file = "../../doc/figures/index_abs_all_paper_gs.pdf", width = 10, height = 8.6666)
plot_grid(p1,p2, ncol = 1)
dev.off()

## 3. Robustness for the baseline sectoral decomposition
## Non-tradable CS
res.18.cs <- get.abs.effect(res.cs, index)

pdf(file = "../../doc/figures/index_abs_all_paper_cs.pdf", width = 10, height = 13)
base.plot(res.18.cs)
dev.off()

## High trade elasticity for services
res.18.high <- get.abs.effect(res.high, index.high)

pdf(file = "../../doc/figures/index_abs_all_paper_high.pdf", width = 10, height = 13)
base.plot(res.18.high)
dev.off()

## Low trade elasticity for services
res.18.low <- get.abs.effect(res.low, index.low)

unique(c(res.18.low$country[abs(res.18.low$index) > 1.0], res.18.low$country[abs(res.18.low$abs.effect) > 20])) #Brunei, Luxemburg, Saudi, Lithuania, Norway, Slovakia, Malta
## x.lim <- c(-1.1, 1.1)
## y.lim <- c(-20.5, 20.5)

res.18.low <- subset(res.18.low, !country %in% c("LTU","NOR", "SVK", "MLT"))

pdf(file = "../../doc/figures/index_abs_all_paper_low.pdf", width = 10, height = 13)
base.plot(res.18.low)
dev.off()

## Wedges approach
res.18.wedge <- get.abs.effect(res.wedge, index.orig)

pdf(file = "../../doc/figures/index_abs_all_paper_wedge.pdf", width = 10, height = 13)
base.plot(res.18.wedge)
dev.off()

## Role of NX
res.18.nx <- get.abs.effect(res.nx, index)

pdf(file = "../../doc/figures/index_abs_all_paper_nx.pdf", width = 10, height = 13)
base.plot(res.18.nx)
dev.off()

## Nested production function
res.18.nest <- get.abs.effect(res.nest, index)

pdf(file = "../../doc/figures/index_abs_all_paper_nest.pdf", width = 10, height = 13)
base.plot(res.18.nest)
dev.off()

## No adjustments for (Trade costs < 1)
res.18.orig <- get.abs.effect(res.orig, index.orig)

pdf(file = "../../doc/figures/index_abs_all_paper_orig.pdf", width = 10, height = 13)
base.plot(res.18.orig)
dev.off()


###################################################################################
## Plots for G and S only globalization and denoting whether they are DEV or ADV ##
###################################################################################

## Plots for the paper
## Arrow location for paper plots
gs.x.dist.mid <- 0.06
gs.x.dist.end <- 0.05
gs.y.loc <- -0.16

gs.y.dist.mid <- 0.2
gs.y.dist.end <- 0.05
gs.x.loc <- -0.06

## X arrow annotation location for paper plots
gs.xarrow.left <- c(0.15, -0.2)
gs.xarrow.right <- c(0.66, -0.2)

## Y arrow annotation location for paper plots
gs.yarrow.x.loc <- -0.19
gs.yarrow.y.loc <- 0.27

draw.arrows.gs <- draw.arrows.four(gs.x.dist.mid, gs.x.dist.end, gs.y.loc, gs.y.dist.mid, gs.y.dist.end, gs.x.loc)

## Annotate arrows for x-axis
annotate.arrow.x.g <- function(x.left, x.right, ffsize = 14)
    list(annotation_custom(textGrob(TeX(r"($\Delta(\tau^g)$ \textbf{weakens} $CA^g$)"), x = x.right[1], y = x.right[2], hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(r"($\Delta(\tau^g)$ \textbf{strengthens} $CA^g$)"), x = x.left[1], y = x.left[2], hjust = 0, gp = gpar(fontsize = ffsize))))

annotate.arrow.x.ps <- function(x.left, x.right, ffsize = 14)
    list(annotation_custom(textGrob(TeX(r"($\Delta(\tau^{ps})$ \textbf{strengthens} $CA^g$)"), x = x.right[1], y = x.right[2], hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(r"($\Delta(\tau^{ps})$ \textbf{weakens} $CA^g$)"), x = x.left[1], y = x.left[2], hjust = 0, gp = gpar(fontsize = ffsize))))

## Annotate arrows for y-axis
annotate.arrow.y.gs <- function(x.loc, y.loc, da.v.ad, from.v.to, i.sec, ffsize = 14, gs = "G"){
    dd <- "decelerates"
    aa <- "accelerates"
    if (da.v.ad == "da"){
        da.1 <- dd
        da.2 <- aa
    }
    else {
        da.1 <- aa
        da.2 <- dd
    }
    
    arrow.inner <- function(da)
        paste0("r\"(\\overset{\\normalsize{$\\Delta(\\tau^{",
               gs, 
               "})$}}{\\overset{\\normalsize{\\textbf{",
               da,
               "}}}{\\overset{\\normalsize{str. trans.}}{\\normalsize{",
               from.v.to,
               " $",
               i.sec,
               "$}}}})\"")

    arrow.up <- arrow.inner(da.1)
    arrow.down <- arrow.inner(da.2)

    list.arrows <- list(annotation_custom(textGrob(TeX(eval(parse(text = arrow.up))), x = x.loc, y = 0.5 + y.loc, hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(eval(parse(text = arrow.down))), x = x.loc, y = 0.5 - y.loc, hjust = 0, gp = gpar(fontsize = ffsize))))

    return(list.arrows) 
}

## Xlim Ylim 
x.lim.g <- 2.6*c(-1, 1)
y.lim.g <- 42.4*c(-1, 1)

x.lab.g <- "Growth rate of relative export trade costs (goods)"

base.plot.g <- function(res.18){
    p1 <- plot.index.no.arrow(res.18, "G", "black", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.g, x.var = "glre.g") + ggtitle("(a) Goods") +
        draw.arrows.gs +
        annotate.arrow.x.g(gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "da", "from", "G", ffsize = 12, gs = "g")

    p2 <- plot.index.no.arrow(res.18, "PS", "red", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.g, x.var = "glre.g") + ggtitle("(b) Producer Services") +
        draw.arrows.gs +
        annotate.arrow.x.g(gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "PS", ffsize = 12, gs = "g")
    
    p3 <- plot.index.no.arrow(res.18, "CS", "blue", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.g, x.var = "glre.g") + ggtitle("(c) Consumer Services") +
        draw.arrows.gs +
        annotate.arrow.x.g(gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "CS", ffsize = 12, gs = "g")

    plot_grid(p1,p2,p3, ncol = 1)
}

pdf(file = "../../doc/figures/index_g_abs_all_paper.pdf", width = 10, height = 13)
base.plot.g(res.18.g)
dev.off()

x.lab.ps <- "Growth rate of relative export trade costs (producer services)"

base.plot.ps <- function(res.18){
    p1 <- plot.index.no.arrow(res.18, "G", "black", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.g, x.var = "glre.ps") + ggtitle("(a) Goods") +
        draw.arrows.gs +
        annotate.arrow.x.ps(gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "da", "from", "G", ffsize = 12, gs = "ps")

    p2 <- plot.index.no.arrow(res.18, "PS", "red", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.g, x.var = "glre.ps") + ggtitle("(b) Producer Services") +
        draw.arrows.gs +
        annotate.arrow.x.ps(gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "PS", ffsize = 12, gs = "ps")
    
    p3 <- plot.index.no.arrow(res.18, "CS", "blue", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.g, x.var = "glre.ps") + ggtitle("(c) Consumer Services") +
        draw.arrows.gs +
        annotate.arrow.x.ps(gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "CS", ffsize = 12, gs = "ps")

    plot_grid(p1,p2,p3, ncol = 1)
}

pdf(file = "../../doc/figures/index_ps_abs_all_paper.pdf", width = 10, height = 13)
base.plot.ps(res.18.s)
dev.off()

## Relationship with GDP.

plot.index.gdp <- function(res.18, i.ind, i.color, x.lim, y.lim, paper = FALSE, outlier.countries = c("SAU", "BRN", "LUX")){

    themes <- list(theme(text = element_text(size = 15), plot.margin = unit(c(5.5, 5.5, 30, 80), "pt")))
    if (paper == TRUE){
        themes <- append(list(theme_bw(), theme(plot.title = element_text(hjust = 0.5, size = 15))), themes)
    }
    
    pp <- ggplot(data = subset(res.18, ind == i.ind & !(country %in% outlier.countries)), aes(x = log(gdppc), y = abs.effect)) +
        geom_text(aes(label = country), color = i.color) +
        coord_cartesian(xlim = x.lim, ylim = y.lim, clip = "off") +
        scale_x_continuous(limits = x.lim, expand = c(0,0)) +
        scale_y_continuous(limits = y.lim, expand = c(0,0)) +
        geom_smooth(method = "lm", alpha=0.3, linewidth = 0.5, color = i.color, fullrange = TRUE) +
        xlab("Log(GDP per capita in 1995)") +
        ylab("Effect (p.p.)") +
        themes +
        geom_hline(yintercept = 0, linetype = "dotted")
    
    return(pp)
}

## Plots for the paper
## Arrows for only y-axis

draw.arrows.two <- function(y.dist.mid, y.dist.end, x.loc)
    list(draw.arrows(x.loc, 0.5 + y.dist.mid, x.loc, 1 - y.dist.end),
         draw.arrows(x.loc, 0.5 - y.dist.mid, x.loc, 0 + y.dist.end))

draw.arrows.gdp <- draw.arrows.two(gs.y.dist.mid, gs.y.dist.end, gs.x.loc)

gdp <- read.csv("../../output/cleaned_data/gdp_per_capita.csv")
gdp <- subset(gdp, year == "X1995")[, c("country", "gdppc")]

add.gdp <- function(res) merge(res, gdp, by = "country", all.x = TRUE)

res.18 <- add.gdp(res.18)

res.18.g <- add.gdp(res.18.g)

res.18.s <- add.gdp(res.18.s)

x.lim.gdp <- c(5.93, 11.64)
y.lim.gdp <- 40*c(-1, 1)

exception.gdp <- c("BRN")

base.plot.gdp <- function(res.18){

    p1 <- plot.index.gdp(res.18, "G", "black", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(i) Goods") +
        draw.arrows.gdp +
        annotate.arrow.y(gs.yarrow.x.loc, gs.yarrow.y.loc, "da", "from", "G", ffsize = 12)

    p2 <- plot.index.gdp(res.18, "PS", "red", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(ii) Producer Services") +
        draw.arrows.gdp +
        annotate.arrow.y(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "PS", ffsize = 12)

    p3 <- plot.index.gdp(res.18, "CS", "blue", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(iii) Consumer Services") +
        draw.arrows.gdp +
        annotate.arrow.y(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "CS", ffsize = 12)
    
    plot_grid(p1,p2,p3, ncol = 1)
}

pdf(file = "../../doc/figures/gdp_abs_all_paper.pdf", width = 10, height = 20)
base.plot.gdp(res.18)
dev.off()

base.plot.gs.gdp <- function(res.18, g.s = "g"){

    p1 <- plot.index.gdp(res.18, "G", "black", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(i) Goods") +
        draw.arrows.gdp +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "da", "from", "G", ffsize = 12, gs = g.s)

    p2 <- plot.index.gdp(res.18, "PS", "red", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(ii) Producer Services") +
        draw.arrows.gdp +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "PS", ffsize = 12, gs = g.s)

    p3 <- plot.index.gdp(res.18, "CS", "blue", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(iii) Consumer Services") +
        draw.arrows.gdp +
        annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "CS", ffsize = 12, gs = g.s)
    
    plot_grid(p1,p2,p3, ncol = 1)
}

pdf(file = "../../doc/figures/gdp_g_abs_all_paper.pdf", width = 10, height = 20)
base.plot.gs.gdp(res.18.g)
dev.off()


summary(lm(abs.effect ~ gdppc, subset(res.18.g, ind == "G")))
summary(lm(abs.effect ~ gdppc, subset(res.18.g, ind == "PS")))
summary(lm(abs.effect ~ gdppc, subset(res.18.s, ind == "G")))
summary(lm(abs.effect ~ gdppc, subset(res.18.s, ind == "PS"))) #6.6% p-value.

pdf(file = "../../doc/figures/gdp_ps_abs_all_paper.pdf", width = 10, height = 20)
base.plot.gs.gdp(res.18.s, g.s = "ps")
dev.off()


###############
## Model fit ##
###############

## Prices
fit.price <- read.csv("../../output/model/model_fit_price.csv")
fit.price <- pivot_longer(fit.price, cols = c(paste0("p.", list.inds), "w"))
fit.price <- pivot_wider(fit.price, names_from = "type", values_from = "value")

fit.price$ind.alp <- ifelse(fit.price$name == "p.g", "(i) Goods",
                      ifelse(fit.price$name == "p.cs", "(iii) Consumer services",
                      ifelse(fit.price$name == "p.ps", "(ii) Producer services",
                      ifelse(fit.price$name == "w", "(iv) Value added", ""))))

fit.price$ind.alp <- factor(fit.price$ind.alp, levels = c("(i) Goods", "(ii) Producer services", "(iii) Consumer services", "(iv) Value added"))

fit.price$ind <- ifelse(fit.price$name == "p.g", "Goods",
                  ifelse(fit.price$name == "p.cs", "Consumer services",
                  ifelse(fit.price$name == "p.ps", "Producer services",
                  ifelse(fit.price$name == "w", "Value added", ""))))

fit.price$ind <- factor(fit.price$ind, levels = c("Goods", "Producer services", "Consumer services", "Value added"))

## PLOT

plot.fit <- function(dat, x.lab, y.lab, i.title, paper = TRUE){
    pp <- ggplot(data = dat, aes(x = log(data), y = log(baseline))) +
        geom_point(alpha = 0.05, size = 3) +
        xlab(x.lab) +
        ylab(y.lab)
    if (paper){
        pp +
            facet_wrap(~ind.alp, scale = "free", ncol = 2) +
            theme_bw() +
            theme(text = element_text(size = 40), axis.text = element_text(size = 30), strip.text = element_text(size = 40, margin = margin(0,0,10,0)), strip.background = element_blank())
    }
    else {
        pp +
            facet_wrap(~ind, scale = "free", ncol = 2) +
            ggtitle(i.title) +
            theme(text = element_text(size = 40), axis.text = element_text(size = 20), plot.title = element_text(hjust = 0.5))
    }
}


png(file = "../../doc/figures/model_fit_price.png", width = 1200, height = 1200)
plot.fit(fit.price, "Log price (data)", "Log price (baseline)", "")
dev.off()

## Import shares
fit.sh <- read.csv("../../output/model/model_fit_share.csv")
list.country <- unique(fit.sh$country)
fit.sh <- pivot_longer(fit.sh, cols = all_of(list.country))
fit.sh <- pivot_wider(fit.sh, names_from = "type", values_from = "value")
fit.sh$ind.alp <- ifelse(fit.sh$ind == "g", "(i) Goods",
                  ifelse(fit.sh$ind == "cs", "(iii) Consumer Services",
                  ifelse(fit.sh$ind == "ps", "(ii) Producer Services", "")))
fit.sh$ind.alp <- factor(fit.sh$ind.alp, levels = c("(i) Goods", "(ii) Producer Services", "(iii) Consumer Services"))

fit.sh$ind <- ifelse(fit.sh$ind == "g", "Goods",
              ifelse(fit.sh$ind == "cs", "Consumer services",
              ifelse(fit.sh$ind == "ps", "Producer services", "")))
fit.sh$ind <- factor(fit.sh$ind, levels = c("Goods", "Producer services", "Consumer services"))

png(file = "../../doc/figures/model_fit_share.png", width = 1200, height = 1200)
plot.fit(fit.sh, "Log import share (data)", "Log import share (baseline)", "")
dev.off()

## PLOTS for beamer

p.price <- plot.fit(fit.price, "Log price (data)", "Log price (baseline)", "Model fit (prices)", paper = FALSE)
p.sh <- plot.fit(fit.sh, "Log import share (data)", "Log import share (baseline)", "Model fit (import shares)", paper = FALSE)

png(file = "../../doc/figures/model_fit_beamer.png", width = 2000, height = 1200)
plot_grid(p.price, p.sh)
dev.off()
