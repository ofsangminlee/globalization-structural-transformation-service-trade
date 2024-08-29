## motivation_two_services.R
## Dividing services sector into producer and consumer services

## Solving package dependency with Renv
renv::load("../")

## Load packages and functions
library("openxlsx")
library("tidyverse")
library("ggrepel")
library("xtable")
source("../common_functions.R")

## Load country names and industry names
list.country <- read.csv("../../output/cleaned_data/list_country.csv")
list.ind <- read.csv("../../output/cleaned_data/list_industry_two.csv")
list.ind.short <- read.csv("../../data/icio/industry_name_isic.csv") # Short industry names for plotting.
list.ind <- merge(list.ind, list.ind.short, by.x = "ind", by.y = "name", all = TRUE)

## Load 2018 ICIO
icio.2018 <- read.csv(unz("../../data/icio/ICIO_2015-2018.zip", "ICIO2021_2018.csv"), header = T)

## Derive intermediate usage intensity + tradedness
## condense.g: option to look at goods sector as a whole or not.
int.tr <- function(dat, condense.g = TRUE){

    colnames(dat)[1] <- "country.ind" 
    colnames(dat) <- gsub("_", "\\.", colnames(dat))
    dat$country.ind <- gsub("_", "\\.", dat$country.ind)
    
    if (condense.g){
        ## One goods sector
        dat <- row.agg.ind(dat, "g", list.ind$code[list.ind$gs == "g"])
        dat <- col.agg.ind(dat, "g", list.ind$code[list.ind$gs == "g"])
    }
    
    ## MEX and CHN 1,2 merge
    dat <- col.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- col.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))
    dat <- row.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- row.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))

    ## How much of output is used as intermediate and final consumption
    list.cons.full <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DPABR")
    dat.int <- dat
    dat.int <- col.agg.ind(dat.int, "c", list.cons.full)
    dat.int <- col.agg.country(dat.int, "ALL", list.country$code)
    dat.int <- row.agg.country(dat.int, "ALL", list.country$code)
    dat.int$int <- dat.int$TOTAL - dat.int$ALL.c
    dat.int <- dat.int[, c("country.ind", "TOTAL", "int", "ALL.c")]
    dat.int$final <- dat.int$ALL.c
    dat.int$ALL.c <- NULL
    dat.int$intermediateness <- dat.int$int/dat.int$TOTAL
    dat.int <- subset(dat.int, !country.ind %in% c("VALU", "OUTPUT", "ALL.TAXSUB"))
    dat.int$country.ind <- name.ind(dat.int$country.ind)

    ## Tradedness
    dat.tr <- dat
    dat.tr <- col.agg.ind(dat.tr, "all", c("g", list.ind$code, list.cons.full))
    dat.tr <- subset(dat.tr, name.ind(country.ind) != "TAXSUB" & country.ind != "VALU" & country.ind != "OUTPUT")
    dat.tr$dom <- NA
    for (i.row in 1:nrow(dat.tr)){
        dat.tr[i.row, "dom"] <- dat.tr[i.row, pasted(name.country(dat.tr[i.row, "country.ind"]), "all")]
    }
    dat.tr$export <- dat.tr$TOTAL - dat.tr$dom
    dat.tr <- dat.tr[, c("country.ind", "TOTAL", "dom", "export")]
    dat.tr <- row.agg.country(dat.tr, "ALL", list.country$code)
    dat.tr$tradedness <- dat.tr$export/dat.tr$TOTAL
    dat.tr$country.ind <- name.ind(dat.tr$country.ind)

    ## Merge intermediate intensity and tradedness
    dat.int.tr <- merge(dat.tr, dat.int, by = "country.ind", ALL = TRUE)
    identical(dat.int.tr$TOTAL.x, dat.int.tr$TOTAL.y)
    dat.int.tr$total <- dat.int.tr$TOTAL.x
    dat.int.tr$TOTAL.x <- dat.int.tr$TOTAL.y <- NULL

    return(dat.int.tr)
}


## 2018 result & divide S into BTS and HTS
int.tr.2018 <- int.tr(icio.2018)
int.tr.2018 <- merge(int.tr.2018, list.ind[, c("code", "short_name")], by.x = "country.ind", by.y = "code", all.x = TRUE)
int.tr.2018 <- clean.colnames(int.tr.2018, "short_name", "short.name")
int.tr.2018$short.name[int.tr.2018$country.ind == "g"] <- "Goods"
int.tr.2018$g.bts.hts <- "BTS"
int.tr.2018$g.bts.hts[int.tr.2018$tradedness > 0.05] <- "HTS"
int.tr.2018$g.bts.hts[int.tr.2018$country.ind == "g"] <- "G"

## Save it as a table
tab.tr <- int.tr.2018
tab.tr <- tab.tr[, c("g.bts.hts", "short.name", "country.ind", "tradedness")]
tab.tr$country.ind[tab.tr$country.ind == "g"] <- "01T39"
tab.tr$country.ind <- gsub("T", "-", tab.tr$country.ind)
tab.tr <- tab.tr[order(-tab.tr$tradedness), ]
tab.tr <- tab.tr[c(3,1,2,4:nrow(tab.tr)), ]
tab.tr$tradedness <- round(tab.tr$tradedness*100,1)
colnames(tab.tr) <- c("Sector", "Industry", "ISIC Rev. 4", "Tradedness (%)")
tab.tr <- cbind(tab.tr[1:(nrow(tab.tr)/2), ], tab.tr[(nrow(tab.tr)/2+1):nrow(tab.tr), ])

sink(file = "../../doc/tables/three_sector.tex")
print(xtable(tab.tr, digits = 1),floating = FALSE, include.rownames = FALSE)
sink()                

## Plotting service industries by intermediate usage and tradedness

plot.tradedness.intermediateness <- function(dat, size.repel = 5, my.theme = NULL, var.color = "g.bts.hts", pal.color = default.color.base)
    ggplot(dat, aes(x = intermediateness, y = tradedness)) + geom_point(aes(color = !!sym(var.color), shape = !!sym(var.color)), size = 3) +
        geom_text_repel(aes(label = short.name, color = !!sym(var.color)), nudge_y = 0.00, size = 5) +
        scale_color_manual(values = pal.color) +
        labs(x = "Total intermediate usage / total gross output", y = "Total exports / total gross output") +
        my.theme +
        theme(legend.position = "none", axis.title=element_text(size=15), axis.text = element_text(size = 13))
      
pdf(file = "../../doc/figures/int_tr_2018_wide.pdf", width = 12, height = 6)
plot.tradedness.intermediateness(int.tr.2018)
dev.off()

pdf(file = "../../doc/figures/int_tr_2018.pdf", width = 10, height = 6)
plot.tradedness.intermediateness(int.tr.2018)
dev.off()

pdf(file = "../../doc/figures/int_tr_2018_paper.pdf", width = 10, height = 6)
plot.tradedness.intermediateness(int.tr.2018, my.theme = theme_bw())
dev.off()

## Save result in CSV
write.csv(int.tr.2018, file = "../../doc/figures/int_tr_num.csv", row.names = FALSE)
## int.tr.2018[order(-int.tr.2018$export), ]g

## Total trade volume for each sector
motivation.num.total.trade <- int.tr.2018[, c("export", "g.bts.hts")] %>%
    group_by(g.bts.hts) %>%
    summarize(total = sum(export))

write.csv(motivation.num.total.trade, file = "../../doc/figures/motivation_trade_volumes.csv", row.names = FALSE)

## Three-sector industry classification.
list.ind.three <- read.csv("../../output/cleaned_data/list_industry_two.csv")
ind.bts <- int.tr.2018$country.ind[int.tr.2018$tradedness < 0.05]
ind.hts <- setdiff(int.tr.2018$country.ind, c(ind.bts, "g"))
list.ind.three$g.s <- list.ind.three$gs
list.ind.three$gs <- NULL
list.ind.three$g.bts.hts <- ifelse(list.ind.three$g.s == "g", "g",
                          ifelse(list.ind.three$code %in% ind.bts, "bts",
                          ifelse(list.ind.three$code %in% ind.hts, "hts", NA)))
write.csv(list.ind.three, file = "../../output/cleaned_data/list_industry_three.csv", row.names = FALSE)

## Disaggregated goods industry version
int.tr.2018.g <- int.tr(icio.2018, FALSE)
int.tr.2018.g <- merge(int.tr.2018.g, list.ind[, c("code", "short_name")], by.x = "country.ind", by.y = "code", all.x = TRUE)
int.tr.2018.g <- clean.colnames(int.tr.2018.g, "short_name", "short.name")
int.tr.2018.g$g.bts.hts <- "BTS"
int.tr.2018.g$g.bts.hts[int.tr.2018.g$tradedness > 0.05] <- "HTS"
int.tr.2018.g$g.bts.hts[int.tr.2018.g$country.ind %in% list.ind$code[list.ind$ISIC4_one %in% LETTERS[1:5]]] <- "G"

pdf(file = "../../doc/figures/int_tr_2018_g.pdf", width = 10, height = 6)
plot.tradedness.intermediateness(int.tr.2018.g, size.repel = 4)
dev.off()

pdf(file = "../../doc/figures/int_tr_2018_g_paper.pdf", width = 10, height = 6)
plot.tradedness.intermediateness(int.tr.2018.g, size.repel = 4, my.theme = theme_bw())
dev.off()

## Save the numbers for the figures.
int.tr.2018.g <- rbind(int.tr.2018.g, subset(int.tr.2018, g.bts.hts == "G"))
write.csv(int.tr.2018.g, "../../doc/figures/int_tr_num_g.csv", row.names = FALSE)

######################
## Robustness check ##
######################

## Two service industries (Accomodation & Food and Art, Entertainment, Recreation) can be classified differently.
## Instead of HTS and BTS, I can classify services into Producer Services and Consumer Services by reclassifying the two industries.

## Check 1. how much does the two industries account for in gross output and trade?
dat.aa <- int.tr.2018[, c("country.ind", "short.name", "g.bts.hts", "export", "total")]
dat.aa <- subset(dat.aa, g.bts.hts!= "G")
dat.aa <- data.frame(ind = c("BTS", "HTS", "AA"),
                      go = c(sum(dat.aa$total[dat.aa$g.bts.hts == "BTS"]), sum(dat.aa$total[dat.aa$g.bts.hts == "HTS"]), sum(dat.aa$total[dat.aa$short.name %in% c("Art, entertainment, recreation", "Accommodation & food")])),
                      export = c(sum(dat.aa$export[dat.aa$g.bts.hts == "BTS"]), sum(dat.aa$export[dat.aa$g.bts.hts == "HTS"]), sum(dat.aa$export[dat.aa$short.name %in% c("Art, entertainment, recreation", "Accommodation & food")]))
                     )
write.csv(dat.aa, "../../doc/nums/fraction_aa.csv", row.names = FALSE)

## About 12% of BTS and 10% of PS
dat.aa$go[dat.aa$ind == "AA"]/dat.aa$go[dat.aa$ind == "BTS"]
dat.aa$go[dat.aa$ind == "AA"]/dat.aa$go[dat.aa$ind == "HTS"]
dat.aa$go[dat.aa$ind == "AA"]/(dat.aa$go[dat.aa$ind == "HTS"] + dat.aa$go[dat.aa$ind == "BTS"])

## About 7% of total trade
dat.aa$export[dat.aa$ind == "AA"]/(dat.aa$export[dat.aa$ind == "BTS"] + dat.aa$export[dat.aa$ind == "HTS"])
dat.aa$export[dat.aa$ind == "AA"]/dat.aa$export[dat.aa$ind == "HTS"]
dat.aa$export[dat.aa$ind == "AA"]/dat.aa$export[dat.aa$ind == "BTS"]

## Check 2. Run the whole code with a different sectoral composition. This is what I do for the paper. More robust.
ind.cs <- int.tr.2018$country.ind[int.tr.2018$intermediateness < 0.4]
ind.ps <- setdiff(int.tr.2018$country.ind, c(ind.cs, "g"))
list.ind.three$g.cs.ps <- ifelse(list.ind.three$g.s == "g", "g",
                          ifelse(list.ind.three$code %in% ind.cs, "cs",
                          ifelse(list.ind.three$code %in% ind.ps, "ps", NA)))
write.csv(list.ind.three, file = "../../output/cleaned_data/list_industry_three_alt.csv", row.names = FALSE)

## Plotting
int.tr.2018$g.cs.ps <- "CS"
int.tr.2018$g.cs.ps[int.tr.2018$intermediateness > 0.4] <- "PS"
int.tr.2018$g.cs.ps[int.tr.2018$country.ind == "g"] <- "G"

pdf(file = "../../doc/figures/int_tr_2018_alt.pdf", width = 10, height = 6)
plot.tradedness.intermediateness(int.tr.2018, var.color = "g.cs.ps", pal.color = default.color.alt)
dev.off()

pdf(file = "../../doc/figures/int_tr_2018_alt_paper.pdf", width = 10, height = 6)
plot.tradedness.intermediateness(int.tr.2018, var.color = "g.cs.ps", pal.color = default.color.alt, my.theme = theme_bw())
dev.off()

##########################################
## How much of CS and PS are traveling? ##
##########################################

## Calculate trade volumes by end usage: intermediate, C (other consumption) and DPABR (direct purchases abroad)
tr.type <- function(dat, condense.g = TRUE){

    colnames(dat)[1] <- "country.ind" 
    colnames(dat) <- gsub("_", "\\.", colnames(dat))
    dat$country.ind <- gsub("_", "\\.", dat$country.ind)

    if (condense.g){
        ## One goods sector
        dat <- row.agg.ind(dat, "g", list.ind$code[list.ind$gs == "g"])
        dat <- col.agg.ind(dat, "g", list.ind$code[list.ind$gs == "g"])
    }
    

    ## MEX and CHN 1,2 merge
    dat <- col.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- col.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))
    dat <- row.agg.country(dat, "MEX", c("MEX", "MX1", "MX2"))
    dat <- row.agg.country(dat, "CHN", c("CHN", "CN1", "CN2"))

    ## Check how much trade by type
    ## Sectoral trade check. (c.no / dpbar / int)
    list.cons.no <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT")
    list.cons.dpabr <- "DPABR"
    dat <- col.agg.ind(dat, "c.no", list.cons.no)
    dat <- col.agg.ind(dat, "int", c("g", list.ind$code))
    dat <- subset(dat, name.ind(country.ind) != "TAXSUB" & country.ind != "VALU" & country.ind != "OUTPUT")
    list.ind.inner <- c("c.no", "int", "DPABR")
    dat[, pasted(list.ind.inner, "dom")] <- NA
    for (i.ind in 1:length(list.ind.inner)){
        for (i.row in 1:nrow(dat)){
            dat[i.row, pasted(list.ind.inner[i.ind], "dom")] <- dat[i.row, pasted(name.country(dat[i.row, "country.ind"]), list.ind.inner[i.ind])]
        }
    }

    for (i.ind in 1:length(list.ind.inner)){
        dat[, pasted(list.ind.inner[i.ind], "total")] <- rowSums(dat[, name.ind(colnames(dat)) == list.ind.inner[i.ind]])
        dat[, pasted(list.ind.inner[i.ind], "export")] <- dat[, pasted(list.ind.inner[i.ind], "total")] - dat[, pasted(list.ind.inner[i.ind], "dom")]
    }
    
    dat <- dat[, c("country.ind", pasted(list.ind.inner, "total"), pasted(list.ind.inner, "dom"), pasted(list.ind.inner, "export"))]
    dat <- row.agg.country(dat, "ALL", list.country$code)
    dat$ind <- name.ind(dat$country.ind)
    dat$country.ind <- NULL

    return(dat)
}

tr.type.2018 <- tr.type(icio.2018)

tr.type.2018 <- merge(tr.type.2018, int.tr.2018[, c("country.ind", "g.bts.hts", "short.name")], by.x = "ind", by.y = "country.ind")

tr.type.sum <- tr.type.2018 %>%
    group_by(g.bts.hts) %>%
    summarize(c.no.export = sum(c.no.export), int.export = sum(int.export), dpabr.export = sum(DPABR.export))

tr.type.sum$total.export <- rowSums(tr.type.sum[, colnames(tr.type.sum) != "g.bts.hts"])
tr.type.sum$dpabr.share <- tr.type.sum$dpabr.export/tr.type.sum$total.export

write.csv(tr.type.sum, file = "../../doc/nums/share_type_export.csv", row.names = FALSE)
