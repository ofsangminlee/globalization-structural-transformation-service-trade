## common_functions.R
## Contains often-used functions for data manipulation + functions to calculate GDP from input output tables.

#############################################
## Often-used functions for wrangling data ##
#############################################

## Get country and industry names from country-industry pair (e.g. AUS.g -> AUS and g)
name.country <- function(x) substr(x, 1, 3)
name.ind <- function(x) substr(x, 5, nchar(x))

## Paste with dot. (Period is a separator between a country and an industry.)
pasted <- function(x,y) paste0(x,".",y)

## Get flow from a producing country-industry to consuming country-industry.
prod.cons <- function(country.prod, ind.prod, country.cons, ind.cons, dat){
    return(dat[dat$country.ind %in% pasted(country.prod, ind.prod), pasted(country.cons, ind.cons)])
}

## Change column names
clean.colnames <- function(dat, old.cols, new.cols){
    stopifnot(length(old.cols) == length(new.cols))
    for (i in 1:length(old.cols)){
        colnames(dat)[colnames(dat) == old.cols[i]] <- new.cols[i]
    }
    return(dat)
}

## Functions for condensing ICIO

## Column aggregations for industries 
col.agg.ind <- function(data, new.ind, old.inds){
    index.inds <- name.ind(colnames(data)) %in% old.inds
    temp <- data[, index.inds]  # Data to be aggregated
    not.temp <- data[, !index.inds] # Not to be aggregated
    list.countries <- unique(name.country(colnames(temp)))
    temp <- do.call("cbind", lapply(list.countries, function(x){
        temp2 <- as.data.frame(rowSums(temp[, name.country(colnames(temp)) == x]))
        colnames(temp2) <- pasted(x, new.ind)
        return(temp2)
    }))
    return(cbind(not.temp,temp))
}

## Row aggregation for industries
row.agg.ind <- function(data, new.ind, old.inds){ 
    temp <- subset(data, name.ind(country.ind) %in% old.inds)
    not.temp <- subset(data, !name.ind(country.ind) %in% old.inds)
    temp <- split(temp, name.country(temp$country.ind))
    temp <- do.call("rbind", lapply(temp, function(x){
        country <- name.country(x$country.ind[1])
        x$country.ind <- NULL
        return(cbind(data.frame(country.ind = pasted(country, new.ind), as.data.frame(t(colSums(x))))))
    }))
    return(rbind(not.temp, temp))
}

## Column aggregation for countries
col.agg.country <- function(data, new.country, old.countries){
    index.countries <- name.country(colnames(data)) %in% old.countries
    temp <- data[, index.countries]  # Data to be aggregated
    not.temp <- data[, !index.countries] # Not to be aggregated
    list.inds <- unique(name.ind(colnames(temp)))
    temp <- do.call("cbind", lapply(list.inds, function(x){
        ## To deal with MEX and CHN, which have separate country entries (MX1, MX2, CN1, CN2) to account for firm heterogeneities.
        if(sum(name.ind(colnames(temp)) == x) == 1){
            temp2 <- temp[, name.ind(colnames(temp)) == x, drop = FALSE]
        }
        else {
            temp2 <- as.data.frame(rowSums(temp[, name.ind(colnames(temp)) == x]))
        }
        colnames(temp2) <- pasted(new.country, x)
        return(temp2)
    }))
    return(cbind(not.temp,temp))
}

## Row aggregation for countries
row.agg.country <- function(data, new.country, old.countries){ 
    temp <- subset(data, name.country(country.ind) %in% old.countries)
    not.temp <- subset(data, !name.country(country.ind) %in% old.countries)
    temp <- split(temp, name.ind(temp$country.ind))
    temp <- do.call("rbind", lapply(temp, function(x){
        ind <- name.ind(x$country.ind[1])
        x$country.ind <- NULL
        return(cbind(data.frame(country.ind = pasted(new.country, ind), as.data.frame(t(colSums(x))))))
    }))
    return(rbind(not.temp, temp))
}

## Functions for calculating GDP

## Production-side GDP
## gross value-added (output at basic prices - intermediate consumption at purchaser's prices) + taxes less subsidies = (output at basic prices - intermediate consumption at basic prices - TLS for intermediate consumption) + (TLS for int and TLS for final demand)
gdp.prod <- function(country, dat, with.tax){
    if(with.tax){
        va.country <- sum(dat[dat$country.ind == "VALU", name.country(colnames(dat)) == country]) # Value-added from production
        taxsub.country <- dat[dat$country.ind == pasted(country, "TAXSUB"), "TOTAL"] # Total TAXSUB from domestic production and consumption, foreigner's direct purchases  
        return(va.country + taxsub.country)
    }
    else{
        va.country <- sum(dat[dat$country.ind == "VALU", name.country(colnames(dat)) == country]) # Value-added from production
        return(va.country)
    }
}

## Expenditure-side: C + I + G + NX
## Note: NX terms include direct purchases abroad
nx.country <- function(country, dat){
    x.country <- sum(dat[(!dat$country.ind %in% c("VALU", "OUTPUT")) & (name.country(dat$country.ind) == country), (colnames(dat) != "country.ind") & (name.country(colnames(dat)) != country) &  (colnames(dat) != "TOTAL")])
    i.country <- sum(dat[(!dat$country.ind %in% c("VALU", "OUTPUT")) & (name.country(dat$country.ind) != country), (colnames(dat) != "country.ind") & (name.country(colnames(dat)) == country) &  (colnames(dat) != "TOTAL")])
    return(x.country - i.country)
}

gdp.exp <- function(country, dat, list.cons){
    cig.country <- sum(dat[, colnames(dat) %in% pasted(country, list.cons)])
    nx.country <- nx.country(country, dat)
    return(cig.country + nx.country)
}

#######################################
## Often-used functions for plotting ##
#######################################

## Theme and x axis for plotting time series.
## Geom_point size = 2
## Geom_dl, cex = 1
## Line?? Default linewidth.
## plot.title = element_text(hjust = 0.5, size = 15)))


## Default: 13, 15
plot.time.options <- function(x.limits = c(1995, 2018), bw = TRUE, ...){
    res <- list(xlab("Year"),
                scale_x_continuous(limits = x.limits, breaks = seq(1995, 2018, 5)),
                theme(legend.position = "none", axis.text = element_text(size = 12), text = element_text(size = 14), strip.text = element_text(size =15), panel.grid.minor.x = element_blank())# If I don't set sizes for  axis.text separately, due to the incompleteness of the theme, 15*0.8 (default rel size) will be the default size for axis.text.
                )
    if (bw){
        res <- append(list(theme_bw(), theme(strip.background = element_blank())), res)
        return(res)
    }
    else {
        return(res)
    }
}

## Theme for plotting cross-country scatter plot
## If I don't set sizes for  axis.text separately, due to the incompleteness of the theme, 15*0.8 (default rel size) will be the default size for axis.text. & strip.text size would affect the size of the subtitle.
## 13, 15, 16 was original setting
plot.scatter.options <- function(bw = TRUE){
    res <- list(theme(legend.position = "none", axis.text = element_text(size = 12), text = element_text(size = 14), strip.text = element_text(size =15)) 
                )
    if (bw){
        res <- append(list(theme_bw(), theme(strip.background = element_blank())), res)
        return(res)
    }
    else {
        return(res)
    }
}

## ## Rescaling axis (ggplot axies doesn't show min and max grid points well)
## plot.rescale <- function(num.limits, num.breaks, axis){
##     if (axis == "x"){
##         return(list(scale_x_continuous(limits = num.limits, breaks = num.breaks)))
##     }
##     if (axis == "x"){
##         return(list(scale_x_continuous(limits = num.limits, breaks = num.breaks)))
##     }
## }

## Default aesthetics

## Default colors for plotting
default.color.base <- c("BTS" = "blue", "HTS" = "red", "G" = "black")
default.color.alt  <- c("CS" = "green4", "PS" = "orange", "G" = "black")
default.color.gs <- c("S" = "purple", "G" = "black")

## Default shapes for plotting
default.shape.base <- c("G" = 17, "HTS" = 15, "BTS" = 16)
default.shape.alt <- c("G" = 17, "PS" = 15, "CS" = 16)
default.shape.gs <- c("G" = 17, "S" = 15)

## Default linetypes for plotting
default.linetype.base <- c("G" = "solid", "HTS" = "dashed", "BTS" = "dotted")
default.linetype.alt <- c("G" = "solid", "PS" = "dashed", "CS" = "dotted")
default.linetype.gs <- c("G" = "solid", "S" = "dashed")

## For facet_wrap, adding title

add.title <- function(dat){
    dat$title.alp <- ifelse(dat$ind == "G", "(a) Goods",
                     ifelse(dat$ind == "HTS", "(b) Highly Tradable Services",
                     ifelse(dat$ind == "BTS", "(c) Barely Tradable Services", "")))
    dat$title.alp <- factor(dat$title.alp, levels = c("(a) Goods", "(b) Highly Tradable Services", "(c) Barely Tradable Services"))

    dat$title.rom <- ifelse(dat$ind == "G", "(i) Goods",
                     ifelse(dat$ind == "HTS", "(ii) Highly Tradable Services",
                     ifelse(dat$ind == "BTS", "(iii) Barely Tradable Services", "")))
    dat$title.rom <- factor(dat$title.rom, levels = c("(i) Goods", "(ii) Highly Tradable Services", "(iii) Barely Tradable Services"))
    
    dat$title <- ifelse(dat$ind == "G", "Goods",
                 ifelse(dat$ind == "HTS", "Highly Tradable Services",
                 ifelse(dat$ind == "BTS", "Barely Tradable Services", "")))
    dat$title <- factor(dat$title, levels = c("Goods", "Highly Tradable Services", "Barely Tradable Services"))
    
    return(dat)
}

add.title.alt <- function(dat){
    dat$title.alp <- ifelse(dat$ind == "G", "(a) Goods",
                     ifelse(dat$ind == "PS", "(b) Producer Services",
                     ifelse(dat$ind == "CS", "(c) Consumer Services", "")))
    dat$title.alp <- factor(dat$title.alp, levels = c("(a) Goods", "(b) Producer Services", "(c) Consumer Services"))

    dat$title.rom <- ifelse(dat$ind == "G", "(i) Goods",
                     ifelse(dat$ind == "PS", "(ii) Producer Services",
                     ifelse(dat$ind == "CS", "(iii) Consumer Services", "")))
    dat$title.rom <- factor(dat$title.rom, levels = c("(i) Goods", "(ii) Producer Services", "(iii) Consumer Services"))

    dat$title <- ifelse(dat$ind == "G", "Goods",
                 ifelse(dat$ind == "PS", "Producer Services",
                 ifelse(dat$ind == "CS", "Consumer Services", "")))
    dat$title <- factor(dat$title, levels = c("Goods", "Producer Services", "Consumer Services"))
    
    return(dat)
}


add.title.gs <- function(dat){
    dat$title.alp <- ifelse(dat$ind == "G", "(a) Goods",
                     ifelse(dat$ind == "S", "(b) Services",""))
    dat$title <- ifelse(dat$ind == "G", "Goods",
                 ifelse(dat$ind == "S", "Services", ""))
    
    return(dat)
}
   

## Cowplot vs facet. Cowplots = you can have different axes for subplots.



