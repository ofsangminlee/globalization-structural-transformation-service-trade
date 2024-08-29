## get_bea_prod.R
## Obtain productivity growth rates from BEA KLEMS

## Solving package dependency with Renv
renv::load("../")

## Load libraries
library("openxlsx")
library("tidyverse")

## Read data
loc.file <- "../../data/bea/BEA-BLS-industry-level-production-account-1987-2020.xlsx"

## Gross output quantity (Q) and volumes (P*Q)
sheet.names <- getSheetNames(loc.file)
(q.go <- sheet.names[11])
(x.go <- sheet.names[29])
q.go <- read.xlsx(loc.file, sheet = q.go, startRow = 2)
x.go <- read.xlsx(loc.file, sheet = x.go, startRow = 2)
list.go <- list("q.go" = q.go, "x.go" = x.go)

## Inputs in Q and PQ
## Capital (arts)
(q.c.art <- sheet.names[3])
(x.c.art <- sheet.names[19])
## Capital (R&D)
(q.c.rd <- sheet.names[4])
(x.c.rd <- sheet.names[22])
## Capital (IT)
(q.c.it <- sheet.names[5])
(x.c.it <- sheet.names[20])
## Capital (Software)
(q.c.soft <- sheet.names[7])
(x.c.soft <- sheet.names[23])
## Capital (Other)
(q.c.other <- sheet.names[6])
(x.c.other <- sheet.names[21])
## Energy
(q.e <- sheet.names[8])
(x.e <- sheet.names[24])
## Materials
(q.m <- sheet.names[9])
(x.m <- sheet.names[25])
## Services
(q.s <- sheet.names[10])
(x.s <- sheet.names[26])
## Labor (college)
(q.l.col <- sheet.names[13])
(x.l.col <- sheet.names[28])
## Labor (non-college)
(q.l.no <- sheet.names[14])
(x.l.no <- sheet.names[27])

## READ the sheets to get Q and PQ for inputs
list.obj <- c("c.art", "c.rd", "c.it", "c.soft", "c.other", "e", "m", "s", "l.col", "l.no")
list.q.name <- paste0("q.", list.obj)
list.x.name <- paste0("x.", list.obj)

for (i in c(list.q.name, list.x.name)){
    temp <- eval(paste0("read.xlsx(\"", eval(loc.file), "\", sheet = \"", eval(parse(text =i)), "\", startRow = 2)"))
    assign(i, eval(parse(text = temp)))
}

list.q <- list()
for (i in list.q.name){
    temp <- list(eval(parse(text = i)))
    names(temp) <- i
    list.q <- c(list.q, temp)
}

list.x <- list()
for (i in list.x.name){
    temp <- list(eval(parse(text = i)))
    names(temp) <- i
    list.x <- c(list.x, temp)
}

## All are same dimensions and rename columns
for (i in c(list.go, list.q, list.x)){
    stopifnot(nrow(i) == 63)
    stopifnot(ncol(i) == 35)
}

## Classify industries to three sectors
## NAICS to G, BTS, HTS
## Plus colnames years to "X1995" years.
naics <- read.csv("../../data/bea/naics_three.csv")
colnames(naics) <- c("ind.full", "ind")

ind.sort <- naics$ind.full

## Add three sector definition
## Years -> X Years
## Order the sheets in the same way
merge.naics <- function(dat){
    stopifnot(identical(colnames(dat)[2:35], as.character(1987:2020)))
    colnames(dat)[2:35] <- paste0("X", colnames(dat)[2:35])
    dat$ind.full <- dat$Industry.Description
    dat$Industry.Description <- NULL
    dat <- merge(dat, naics, by = "ind.full", all.x = TRUE)
    dat <- dat[match(ind.sort, dat$ind.full), ]
    return(dat)
}

list.go <- lapply(list.go, merge.naics)
list.q <- lapply(list.q, merge.naics)
list.x <- lapply(list.x, merge.naics)

## 1. Calculate total input usage (volume) for aggregate sectors G, BTS, and HTS. E.g. for G, how much KLEMS did it employ?
## These will be (i) weights for tfp calculation and (ii) be used for aggregating subsectors into Tornqvist quantity index of each input category.
years.x <- paste0("X", 1987:2020)
years.prev <- paste0("X", 1987:2019)
years.next <- paste0("X", 1988:2020)

agg.ind <- function(dat) do.call("rbind", lapply(split(dat, dat$ind), function(x) data.frame(ind = x$ind[1], t(colSums(x[, years.x])))))

## Gross-output of a sector
x.go.agg <- lapply(list.go["x.go"], agg.ind)

## Input usage in volume of a sector by input category
x.input.agg <- lapply(list.x, agg.ind)

## Fraction of an input category for a sector.
## E.g. how much does no-college labor account for in goods production?
weight.input.agg.raw <- lapply(x.input.agg, function(dat){
    stopifnot(identical(dat$ind, x.go.agg[[1]]$ind)) # Check x.agg and go.agg are ordered the same.
    res <- dat[, years.x]/x.go.agg[[1]][, years.x]
    res$ind <- dat$ind
    return(res)
})

## Do input shares sum up to one?
test.sum.weight <- function(dat, years){
    temp <- dat[[1]][, years]
    for (i in dat[2:length(dat)]){
        temp <- temp + i[, years]
    }
    return(temp)
}

max(abs(unlist(test.sum.weight(weight.input.agg.raw, years.x)) - 1))

## Function to get weights for tornqvist index (0.5*previous-year + 0.5*current-year)
get.weight.torn <- function(dat.weight, names.ind){
    ## Sanity check: ordering of industries the same across dataframes?
    for (i in dat.weight){
        stopifnot(identical(i[, names.ind], dat.weight[[1]][, names.ind]))
    }
    ## Previous year weights: column X2000 detnoes input weights in X2019.
    dat.prev <- lapply(dat.weight, function(dat){
        dat <- dat[, c(names.ind, years.prev)]
        colnames(dat)[(ncol(dat) - length(years.prev) + 1):ncol(dat)] <- years.next
        return(dat)
    })
    ## Calculate weight from previous-year and current-year weights
    res.final <- mapply(function(w.prev, w.next){
        res <- 0.5*(w.prev[, years.next] + w.next[, years.next])
        for (i in 1:length(names.ind)){
            res[, names.ind[i]] <- w.prev[, names.ind[i]]
        }
        return(res)   
    }, dat.prev, dat.weight, SIMPLIFY = FALSE)
    stopifnot(identical(names(res.final), names(dat.weight)))
    return(res.final)
}

weight.input.agg <- get.weight.torn(weight.input.agg.raw, "ind")

max(abs(unlist(test.sum.weight(weight.input.agg, years.next))-1))

## 2. Inputs: aggregate them in a Tornqvist index way
## Quantity is the one to do. (indirect method)
## What you are doing: \sum_i w_{i,t+1}[log(q_i,t+1) - log(q_i,t)], where
## w_{i,t} = 0.5*(p_{i,t}q_{i,t}/sum(pq) + p_{i,t+1}q_{i,t+1}/sum(pq))

## 2.1. Input weights
## Input usage (volume) for g, bts, hts
x.input.agg.long <- lapply(x.input.agg, function(x) pivot_longer(x, cols = all_of(years.x), names_to = "year", values_to = "total"))

## Input usage (volume) for detailed industry
x.input.detail.long <- lapply(list.x, function(x) pivot_longer(x, cols = all_of(years.x), names_to = "year", values_to = "value"))

## Function to calculate weights each year and turning it back into wide format
## Weights: for non-college labor usage of "G" sector, how much was it from "Farms" industry?
get.weight.inner <- function(dat.agg.long, dat.detail.long){
    res <- mapply(function(x.ind,x.tot){
        temp <- merge(x.ind,x.tot, by = c("ind", "year"), all.x = TRUE)
        temp$weight <- temp$value/temp$total # value: usage by industry, total: usage by sector
        return(temp)
    }, dat.detail.long, dat.agg.long, SIMPLIFY = FALSE)

    res <- lapply(res, function(x){
        temp <- x[, c("ind.full", "ind", "year", "weight")]
        temp$weight[is.nan(temp$weight)] <- 0 # This happens when total input is 0. It is 0/0. e.g. capital (art) input to goods.
        temp <- pivot_wider(temp, names_from = "year", values_from = "weight")
        return(temp)
    })
    return(res)
}

weight.input.detail.raw <- get.weight.inner(x.input.agg.long, x.input.detail.long)

## Sanity check: does weight sum up to 1? (or 0?)
test.sum.weight.cols <- function(dat, i.ind, years){
    dat.test <- lapply(dat, function(x) subset(x, ind == i.ind))
    dat.test <- lapply(dat.test, function(x) colSums(x[, years])) # This should be either all 0 or 1. 0 because c.art is not used for G.
    return(dat.test)
}

table(unlist(test.sum.weight.cols(weight.input.detail.raw, "g", years.x)))

## Get tornqvist index.
weight.input.detail <- get.weight.torn(weight.input.detail.raw, c("ind", "ind.full"))

table(unlist(test.sum.weight.cols(weight.input.detail, "g", years.next)))

## 2.2 Calculate changes in log(q)
## AS ALWAYS, ORDERING MATTERS.
for (i in list.q){
    stopifnot(identical(i$ind.full, list.q[[1]]$ind.full))
}

get.log.q.gap <- function(dat.q, names.ind){
    for (i in dat.q){
        stopifnot(identical(i[, names.ind], dat.q[[1]][, names.ind]))
    }
    dat.prev <- lapply(dat.q, function(dat){
        dat <- dat[, c(names.ind, years.prev)]
        colnames(dat)[(ncol(dat) - length(years.prev) + 1):ncol(dat)] <- years.next
        return(dat)
    })
    res.final <- mapply(function(q.prev, q.next){
        res <- log(q.next[, years.next]) - log(q.prev[, years.next])
        for (i in names.ind){
            res[, i] <- q.prev[, i]
        }
        return(res)   
    }, dat.prev, dat.q, SIMPLIFY = FALSE)
    stopifnot(identical(names(res.final), names(dat.q)))
    return(res.final)
}

q.gap.detail <- get.log.q.gap(list.q, c("ind", "ind.full"))

## 2.3 Calculate the final ones. changes in log(q)*weight summed.
## AS ALWAYS, ordering matters. Re order input weights.
identical(q.gap.detail[[1]]$ind.full, weight.input.detail[[1]]$ind.full)

weight.input.detail <- lapply(weight.input.detail, function(x) x[match(q.gap.detail[[1]]$ind.full, x$ind.full), ])

for (i in c(weight.input.detail, q.gap.detail)){
    stopifnot(identical(i$ind.full, q.gap.detail[[1]]$ind.full))
}

## Sum them across industry
sum.industry <- function(dat.w, dat.qq){
    for (i in c(dat.w, dat.qq)){
        stopifnot(identical(i$ind, dat.w[[1]]$ind))
    }
    
    res <- mapply(function(dat.weight, dat.q){
        res <- dat.weight[, years.next]*dat.q[, years.next]
        res$ind <- dat.q$ind
        res$ind.full <- dat.q$ind.full
        return(res)
    }, dat.w, dat.qq, SIMPLIFY = FALSE)

    res <-  lapply(res, function(x)
        do.call("rbind", lapply(split(x, x$ind), function(x.inner){
            temp <- data.frame(t(colSums(x.inner[, years.next])))
            temp$ind <- x.inner$ind[1]
            return(temp)
        }))
        )
    return(res)
}

q.gap.agg <- sum.industry(weight.input.detail, q.gap.detail)

for (i in c(q.gap.agg, weight.input.agg.raw)){
    stopifnot(identical(i$ind, c("bts", "g", "hts")))
}

res.input.growth <- mapply(function(dat.weight, dat.q){
    res <- dat.weight[, years.next]*dat.q[, years.next]
    res$ind <- c("bts", "g", "hts")
    return(res)
}, weight.input.agg.raw, q.gap.agg, SIMPLIFY = FALSE)

total.input.growth <- res.input.growth[[1]]
for (i in 2:length(res.input.growth)){
    total.input.growth[, years.next] <- total.input.growth[, years.next] + res.input.growth[[i]][, years.next]
}


## 3. Now calculate sectoral TFP growth

## Quantity of GO growth.
## Construct weight
x.go.agg.long <- lapply(x.go.agg, function(x) pivot_longer(x, cols = all_of(years.x), names_to = "year", values_to = "total"))

x.go.detail.long <- lapply(list.go["x.go"], function(x) pivot_longer(x, cols = all_of(years.x), names_to = "year", values_to = "value"))

weight.go.detail.raw <- get.weight.inner(x.go.agg.long, x.go.detail.long)

table(test.sum.weight.cols(weight.go.detail.raw, "g", years.x))

weight.go.detail <- get.weight.torn(weight.go.detail.raw, c("ind", "ind.full"))

table(test.sum.weight.cols(weight.go.detail, "g", years.next))

## Growth of each sector
q.gap.go.detail <- get.log.q.gap(list.go["q.go"], c("ind", "ind.full"))

identical(q.gap.go.detail[[1]]$ind.full, weight.go.detail[[1]]$ind.full)

weight.go.detail <- lapply(weight.go.detail, function(x) x[match(q.gap.go.detail[[1]]$ind.full, x$ind.full),])

q.gap.go <- sum.industry(weight.go.detail, q.gap.go.detail)


## Calculate final statistics
identical(q.gap.go$ind, q.gap.agg$ind)
tfp.growth <- q.gap.go[[1]][, years.next] - total.input.growth[, years.next]
tfp.growth$ind <- total.input.growth$ind

## Calculate TFPs
final.res <- data.frame(ind = tfp.growth$ind, X1987 = 1)
for (i in 1988:2020){
    final.res[, paste0("X", i)] <- final.res[, paste0("X", i-1)]*exp(tfp.growth[, paste0("X", i)])
}

norm.95 <- final.res$X1995
final.res[, years.x] <- final.res[, years.x]/norm.95

write.csv(final.res, "../../output/cleaned_data/us_prod.csv", row.names = FALSE)
