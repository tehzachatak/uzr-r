# Libraries
library(data.table)

# Import data
uzr.table <- read.csv("~/projects/uzr-r/data/fielding_2002_2016-05-03.csv")
# Subset data
uzr.table <- uzr.table[, c(27, 1:5, 20:25)]
# Put data into correct forms
uzr.table$Pos <- factor(Pos, levels = c("P", "C", "1B", "2B", "3B", "SS", 
                                        "LF", "CF", "RF"))
uzr.table$Name <- as.character(uzr.table$Name)

# Fix inning suffixes
uzr.table$inn.round <- round(Inn, 0)
uzr.table$diff <- uzr.table$Inn - uzr.table$inn.round
# Note that this is a highly inelegant solution that produces ugly ass data.
# It works, but I would like to make it clean in the future.
uzr.table$suff <- uzr.table$diff * 1/3 * 10
uzr.table$Inn <- uzr.table$inn.round + uzr.table$suff
uzr.table <- uzr.table[, c(1:12)]
# Express runs stats as rates
uzr.table$arm.1458 <- uzr.table$ARM/uzr.table$Inn*1458
uzr.table$dpr.1458 <- uzr.table$DPR/uzr.table$Inn*1458
uzr.table$rngr.1458 <- uzr.table$RngR/uzr.table$Inn*1458
uzr.table$errr.1458 <- uzr.table$ErrR/uzr.table$Inn*1458
uzr.table$uzr.1458 <- uzr.table$UZR/uzr.table$Inn*1458
# Generate recency weight
uzr.table$rec.wgt <- 5*0.8^(2016-uzr.table$Season)

# Weighted average UZR/150
# So close, yet so far away... I want to compute a weighted mean of 
# UZR.150 (and .1458 vars), weighted by rec.wgt * Inn,
# across the subgroups defined by playerid, Name, Pos
# Then I would get to the correctly weighted, unregressed estimate

# Next, develop regression parameters
# Aggregate innings for regression
inn.table <- aggregate(uzr.table$Inn, 
                       by=list(playerid=uzr.table$playerid, Name=uzr.table$Name, 
                               Pos=uzr.table$Pos), sum, na.rm=TRUE)
inn.table$inn <- inn.table$x
inn.table$x <- NULL
# Generate regression rate
inn.table$reg.rate <- inn.table$inn/3500
# Force to 1.0 if greater than 1
inn.table[inn.table$reg.rate > 1, 5] <- 1