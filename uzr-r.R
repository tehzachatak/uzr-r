# Libraries
library(data.table)

# Import data
uzr.table <- read.csv("~/Personal Projects/uzr-r/data/fielding_2002_2016-05-03.csv")
# Subset data
uzr.table <- uzr.table[, c(27, 1:5, 20:25)]
# Put data into correct forms
uzr.table$pos <- factor(uzr.table$pos, levels = c("P", "C", "1B", "2B", "3B", "SS", 
                                        "LF", "CF", "RF"))
uzr.table$name <- as.character(uzr.table$name)

# Fix inning suffixes
uzr.table$inn.round <- round(uzr.table$inn, 0)
uzr.table$diff <- uzr.table$inn - uzr.table$inn.round
# Note that this is a highly inelegant solution
uzr.table$suff <- uzr.table$diff * 1/3 * 10
uzr.table$inn <- uzr.table$inn.round + uzr.table$suff
uzr.table <- uzr.table[, c(1:12)]
# Express runs stats as rates
uzr.table$arm.1458 <- uzr.table$arm/uzr.table$inn*1458
uzr.table$dpr.1458 <- uzr.table$dpr/uzr.table$inn*1458
uzr.table$rngr.1458 <- uzr.table$rngr/uzr.table$inn*1458
uzr.table$errr.1458 <- uzr.table$errr/uzr.table$inn*1458
uzr.table$uzr.1458 <- uzr.table$uzr/uzr.table$inn*1458
# Generate recency weight
uzr.table$rec.wgt <- 5*0.8^(2016-uzr.table$season)

# Next, develop regression parameters
# Aggregate innings for regression
inn.table <- aggregate(uzr.table$inn, 
                       by=list(playerid=uzr.table$playerid, name=uzr.table$name, 
                               pos=uzr.table$pos), sum, na.rm=TRUE)
inn.table$inn <- inn.table$x
inn.table$x <- NULL
# Generate regression rate
inn.table$reg.rate <- inn.table$inn/3500
# Force to 1.0 if greater than 1
inn.table[inn.table$reg.rate > 1, 5] <- 1

# Weighted average UZR/150
# Make uzr.table a data.table
setDT(uzr.table)
# Set key
setkey(uzr.table, playerid, pos)
fld.est <- uzr.table[, .(uzr150=weighted.mean(uzr150,rec.wgt*inn)),
                     by = .(playerid, pos, name)]
# Join
setDT(inn.table)
setkey(inn.table, playerid, pos, name)
fld.est <- fld.est[inn.table]
# Regress
fld.est$uzr150.reg <- fld.est$uzr150*fld.est$reg.rate