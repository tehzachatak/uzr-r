# Libraries
library(data.table)
# Import data & make a DT
uzr.table <- read.csv("data/fielding_2002_2016-05-03.csv")
setDT(uzr.table)
setkey(uzr.table, playerid, name, pos)
# Put data into correct forms (in base - can I use data.table?)
uzr.table$pos <- factor(
  uzr.table$pos, levels = c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"))
uzr.table$name <- as.character(uzr.table$name)
# Fix inning suffixes
uzr.table$inn.round <- round(uzr.table$inn, 0)
uzr.table$diff <- uzr.table$inn - uzr.table$inn.round
# Note that this is a highly inelegant solution
uzr.table$suff <- uzr.table$diff * 1/3 * 10
uzr.table$inn <- uzr.table$inn.round + uzr.table$suff
# Clean up/subset data (in data.table)
uzr.table <- 
  uzr.table[, .(playerid, name, pos, season, 
                team, inn, arm, dpr, rngr, errr, uzr, uzr150)]
# Express runs stats as rates
uzr.table[, arm := arm/inn*1458]
# Generate recency weight
uzr.table$rec.wgt <- 5*0.8^(2016-uzr.table$season)
# Next, develop regression parameters
inn.table <- uzr.table[, .(inn=sum(inn)), by = .(playerid, pos, name)]
# Generate regression rate
inn.table$reg.rate <- inn.table$inn/3500
# Force to 1.0 if greater than 1
inn.table[inn.table$reg.rate > 1, 5] <- 1
# Weighted average UZR/150
fld.est <- uzr.table[, .(uzr150=weighted.mean(uzr150,rec.wgt*inn), 
                         arm=weighted.mean(arm, rec.wgt*inn),
                         dpr=weighted.mean(dpr, rec.wgt*inn), 
                         rngr=weighted.mean(rngr, rec.wgt*inn),
                         errr=weighted.mean(errr, rec.wgt*inn),
                         uzr=weighted.mean(uzr, rec.wgt*inn)),
                     by = .(playerid, pos, name)]
# Join
setkey(inn.table, playerid, name, pos)
setkey(fld.est, playerid, name, pos)
fld.est <- fld.est[inn.table]
# Generate years
year.table <- uzr.table[, .(rec.seas=max(season)), by = .(playerid, pos, name)]
setkey(year.table, playerid, name, pos)
fld.est <- fld.est[year.table]
# Regress (I should be able to do this so much more concisely, but how?)
fin.est <- 
  fld.est[, .(uzr150 = uzr150*reg.rate, arm = arm*reg.rate, 
              dpr = dpr*reg.rate, rngr = rngr*reg.rate, 
              errr = errr*reg.rate, uzr = uzr*reg.rate), 
          by = .(playerid, pos, name, rec.seas)]
# Drop positions we don't have info for
fin.est <- fin.est[pos != "P" & pos != "C"]
# Drop records with no UZR estimate
fin.est <- fin.est[uzr150 != "NA"]
# Drop unnecessary tables
rm(fld.est, inn.table, uzr.table)
# Keep only 2016 records
est.2016 <- fin.est[rec.seas == 2016]