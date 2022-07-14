## Preprocess tagging data, write TAF data tables

## Before: JP_tagging_modified_16_June.csv (bootstrap/data),
##         PTTP_shimizu.csv (bootstrap/data/sql)
## After:  tags_jp.csv, tags_shimizu.csv (data)

library(TAF)

mkdir("data")

## Read original data
jp <- read.taf("bootstrap/data/JP_tagging_modified_16_June.csv")
if(names(jp)[1]=="") jp <- jp[-1]
shi <- read.taf("bootstrap/data/sql/PTTP_shimizu.csv")

## Alias jp column names
jp$lenRel <- jp$Rellen
jp$lenRec <- jp$RECFL

## Calculate days at liberty (jp)
jp$libertyDays <- as.integer(
  as.Date(ISOdate(jp$Recyear,jp$Recmonth,jp$Recday)) -
  as.Date(ISOdate(jp$Relyear,jp$Relmonth,jp$Relday)))
jp$libertyYears <- jp$libertyDays / 365

## Calculate days and years at liberty (shi)
shi$RelDate <- as.Date(shi$sch_date)
shi$RecDate <- as.Date(shi$catchdate)
shi$libertyDays <- as.integer(shi$RecDate - shi$RelDate)
shi$libertyYears <- shi$libertyDays / 365

## Calculate growth (jp)
jp$lenDiff <- jp$lenRec - jp$lenRel
jp$lenAvg <- (jp$lenRel + jp$lenRec) / 2
jp$growth1 <- jp$lenDiff / jp$libertyDays
jp$growth30 <- jp$growth1 * 30
jp$growth365 <- jp$growth1 * 365

## Calculate growth (shi)
shi$lenDiff <- shi$lenRec - shi$lenRel
shi$lenAvg <- (shi$lenRec + shi$lenRel) / 2
shi$growth1 <- shi$lenDiff / shi$libertyDays
shi$growth30 <- shi$growth1 * 30
shi$growth365 <- shi$growth1 * 365

## Filter (jp)                                    # 339
jp <- subset(jp, jp$METHODraw == "Measured FL")   # 339
jp <- subset(jp, !is.na(jp$RECFL))                # 339
jp <- subset(jp, jp$libertyDays >= 30)            # 339
jp <- subset(jp, jp$libertyDays <= 3*365)         # 339
jp <- subset(jp, jp$growth1 >= 0)                 # 307
jp <- subset(jp, jp$growth1 <= 0.2)               # 304
jp <- subset(jp, jp$Reliability_Recdate == 1)     # 241
jp <- subset(jp, jp$Reliability_Recfl >= 1)       # 241

## Filter (shi)                                   # 422
shi <- subset(shi, shi$sp_id == "S")              # 393
shi <- subset(shi, shi$libertyDays >= 30)         # 379
shi <- subset(shi, shi$libertyDays < 3*365)       # 379
shi <- subset(shi, shi$growth1 >= 0)              # 379
shi <- subset(shi, shi$growth1 <= 0.2)            # 377
shi <- subset(shi, shi$RecDate_reliability <= 3)  # 250
shi <- subset(shi, shi$Rel_len_reliability == 1)  # 247

## Write tables
write.taf(jp, "data/tags_jp.csv")
write.taf(shi, "data/tags_shi.csv")
