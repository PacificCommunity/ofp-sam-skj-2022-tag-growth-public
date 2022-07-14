## Prepare R2 table for report

## Before: gcm_oto_residuals.csv, vbtag_oto_residuals.csv (data)
## After:  r2.csv (report)

library(TAF)

mkdir("report")

gcm <- read.taf("output/gcm_oto_residuals.csv")
vbtag <- read.taf("output/vbtag_oto_residuals.csv")

## Calculate R2 for Gcm_Oto
gcm.rel <- gcm$Length[gcm$Component=="TagRel"]
gcm.rec <- gcm$Length[gcm$Component=="TagRec"]
gcm.oto <- gcm$Length[gcm$Component=="Otoliths"]
gcm.total <- sum((gcm.rel-mean(gcm.rel))^2) +
  sum((gcm.rec-mean(gcm.rec))^2) +
  sum((gcm.oto-mean(gcm.oto))^2)
gcm.remaining <- sum(gcm$Residual^2)
gcm.explained <- gcm.total - gcm.remaining
gcm.r2 <- gcm.explained / gcm.total

## Calculate R2 for VBtag_oto
vbtag.rel <- vbtag$Length[vbtag$Component=="TagRel"]
vbtag.rec <- vbtag$Length[vbtag$Component=="TagRec"]
vbtag.oto <- vbtag$Length[vbtag$Component=="Otoliths"]
vbtag.total <- sum((vbtag.rel-mean(vbtag.rel))^2) +
  sum((vbtag.rec-mean(vbtag.rec))^2) +
  sum((vbtag.oto-mean(vbtag.oto))^2)
vbtag.remaining <- sum(vbtag$Residual^2)
vbtag.explained <- vbtag.total - vbtag.remaining
vbtag.r2 <- vbtag.explained / vbtag.total

## Create table
r2 <- data.frame(GCM_oto=gcm.r2, VBtag_oto=vbtag.r2)

## Export table
write.taf(r2, dir="report")
