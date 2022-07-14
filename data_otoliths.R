## Preprocess otolith data, write TAF data tables

## Before: Leroy_oto_tag_estimates.csv (bootstrap/data),
## After:  otoliths.csv (data)

library(TAF)

mkdir("data")

## Read original data
oto <- read.taf("bootstrap/data/Leroy_oto_tag_estimates.csv")

## Filter rows and columns
oto <- oto[oto$source == "otolith", c("age_yrs","FL_cm")]

## Rename columns
names(oto) <- c("age", "len")

## Write table
write.taf(oto, "data/otoliths.csv")
