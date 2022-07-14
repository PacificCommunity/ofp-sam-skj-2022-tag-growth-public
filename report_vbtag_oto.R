## Prepare VBtag_oto plots and tables for report

## Before: otoliths.csv (data), vbtag_oto_agepred.csv, vbtag_oto_coefs.csv,
##         vbtag_oto_conv.csv, vbtag_oto_curve.csv,
##         vbtag_oto_residuals.csv (output)
## After:  vbtag_oto.pdf (report)

library(TAF)
library(areaplot)      # confplot
source("utilities.R")  # plot.conv

mkdir("report")

agepred <- read.taf("output/vbtag_oto_agepred.csv")
coefs <- read.taf("output/vbtag_oto_coefs.csv")
conv <- read.taf("output/vbtag_oto_conv.csv")
curve <- read.taf("output/vbtag_oto_curve.csv")
oto <- read.taf("data/otoliths.csv")
res <- read.taf("output/vbtag_oto_residuals.csv")

pdf("report/vbtag_oto.pdf", width=12, height=4)
par(mfrow=c(1,4))
plot(NA, xlim=c(0,4), ylim=c(20,80), xlab="Age (yr)", ylab="Length (cm)")
title(main=paste("Linf =", round(coefs$Linf)))
confplot(curve[curve$Year<=4,c("Year", "loPred", "upPred")], col="lightgray",
         add=TRUE)
points(lenRel~ageRel, data=agepred, col=4)
points(lenRec~ageRec, data=agepred, col=2)
lines(Lhat~Year, curve, subset=Year>=0 & Year<=4)
points(oto[1:2], col="black")
## lines(c(1.0, 1.0+14/365), c(30, 30))  # fortnight

blue <- paste0(palette()[4],"40")
red <- paste0(palette()[2],"40")
black <- "#00000040"
plot(NA, xlim=c(0,10), ylim=c(0,100), xlab="Age (yr)", ylab="Length (cm)")
title(main=paste("k =", round(coefs$k, 2)))
confplot(curve[c("Year", "loPred", "upPred")], col="lightgray", add=TRUE)
## sigma could be a function of length (a=30 cm, b=60 cm), instead of age
points(lenRel~ageRel, data=agepred, pch=16, cex=0.6, col=blue)
points(lenRec~ageRec, data=agepred, pch=16, cex=0.6, col=red)
points(oto[1:2], pch=16, cex=0.6, col=black)
lines(Lhat~Year, curve)

plot(NA, xlim=c(0,4), ylim=c(-16,16),
     xlab="Age (yr)", ylab="Length residual (cm)")
title(main=paste("t0 =", round(coefs$t0, 2)))
points(Residual~Age, res, subset=Component=="TagRel", col=4)
points(Residual~Age, res, subset=Component=="TagRec", col=2)
points(Residual~Age, res, subset=Component=="Otoliths", col="black")
abline(h=0)

plot.conv(conv)
dev.off()

## plot(Length~Age, res)
## points(Length~Age, res, subset=!Inside95band, pch=16, col=7)
