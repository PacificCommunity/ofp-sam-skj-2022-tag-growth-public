## Prepare GCM_oto plots and tables for report

## Before: gcm_oto_agepred.csv, gcm_oto_coefs.csv, gcm_oto_conv.csv,
##         gcm_oto_curve.csv, gcm_oto_otofit.csv, gcm_oto_residuals.csv (output)
## After:  gcm_oto.pdf (report)

library(TAF)
library(areaplot)      # confplot
source("utilities.R")  # plot.conv

mkdir("report")

agepred <- read.taf("output/gcm_oto_agepred.csv")
coefs <- read.taf("output/gcm_oto_coefs.csv")
conv <- read.taf("output/gcm_oto_conv.csv")
curve <- read.taf("output/gcm_oto_curve.csv")
oto <- read.taf("output/gcm_oto_otofit.csv")
res <- read.taf("output/gcm_oto_residuals.csv")

pdf("report/gcm_oto.pdf", width=12, height=4)
par(mfrow=c(1,4))
plot(NA, xlim=c(0,4), ylim=c(20,80), xlab="Age (yr)", ylab="Length (cm)")
title(main=paste("Linf =", round(coefs$Linf)))
points(lenRel~ageRel, data=agepred, col=4)
points(lenRec~ageRec, data=agepred, col=2)
lines(Lhat~Year, curve, subset=Year>=0 & Year<=4)
points(oto[1:2], col="black")
## lines(c(1.0, 1.0+14/365), c(30, 30))  # fortnight

blue <- paste0(palette()[4],"40")
red <- paste0(palette()[2],"40")
black <- "#00000040"
plot(NA, xlim=c(0,10), ylim=c(0,100), xlab="Age (yr)", ylab="Length (cm)")
title(main=paste("k =", round(coefs$K, 2)))
confplot(curve[c("Year", "Lower", "Upper")],
         xlab="Age (yr)", ylab="Length (cm)", add=TRUE)
points(lenRel~ageRel, data=agepred, pch=16, cex=0.6, col=blue)
points(lenRec~ageRec, data=agepred, pch=16, cex=0.6, col=red)
points(oto[1:2], pch=16, cex=0.6, col=black)
lines(Lhat~Year, curve)

plot(NA, xlim=c(0,4), ylim=c(-16,16),
     xlab="Age (yr)", ylab="Length residual (cm)")
title(main=paste("rmax =", round(coefs$rmax)))
points(Residual~Age, res, subset=Component=="TagRel", col=4)
points(Residual~Age, res, subset=Component=="TagRec", col=2)
points(Residual~Age, res, subset=Component=="Otoliths", col="black")
abline(h=0)

plot.conv(conv)
dev.off()
