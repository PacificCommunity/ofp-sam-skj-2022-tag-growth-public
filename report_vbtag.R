## Prepare VBtag plots and tables for report

## Before: otoliths.csv (data), vbtag_agepred.csv, vbtag_curve.csv,
##         vbtag_residuals.csv (output)
## After:  vbtag.pdf (report)

library(TAF)
library(areaplot)  # confplot

mkdir("report")

agepred <- read.taf("output/vbtag_agepred.csv")
curve <- read.taf("output/vbtag_curve.csv")
oto <- read.taf("data/otoliths.csv")
res <- read.taf("output/vbtag_residuals.csv")

pdf("report/vbtag.pdf", width=12, height=4)
par(mfrow=c(1,3))
plot(NA, xlim=c(0,4), ylim=c(20,80), xlab="Age (yr)", ylab="Length (cm)")
points(lenRel~ageRel, data=agepred, col=4)
points(lenRec~ageRec, data=agepred, col=2)
lines(Lhat~Year, curve, subset=Year>=0 & Year<=4)
points(oto[1:2], col="gray")
## lines(c(1.0, 1.0+14/365), c(30, 30))  # fortnight

blue <- paste0(palette()[4],"40")
red <- paste0(palette()[2],"40")
gray <- "#bbbbbb40"
plot(NA, xlim=c(0,10), ylim=c(0,100), xlab="Age (yr)", ylab="Length (cm)")
confplot(curve[c("Year", "Lower", "Upper")],
         xlab="Age (yr)", ylab="Length (cm)", add=TRUE)
points(lenRel~ageRel, data=agepred, pch=16, cex=0.6, col=blue)
points(lenRec~ageRec, data=agepred, pch=16, cex=0.6, col=red)
points(oto[1:2], pch=16, cex=0.6, col=gray)
lines(Lhat~Year, curve)

plot(NA, xlim=c(0,4), ylim=c(-16,16),
     xlab="Age (yr)", ylab="Length residual (cm)")
points(Residual~Age, res, subset=Component=="TagRel", col=4)
points(Residual~Age, res, subset=Component=="TagRec", col=2)
abline(h=0)
dev.off()
