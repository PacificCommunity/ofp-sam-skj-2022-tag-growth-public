## Prepare GCM plots and tables for report

## Before: vonbert_curve.csv, vonbert_fit.csv (output)
## After:  vonbert.pdf (report)

library(TAF)

mkdir("report")

curve <- read.taf("output/vonbert_curve.csv")
fit <- read.taf("output/vonbert_fit.csv")

pdf("report/vonbert.pdf", width=12, height=4)
par(mfrow=c(1,3))
plot(Length~Age, fit, xlim=c(0,4), ylim=c(20,80),
     xlab="Age (yr)", ylab="Length (cm)")
lines(Lhat~Age, curve, subset=Age>=0 & Age<=4)

black <- "#00000040"
plot(NA, xlim=c(0,10), ylim=c(0,100), xlab="Age (yr)", ylab="Length (cm)")
points(Length~Age, fit, pch=16, cex=0.6, col=black)
lines(curve)

plot(NA, xlim=c(0,4), ylim=c(-16,16),
     xlab="Age (yr)", ylab="Length residual (cm)")
points(Residual~Age, fit)
abline(h=0)
dev.off()
