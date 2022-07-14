## Extract von Bertalanffy (otolith) results, write TAF output tables

## Before: vonbert_results.RData (model)
## After:  vonbert_coefs.csv, vonbert_curve.csv, vonbert_fit.csv (output)

library(TAF)

mkdir("output")

load("model/vonbert_results.RData")

## Extract coefficients
Linf <- opt$par["Linf"]
k <- opt$par["k"]
t0 <- opt$par["t0"]
coefs <- as.data.frame(as.list(opt$par))

## Calculate curve
a <- seq(0, 10, by=0.01)
curve <- data.frame(Age=a, Lhat=Linf*(1-exp(-k*(a-t0))))

## Rename model fit columns
fit <- report$fit
names(fit) <- c("Age", "Length", "Length_hat", "Residual")

## Export tables
write.taf(coefs, "output/vonbert_coefs.csv")
write.taf(curve, "output/vonbert_curve.csv")
write.taf(fit, "output/vonbert_fit.csv")
