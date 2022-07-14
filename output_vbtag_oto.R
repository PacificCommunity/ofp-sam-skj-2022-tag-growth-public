## Extract VBtag_oto results of interest, write TAF output tables

## Before: vbtag_oto_results.RData (model)
## After:  vbtag_oto_agepred.csv, vbtag_oto_coefs.csv, vbtag_oto_conv.csv,
##         vbtag_oto_curve.csv, vbtag_oto_residuals.csv,
##         vbtag_oto_sigma.csv (output)

library(TAF)
library(TMB)

mkdir("output")

## Read model results
load("model/vbtag_oto_results.RData")

## Extract coefficients
coefs <- as.data.frame(objreport[c("Linf", "k", "t0", "sigma_a", "sigma_b")])

## Calculate prediction sigma by age
a <- obj$env$data$a
b <- obj$env$data$b
sigma.a <- coefs[["sigma_a"]]
sigma.b <- coefs[["sigma_b"]]
sigma.slope = (log(sigma.b) - log(sigma.a)) / (b - a)
sigma.intercept = log(sigma.a) - a * sigma.slope

## Calculate growth curve confidence bands
curve <- summary(sdreport)
curve <- curve[rownames(curve)=="curve",]
curve <- data.frame(Day=seq_len(nrow(curve))-1L,
                    Year=(seq_len(nrow(curve))-1)/365,
                    Lhat=curve[,"Estimate"],
                    SE=curve[,"Std. Error"])
curve$Lower <- curve$Lhat + qnorm(0.025) * curve$SE
curve$Upper <- curve$Lhat + qnorm(0.975) * curve$SE
curve$sigmaPred <- exp(sigma.intercept + curve$Year * sigma.slope)
curve$loPred <- curve$Lhat + qnorm(0.025) * curve$sigmaPred
curve$upPred <- curve$Lhat + qnorm(0.975) * curve$sigmaPred

## Model estimates
agepred <- data.frame(ageRel=objreport$age,
                      ageRec=objreport$age+objreport$liberty,
                      lenRel=objreport$Lrel, lenRec=objreport$Lrec)
otofit <- data.frame(Age=objreport$Aoto,
                     Length=objreport$Loto,
                     Length_hat=objreport$Loto_hat,
                     Residual=objreport$Loto-objreport$Loto_hat)

## Residuals
res <- data.frame(Component=c(rep("TagRel", nrow(agepred)),
                              rep("TagRec", nrow(agepred)),
                              rep("Otoliths", nrow(otofit))),
                  Age=c(agepred$ageRel, agepred$ageRec, otofit$Age),
                  Length=c(agepred$lenRel, agepred$lenRec, otofit$Length),
                  Length_hat=c(objreport$Lrel_hat, objreport$Lrec_hat,
                               otofit$Length_hat))
res$Residual <- res$Length - res$Length_hat

## Prediction band coverage
sigmaPred <- exp(sigma.intercept + res$Age * sigma.slope)
loPred <- res$Length_hat + qnorm(0.025) * sigmaPred
upPred <- res$Length_hat + qnorm(0.975) * sigmaPred
inside <- res$Length >= loPred & res$Length <= upPred
res$Inside95band <- inside

## Convergence
conv <- as.data.frame(as.list(unlist(opt[-1])))
conv$pdHess <- sdreport$pdHess

## Empirical sigma
midpoint <- mean(c(objreport$a, objreport$b))
empiricalYoung <- sd(res$Residual[res$Age < midpoint])
empiricalOld <- sd(res$Residual[res$Age > midpoint])
sigma <- data.frame(empiricalYoung, empiricalOld)

## Export tables
write.taf(agepred, "output/vbtag_oto_agepred.csv")
write.taf(coefs, "output/vbtag_oto_coefs.csv")
write.taf(conv, "output/vbtag_oto_conv.csv")
write.taf(curve, "output/vbtag_oto_curve.csv")
write.taf(otofit, "output/vbtag_oto_otofit.csv")
write.taf(res, "output/vbtag_oto_residuals.csv")
write.taf(sigma, "output/vbtag_oto_sigma.csv")
