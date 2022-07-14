## Extract VBtag results of interest, write TAF output tables

## Before: vbtag_results.RData (model)
## After:  vbtag_agepred.csv, vbtag_coefs.csv, vbtag_curve.csv,
##         vbtag_residuals.csv, vbtag_sigma.csv (output)

library(TAF)
library(TMB)

mkdir("output")

## Read model results
load("model/vbtag_results.RData")

## Extract coefficients
coefs <- as.data.frame(objreport[c("Linf", "k", "t0", "sigma_a", "sigma_b")])

## Calculate growth curve confidence bands
curve <- summary(sdreport)
curve <- curve[rownames(curve)=="curve",]
curve <- data.frame(Day=seq_len(nrow(curve))-1L,
                    Year=(seq_len(nrow(curve))-1)/365,
                    Lhat=curve[,"Estimate"],
                    SE=curve[,"Std. Error"])
curve$Lower <- curve$Lhat + qnorm(0.025) * curve$SE
curve$Upper <- curve$Lhat + qnorm(0.975) * curve$SE

## Model estimates
agepred <- data.frame(ageRel=objreport$age,
                      ageRec=objreport$age+objreport$liberty,
                      lenRel=objreport$Lrel, lenRec=objreport$Lrec)

## Residuals
res <- data.frame(Component=c(rep("TagRel", nrow(agepred)),
                              rep("TagRec", nrow(agepred))),
                  Age=c(agepred$ageRel, agepred$ageRec),
                  Length=c(agepred$lenRel, agepred$lenRec),
                  Length_hat=c(objreport$Lrel_hat, objreport$Lrec_hat))
res$Residual <- res$Length - res$Length_hat

## Empirical sigma
midpoint <- mean(c(objreport$a, objreport$b))
empiricalYoung <- sd(res$Residual[res$Age < midpoint])
empiricalOld <- sd(res$Residual[res$Age > midpoint])
sigma <- data.frame(empiricalYoung, empiricalOld)

## Export tables
write.taf(agepred, "output/vbtag_agepred.csv")
write.taf(coefs, "output/vbtag_coefs.csv")
write.taf(curve, "output/vbtag_curve.csv")
write.taf(res, "output/vbtag_residuals.csv")
write.taf(sigma, "output/vbtag_sigma.csv")
