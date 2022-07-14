## Extract GCM results of interest, write TAF output tables

## Before: gcm_results.RData (model)
## After:  gcm_agepred.csv, gcm_coefs.csv, gcm_curve.csv,
##         gcm_residuals.csv (output)

library(TAF)
library(TMB)

mkdir("output")

## Read model results
load("model/gcm_results.RData")

## Extract coefficients
coefs <- as.data.frame(objreport[c("Linf", "K", "rmax")])

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
agepred <- data.frame(ageRel=objreport$age, ageRec=objreport$age+objreport$tal,
                      lenRel=objreport$Lrel_obs, lenRec=objreport$Lrec_obs)

## Residuals
res <- data.frame(Component=c(rep("TagRel", nrow(agepred)),
                              rep("TagRec", nrow(agepred))),
                  Age=c(agepred$ageRel, agepred$ageRec),
                  Length=c(agepred$lenRel, agepred$lenRec),
                  Length_hat=c(objreport$Lrel_pred, objreport$Lrec_pred))
res$Residual <- res$Length - res$Length_hat

## Export tables
write.taf(agepred, "output/gcm_agepred.csv")
write.taf(coefs, "output/gcm_coefs.csv")
write.taf(curve, "output/gcm_curve.csv")
write.taf(res, "output/gcm_residuals.csv")
