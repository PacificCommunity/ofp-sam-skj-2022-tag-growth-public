## Extract GCM_oto results of interest, write TAF output tables

## Before: gcm_oto_results.RData (model)
## After:  gcm_oto_agepred.csv, gcm_coefs.csv, gcm_otoÖconv.csv,
##         gcm_oto_curve.csv, gcm_oto_otofit.csv, gcm_oto_residuals.csv (output)

library(TAF)
library(TMB)

mkdir("output")

## Read model results
load("model/gcm_oto_results.RData")

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
otofit <- data.frame(Age=objreport$Aoto,
                     Length=objreport$Loto,
                     Length_hat=objreport$Loto_pred,
                     Residual=objreport$Loto-objreport$Loto_pred)

## Residuals
res <- data.frame(Component=c(rep("TagRel", nrow(agepred)),
                              rep("TagRec", nrow(agepred)),
                              rep("Otoliths", nrow(otofit))),
                  Age=c(agepred$ageRel, agepred$ageRec, otofit$Age),
                  Length=c(agepred$lenRel, agepred$lenRec, otofit$Length),
                  Length_hat=c(objreport$Lrel_pred, objreport$Lrec_pred,
                               otofit$Length_hat))
res$Residual <- res$Length - res$Length_hat

# Convergence
conv <- as.data.frame(as.list(unlist(opt[-1])))
conv$pdHess <- sdreport$pdHess

## Export tables
write.taf(agepred, "output/gcm_oto_agepred.csv")
write.taf(coefs, "output/gcm_oto_coefs.csv")
write.taf(conv, "output/gcm_oto_conv.csv")
write.taf(curve, "output/gcm_oto_curve.csv")
write.taf(otofit, "output/gcm_oto_otofit.csv")
write.taf(res, "output/gcm_oto_residuals.csv")
