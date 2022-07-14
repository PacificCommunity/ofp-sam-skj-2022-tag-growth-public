#############################################################
## SKJ growth background analyses for 2022 stock assessment
#############################################################

# Created by: J. Macdonald
# Created on: 09/05/22
# Last updated: 15/06/22

# Code associated with the background information paper (IP) supporting the 2022 SKJ stock assessment.

# The paper has two main components:
# 1.	Review and meta-analysis - a compilation and analysis of all published growth functions derived from previous studies 
#     in the Pacific, including those used in five most recent WCPFC SKJ assessment models. 
#     From this, we derive the first alternative growth curve: 'WCPO meta'.
# 2.	Otolith and tag-recapture analysis - exploratory analyses and model fitting to otolith-based age estimates and tag-recapture 
#     data available from all previous tagging campaigns (i.e. PTTP/RTTP, Japanese tagging experiments). 
#     From this, we derive the second alternative growth curve: 'Leroy 2000 & Jdat'.



###################################
# Review and meta-analysis
###################################

# Fit VB curves for SKJ using all available growth models from the Pacific Ocean.
# We use the 'Typical' parameterisation of the VB function as defined in the 'FSA' package.
# For the purposes of this analysis, we plot all growth models across the full age range used in MFCL-based SKJ assessments, 
# [i.e. from age 1 (end of first quarter of life at 3 months or 0.25 years old) to 
# age 16 (end of the 16th quarter of life and older at 48+ months or 4+ years old)]. 
# This facilitates comparison with past and current MFCL growth estimates.

library(FSA)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###################################
## VB curves (Pacific)
###################################

# NOTES: Most estimates come from Murua et al. 2017.
# Where t0 is provided in the suppl. tables, we used that in the VB equation; where it wasn't, we set t0=0 following the 
# approach of Murua et al. 2017.

(vb <- vbFuns(param="Typical"))
ages<-seq(0.25,4,by=0.25) # age at quarterly time step

# WCPO, WPO, TWPO-J studies
pdf("All studies.pdf")
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)

lines(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, col="indianred1") # Tanabe et al. 2003
lines(vb(t=ages, Linf=70.7, K=0.64, t0=-0.037)~ages, col="indianred1") # Wang et al. 2010 
lines(vb(t=ages, Linf=76.6, K=0.60, t0=-0.31)~ages, col="indianred1") # Yao 1981
lines(vb(t=ages, Linf=103.6, K=0.302, t0=-0.016)~ages, col="indianred1") # Chi & Yang 1973
lines(vb(t=ages, Linf=74.8, K=0.51, t0=0)~ages, col="indianred1") # Wankowski 1951
lines(vb(t=ages, Linf=61.3, K=1.25, t0=0)~ages, col="indianred1") # Sibert et al. 1983
lines(vb(t=ages, Linf=65.5, K=0.95, t0=0)~ages, col="indianred1") # Josse et al. 1979
lines(vb(t=ages, Linf=60.0, K=0.75, t0=0)~ages, col="indianred1") # Brouard et al. 1984
lines(vb(t=ages, Linf=141.8, K=0.19, t0=0)~ages, col="indianred1") # Kawasaki 1963, in Joseph & Calkins 1969
lines(vb(t=ages, Linf=77.4, K=0.176, t0=-2.569)~ages, col="indianred1") # Ku et al. 2015
lines(vb(t=ages, Linf=81.87449, K=1.09070, t0=-0.20626)~ages, col="indianred1") # Leroy 2000 VB model parameters from fit to all otolith data (n=61) (see 'Alternative growth Option 3' below for model details)
# NOTES: 1. Parameter estimates defined for our model differ markedly from those reported in Leroy 2000 based on n=57 samples (i.e. Linf=62.17, K=2.373, t0=-0.04).
#           We included 4 larger fish between 61 and 69 cm in our case.
#        2. The growth curve for Leroy 2000 presented in Hoyle et al. 2011 differs slightly from that estimated in our model
#           We were unable to ascertain how Hoyle et al. 2011 derived their values, so we decided against including that growth estimate in our comparison here.

# CPO studies
ages<-seq(0.25,4,by=0.25)
lines(vb(t=ages, Linf=102.2, K=0.55, t0=-0.02)~ages, col="indianred1") # Uchiyama & Struhsaker 1981 
lines(vb(t=ages, Linf=85.0, K=0.95, t0=0)~ages, col="indianred1") # Brock 1954, in Joseph & Calkins 1969
lines(vb(t=ages, Linf=92.0, K=0.47, t0=0)~ages, col="indianred1") # Skilman 1981
lines(vb(t=ages, Linf=82.3, K=0.77, t0=0)~ages, col="indianred1") # Rothschild 1966, corrected, in Joseph & Calkins 1969
lines(vb(t=ages, Linf=90.6, K=0.59, t0=0)~ages, col="indianred1") # Rothschild 1966, uncorrected, in Joseph & Calkins 1969

# EPO studies
ages<-seq(0.25,4,by=0.25)
lines(vb(t=ages, Linf=88.1, K=0.43, t0=0)~ages) # Joseph & Calkins 1969 (tagging averaged)
lines(vb(t=ages, Linf=72.9, K=0.83, t0=0)~ages) # Joseph & Calkins 1969 (tagging not averaged)
lines(vb(t=ages, Linf=107.5, K=0.41, t0=0)~ages) # Joseph & Calkins 1969 (length frequency)
lines(vb(t=ages, Linf=75.5, K=0.77, t0=0)~ages) # Sibert et al. 1983
lines(vb(t=ages, Linf=79.1, K=0.64, t0=0)~ages) # Josse et al. 1979
lines(vb(t=ages, Linf=88.5, K=0.66, t0=0)~ages) # Bayliff 1988 (ungrouped)
lines(vb(t=ages, Linf=84.6, K=0.83, t0=0)~ages) # Bayliff 1988 (grouped)
lines(vb(t=ages, Linf=85.1, K=0.44, t0=0)~ages) # Schaefer 1961, in Joseph & Calkins 1969

## Growth curve for diagnostic case in 2022 WCPFC assessment
# Dirichlet multinomial likelihood-based (DM) estimate
DM_data<-read.csv("growth_DM.csv")
ages<-seq(0.25,4,by=0.25)
DM_data$ages<-ages
lines(DM_data$length~DM_data$ages, col="red2", lwd=5, lend="butt")
polygon(c(DM_data$ages,rev(DM_data$ages)),
        c(DM_data$length_lower,rev(DM_data$length_upper)),col=rgb(238,0,0, alpha=75, maxColorValue = 255),border=NA)

## Growth curves from recent WCPFC SKJ assessments
# Vincent et al. 2019
Vincent_data<-read.csv("growth2019_MTV_low.diag.high.csv")
Vincent_data<-Vincent_data[1:16,] 
ages<-seq(0.25,4,by=0.25)
Vincent_data$ages<-ages
lines(Vincent_data$length_diag~Vincent_data$ages, col="dodgerblue2", lwd=2, lty=2) # 2019 diagnostic case
polygon(c(Vincent_data$ages,rev(Vincent_data$ages)),
        c(Vincent_data$length_diag_lower,rev(Vincent_data$length_diag_upper)),col=rgb(24,116,205, alpha=50, maxColorValue = 255),border=NA)
lines(Vincent_data$length_low~Vincent_data$ages, col="dodgerblue2", lwd=2, lty=3) # 2019 GrowthLow
lines(Vincent_data$length_high~Vincent_data$ages, col="dodgerblue2", lwd=2, lty=3) # 2019 GrowthHigh

# Hoyle et al. 2010, 2011, Rice et al. 2014; McKechnie et al. 2016 (these assessments all used the same growth curve for the diagnostic case)
Hoyle_data<-read.csv("growth2010.csv")
Hoyle_data<-Hoyle_data[1:16,] 
ages<-seq(0.25,4,by=0.25)
Hoyle_data$ages<-ages
lines(Hoyle_data$length~Hoyle_data$ages, col="grey85", lty=2, lwd=2)

# Legend
legend(x=2.3, y=42, legend=c("WCPO studies", "EPO studies", "2022 DM", "Hoyle et al. 2010", "Vincent et al. 2019", "2019 GrowthLow", "2019 GrowthHigh"), cex=0.95, 
       lty=c(1,1,1,2,2,3,3), col=c("indianred1", "black", "red2", "grey85", "dodgerblue2", "dodgerblue2",
                                   "dodgerblue2"), lwd=c(1,1,4,2,2,2,2), seg.len = 1.6, bty="n")
dev.off()

###################################
## Alternative growth options
###################################

###################################
## WCPO meta
###################################

# This takes the mean length-at-age value (calculated per quarter) across all previous WCPO growth studies.
# We calculate the 95% quantiles per quarter as a measure of the variability in length-at-age.
# For the purposes of this analysis, we extend all growth models to encompass the age structure of the SKJ assessment,
# spanning age 1 (0.25 years old) to age 16 (4 years and older). 
# We exclude Ku et al. 2015 from the calculations due to lack of confidence in these estimates.
# These steps provide us a first alternative growth curve: 'WCPO meta'.

# Get all data
ages<-seq(0.25,4,by=0.25) # set ages to match MFCL age structure for SKJ assessment
VB_l.at.age_WCPO<-setNames(data.frame(matrix(ncol = 16, nrow = 0)), paste("age", 1:16, sep="")) # Create empty data frame to store length-at-age estimates
VB_l.at.age_WCPO[1,]<-vb(t=ages, Linf=93.6, K=0.43, t0=-0.49) # Tanabe et al. 2003
VB_l.at.age_WCPO[2,]<-vb(t=ages, Linf=70.7, K=0.64, t0=-0.037) # Wang et al. 2010 
VB_l.at.age_WCPO[3,]<-vb(t=ages, Linf=76.6, K=0.60, t0=-0.31) # Yao 1981
VB_l.at.age_WCPO[4,]<-vb(t=ages, Linf=103.6, K=0.302, t0=-0.016) # Chi & Yang 1973
VB_l.at.age_WCPO[5,]<-vb(t=ages, Linf=74.8, K=0.51, t0=0) # Wankowski 1951
VB_l.at.age_WCPO[6,]<-vb(t=ages, Linf=61.3, K=1.25, t0=0) # Sibert et al. 1983
VB_l.at.age_WCPO[7,]<-vb(t=ages, Linf=65.5, K=0.95, t0=0) # Josse et al. 1979
VB_l.at.age_WCPO[8,]<-vb(t=ages, Linf=60.0, K=0.75, t0=0) # Brouard et al. 1984
VB_l.at.age_WCPO[9,]<-vb(t=ages, Linf=141.8, K=0.19, t0=0) # Kawasaki 1963, in Joseph & Calkins 1969
VB_l.at.age_WCPO[10,]<-vb(t=ages, Linf=81.87449, K=1.09070, t0=-0.20626) # Leroy 2000 from our otolith model (see 'Alternative growth Option 3' below)
VB_l.at.age_WCPO[11,]<-vb(t=ages, Linf=102.2, K=0.55, t0=-0.02) # Uchiyama & Struhsaker 1981 
VB_l.at.age_WCPO[12,]<-vb(t=ages, Linf=85.0, K=0.95, t0=0) # Brock 1954, in Joseph & Calkins 1969
VB_l.at.age_WCPO[13,]<-vb(t=ages, Linf=92.0, K=0.47, t0=0) # Skilman 1981
VB_l.at.age_WCPO[14,]<-vb(t=ages, Linf=82.3, K=0.77, t0=0) # Rothschild 1966, corrected, in Joseph & Calkins 1969
VB_l.at.age_WCPO[15,]<-vb(t=ages, Linf=90.6, K=0.59, t0=0) # Rothschild 1966, uncorrected, in Joseph & Calkins 1969

# Get means, medians and 95% quantiles of length-at-age by quarter
VB_output<-data.frame(sapply(VB_l.at.age_WCPO, function(x) quantile(x, probs = c(0.025, 0.50, 0.975)))) # medians and 95% quantiles 
VB_output[4,]=colMeans(VB_l.at.age_WCPO) # get means

# Plot for comparison with '2022 DM' estimates  
ages<-seq(0.25,4,by=0.25) # redefine ages if need be
pdf("DM_meta.pdf")
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)
lines(DM_data$length~DM_data$ages, col="red2", lwd=5, lend="butt") # Mean length-at-age from DM estimate
polygon(c(DM_data$ages,rev(DM_data$ages)),
        c(DM_data$length_lower,rev(DM_data$length_upper)),col=rgb(238,0,0, alpha=75, maxColorValue = 255),border=NA)
lines(y=VB_output[4,], x=ages, col="yellow", lwd=2, lend="butt")
polygon(c(ages,rev(ages)),
        c(VB_output[1,],rev(VB_output[3,])), col=rgb(255,255,0, alpha=50, maxColorValue = 255), border=NA)
legend(x=2.3, y=20, legend=c("2022 DM", "WCPO meta"), cex=0.95, 
       lty=c(1,1), col=c("red2", "yellow"), lwd=c(4,2), seg.len = 1.6, bty="n")
dev.off()

# Distribution checks - distribution of length-at-age estimates by quarter
par(mfrow=c(4,4))
for (i in 1:16){
  hist(VB_l.at.age_WCPO[,i], breaks = 10, xaxt="n", main=i)
}


#######################################
## Otolith and tag-recapture analysis
#######################################

# In a nutshell...
# Fit a 'Typical' VB curve to the otolith age estimates from Leroy 2000.
# Use this as a basis for estimating the age at length of release for the screened PTTP/RTTP and Japanese tag-recapture data.
# With this information, we then calculate the recapture age for each individual by adding time-at-liberty to the release age.
# We then fit a second VB curve to the combined otolith and tag-recapture data. This becomes our 2nd alternative growth estimate.

library(FSA) # load if not already loaded     
library(car)  

##################################
# Otolith data model
##################################

# Read in otolith and tag-recapture data (Leroy 2000)
Leroy_oto_tag<-read.csv("Leroy_oto_tag_estimates.csv")
str(Leroy_oto_tag)
par(mfrow=c(1,1))
plot(FL_cm~age_yrs, data=Leroy_oto_tag, type="p")

# Subset for otolith data only
Leroy_oto<-Leroy_oto_tag[Leroy_oto_tag$source=="otolith",] # NOTE: We aimed to make use of all available empirical data, we use all n=61 fish (up to 69 cm FL), whereas Leroy 2000 used n=57 fish up to 61 cm.
                                                           # Hence the discrepancy in VB parameter estimates.       
points(FL_cm~age_yrs, data=Leroy_oto, type="p", col="gold")

# Fit VB model to otolith data
(vb <- vbFuns(param="Typical"))
f.starts<-list(Linf=81.8741, K=1, t0=-0.2) # get starting values from age qtr model which converged (i.e. Linf=81.8741, K=0.2727, t0=-0.825) and scale K and t0 to yearly values by multiplying by 4.
f.fit <- nls(FL_cm~vb(age_yrs,Linf,K,t0),data=Leroy_oto,start=f.starts)
coef(f.fit)
summary(f.fit)
plot(resid(f.fit)~Leroy_oto$age_yrs) # Plot residuals against age_yrs

# Predict length at age across full range of age estimates in otolith data
predict_l <- function(x) predict(x,newdata=data.frame(age_yrs=age_yrs))
age_yrs <- seq(min(Leroy_oto$age_yrs), max(Leroy_oto$age_yrs), length.out=100)
predict_l(f.fit)

# Generate 95% bootstrapped CIs around mean estimates, and save as a dataframe
f.boot2 <- Boot(f.fit,f=predict_l) 
preds <- data.frame(age_yrs, predict_l(f.fit), confint(f.boot2))
names(preds) <- c("age_yrs","fit","LCI","UCI")

# Make plot of data and predictions
plot(FL_cm~age_yrs, data=Leroy_oto, type="p")
lines(preds$fit~preds$age_yrs, type="l", lwd=2)
polygon(c(preds$age_yrs,rev(preds$age_yrs)),
        c(preds$LCI,rev(preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)

# Plot for comparison with '2022 DM' and 'WCPO meta' estimates  
ages<-seq(0.25,4,by=0.25) # redefine ages if need be
pdf("DM_meta_Leroy.pdf")
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)
lines(DM_data$length~DM_data$ages, col="red2", lwd=5, lend="butt") # Mean length-at-age from DM estimate
polygon(c(DM_data$ages,rev(DM_data$ages)),
        c(DM_data$length_lower,rev(DM_data$length_upper)),col=rgb(238,0,0, alpha=75, maxColorValue = 255),border=NA)
lines(y=VB_output[4,], x=ages, col="yellow", lwd=2, lend="butt")
polygon(c(ages,rev(ages)),
        c(VB_output[1,],rev(VB_output[3,])), col=rgb(255,255,0, alpha=50, maxColorValue = 255), border=NA)
points(FL_cm~age_yrs, data=Leroy_oto, col="black", cex=0.6, pch=24, bg="white") # Overlay Leroy 2000 otolith data. 
lines(preds$fit~preds$age_yrs, type="l", lwd=2, lend="butt") # Mean length-at-age from our VB model fitted to otolith data from Leroy 2000.
polygon(c(preds$age_yrs,rev(preds$age_yrs)),
        c(preds$LCI,rev(preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA) # Bootstrapped 95% CIs around the mean estimates.
lines(vb(t=ages, Linf=81.87449, K=1.09070, t0=-0.20626)~ages, col="black", lty=2, lwd=2, lend="butt") # Extrapolation of Leroy 2000 VB curve across full MFCL age structure for SKJ assessment.
legend(x=2.3, y=35, legend=c("2022 DM", "WCPO meta", "Leroy 2000", "Extrapolated Leroy 2000", "Leroy 2000 data"), cex=0.95, 
       lty=c(1,1,1,2,NA), col=c("red2", "yellow", "black", "black", "black"), lwd=c(4,2,2,2,NA), pch=c(NA,NA,NA,NA,24), 
       pt.bg=c(NA,NA,NA,NA,"white"), seg.len = 1.6, bty="n")
dev.off()

##################################
# Analysis of tag-recapture data
##################################

# Datasets: PTTP/RTTP and JPTP tagging campaigns
# The datasets comprise release and recapture lengths for SKJ, dates of release and recapture, and several other relevant fields.

# Workflow:

# 1. Apply a series of data screening and filtering steps, drawing on information in Appendix A in Eveson et al. 2015, and Eveson et al. 2020. 
#    This was done to provide the highest quality tag-recapture data possible for estimating the growth function.
#    See code by Jemery Day and Arni Magnusson for details.
# 2. Exploratory analyses - plots of growth rate versus average length to determine if growth rates vary with length and age.
#                         - Provides insight into an appropriate functional form for the growth curve.
#    See code by Jemery Day and Arni Magnusson for details.
# 3. Estimate age at a given release length.
#    - We use the growth curve estimated from the Leroy 2000 otolith model fitted above (i.e. f.fit)
#    - We rearrange the Typical VB equation to solve for age 'tr' at a specified length 'Lr' (following Ogle and Isermann 2017)
#    - Estimate release age for each individual.
# 4. Calculate recapture age for each individual by adding time-at-liberty to release age.
# 5. Combine tag-recapture data (both the lengths and estimated ages at release and recapture) with the otolith data.
# 6. Fit a new VB model to this dataset.
# 7. Do this for the PTTP/RTTP and JPTP data separately.
# 8. Plot out results, overlaying the tag-recapture vectors for all individuals.

# Function to compute tr from Typical VBGF (from Ogle and Isermann 2017)
calc_tr <- function(Lr,Linf,K=NULL,t0=NULL) {
  if (length(Linf)==3) {
    K <- Linf[[2]]
    t0 <- Linf[[3]]
    Linf <- Linf[[1]] }
  (log(1-Lr/Linf))/(-K)+t0
}
tr <- calc_tr(62,coef(f.fit)) # Example, where release length = 62 cm, and tr  = age in years, in this case = ~ 1.091 years' old.

############################
## JPTP
############################

# Read in and explore screened data 
jdat <- read.csv("../data/tags_jp.csv") # 304
str(jdat)
summary(jdat$Rellen) # min FL at release is 24 cm
jdat<-jdat[jdat$lenRel>=30 & jdat$lenRel<=69,] # Only retain lengths at release that fall within range of Leroy (2000) lengths.
nrow(jdat) # 302 records
jdat$rel.age<-calc_tr(jdat$Rellen,coef(f.fit))
jdat$rec.age<-jdat$rel.age+(jdat$liberty/365)

# Plot data
ages<-seq(0.25,4,by=0.25) # redefine ages if need be
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)
points(jdat$lenRel~jdat$rel.age, col="blue") # releases
points(jdat$lenRec~jdat$rec.age, col="black") # recaptures
points(FL_cm~age_yrs, data=Leroy_oto, col="gold1") # Overlay Leroy 2000 otolith data

# Fit VB model to otolith and tag-recapture data
jdat_comb<-data.frame(len=c(jdat$lenRec, Leroy_oto$FL_cm))
jdat_comb$age<-c(jdat$rec.age,Leroy_oto$age_yrs)
(vb <- vbFuns(param="Typical"))
(jdat.starts <- list(Linf=81.8741, K=1, t0=-0.2))
jdat.fit <- nls(len~vb(age,Linf,K,t0),data=jdat_comb,start=jdat.starts)
coef(jdat.fit)
summary(jdat.fit)
plot(resid(jdat.fit)~jdat_comb$age) # Plot residuals against age_yrs

# Predict length at age across full range of age estimates in otolith data
predict_l <- function(x) predict(x,newdata=data.frame(age=age))
age <- seq(min(jdat_comb$age), max(jdat_comb$age), length.out=100)
predict_l(jdat.fit)

# Generate 95% bootstrapped CIs around mean estimates, and save as dataframe
jdat.boot2 <- Boot(jdat.fit,f=predict_l) 
jdat.preds <- data.frame(age, predict_l(jdat.fit), confint(jdat.boot2))
names(jdat.preds) <- c("age","fit","LCI","UCI")

# Make plot of data and predictions
plot(len~age, data=jdat_comb, type="p")
lines(jdat.preds$fit~jdat.preds$age, type="l", lwd=2)
polygon(c(jdat.preds$age,rev(jdat.preds$age)),
        c(jdat.preds$LCI,rev(jdat.preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)


###########################################
##  PTTP/RTTP - full dataset
###########################################

# Read in and explore screened data
spcdat<-read.csv("tags_spc.csv") # 6752 records
str(spcdat)
summary(spcdat$lenRel) # min FL at release is 24 cm
spcdat<-spcdat[spcdat$lenRel >=30 & spcdat$lenRel<=69,] # Only retain lengths at release that fall within range of Leroy (2000) lengths.
nrow(spcdat) # 7086 records
spcdat$rel.age<-calc_tr(spcdat$lenRel,coef(f.fit))
spcdat$rec.age<-spcdat$rel.age+(spcdat$liberty/365)

ages<-seq(0.25,4,by=0.25) # redefine ages if need be
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)

points(spcdat$lenRel~spcdat$rel.age, col="blue") # releases
points(spcdat$lenRec~spcdat$rec.age, col="black") # recaptures
points(FL_cm~age_yrs, data=Leroy_oto, col="gold1") # Overlay Leroy 2000 otolith data. 

# Fit VB model to otolith and tag-recapture data
spcdat_comb<-data.frame(len=c(spcdat$lenRec, Leroy_oto$FL_cm))
spcdat_comb$age<-c(spcdat$rec.age,Leroy_oto$age_yrs)
(vb <- vbFuns(param="Typical"))
(spc.starts <- vbStarts(len~age,data=spcdat_comb))
spcdat.fit <- nls(len~vb(age,Linf,K,t0),data=spcdat_comb,start=spc.starts)
coef(spcdat.fit)
summary(spcdat.fit)
plot(resid(spcdat.fit)~spcdat_comb$age) # Plot residuals against age_yrs

# Predict length at age across full range of age estimates in otolith data
predict_l <- function(x) predict(x,newdata=data.frame(age=age))
age <- seq(min(spcdat_comb$age), max(spcdat_comb$age), length.out=100)
predict_l(spcdat.fit)

# Generate 95% bootstrapped CIs around mean estimates, and save as dataframe
spcdat.boot2 <- Boot(spcdat.fit,f=predict_l) 
spcdat.preds <- data.frame(age, predict_l(spcdat.fit), confint(spcdat.boot2))
names(spcdat.preds) <- c("age","fit","LCI","UCI")

# Make plot of data and predictions
plot(len~age, data=spcdat_comb, type="p")
lines(spcdat.preds$fit~spcdat.preds$age, type="l", lwd=2)
polygon(c(spcdat.preds$age,rev(spcdat.preds$age)),
        c(spcdat.preds$LCI,rev(spcdat.preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)


###########################################
##  PTTP - Shimizu
###########################################

# We now use a high-quality subset of the tag-recapture PTTP dataset with records retained 
# only where recapture lengths were measured in the Shimizu lab.

# Read in and explore data
load("PTTP_recapture_filter_extract_allrecaps.RData")
tt <- PTTPrecapsAll
tt$RelDate <- as.Date(tt$sch_date)
tt$RecDate <- as.Date(tt$catchdate)
tt$libertyDays <- as.integer(tt$RecDate - tt$RelDate)
tt$libertyYears <- tt$libertyDays / 365

# Calculate average length, and daily, monthly and annual growth rate
tt$lenAvg <- (tt$lenRec + tt$lenRel) / 2
tt$growth1 <- (tt$lenRec - tt$lenRel) / tt$libertyDays
tt$growth30 <- tt$growth1 * 30
tt$growth365 <- tt$growth1 * 365

# Filter                           # 422
tt <- tt[tt$sp_id == "S",]         # 393 - Select SKJ only
tt <- tt[tt$libertyDays >= 30,]    # 379 - time at liberty > 30 days
tt <- tt[tt$libertyDays < 3*365,]  # 379 - time at liberty < 3 years
tt <- tt[tt$growth1 <= 0.2,]       # 377 - exclude records where growth was > 0.2 cm/day
tt <- tt[tt$RecDate_reliability <=3,] # 250 - retain only records with recovery date accuracy to a fortnight.
tt <- tt[tt$Rel_len_reliability ==1,] # 247 - retain only records where release length reliability = 1.

# Explore
str(tt)
summary(tt$lenRel) # min FL at release is 33 cm
tt<-tt[tt$lenRel >=30 & tt$lenRel<=69,] # Only retain lengths at release that fall within range of Leroy (2000) lengths.
nrow(tt) # All 247 records retained from previous step.
tt$rel.age<-calc_tr(tt$lenRel,coef(f.fit))
tt$rec.age<-tt$rel.age+(tt$libertyDays/365)

ages<-seq(0.25,4,by=0.25) # redefine ages if need be
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)

points(tt$lenRel~tt$rel.age, col="blue") # releases
points(tt$lenRec~tt$rec.age, col="black") # recaptures
points(FL_cm~age_yrs, data=Leroy_oto, col="gold1") # Overlay Leroy 2000 otolith data. 

# Fit VB model to otolith and tag-recapture data
tt_comb<-data.frame(len=c(tt$lenRec, Leroy_oto$FL_cm))
tt_comb$age<-c(tt$rec.age, Leroy_oto$age_yrs)
(vb <- vbFuns(param="Typical"))
tt.starts<-list(Linf=81.8741, K=1, t0=-0.2)
tt.fit <- nls(len~vb(age,Linf,K,t0),data=tt_comb,start=tt.starts)
coef(tt.fit)
summary(tt.fit)
plot(resid(tt.fit)~tt_comb$age) # Plot residuals against age_yrs

# Predict length at age across full range of age estimates in otolith data
predict_l <- function(x) predict(x,newdata=data.frame(age=age))
age <- seq(min(tt_comb$age), max(tt_comb$age), length.out=100)
predict_l(tt.fit)

# Generate 95% bootstrapped CIs around mean estimates, and save as dataframe
tt.boot2 <- Boot(tt.fit,f=predict_l) 
tt.preds <- data.frame(age, predict_l(tt.fit), confint(tt.boot2))
names(tt.preds) <- c("age","fit","LCI","UCI")

# Make plot of data and predictions
plot(len~age, data=tt_comb, type="p")
lines(tt.preds$fit~tt.preds$age, type="l", lwd=2)
polygon(c(tt.preds$age,rev(tt.preds$age)),
        c(tt.preds$LCI,rev(tt.preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)


###########################################
##  PTTP - Shimizu and JPTP
###########################################

# Combine datasets and rerun VB model
tt_jdat_comb<-data.frame(len=c(tt$lenRec, jdat$lenRec, Leroy_oto$FL_cm))
tt_jdat_comb$age<-c(tt$rec.age, jdat$rec.age, Leroy_oto$age_yrs)

ages<-seq(0.25,4,by=0.25) # redefine ages if need be
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)

points(tt$lenRel~tt$rel.age, col="blue") # tt releases
points(jdat$lenRel~jdat$rel.age, col="red") # jdat releases
points(tt$lenRec~tt$rec.age, col="blue") # tt recaptures
points(jdat$lenRec~jdat$rec.age, col="red") # jdat recaptures
points(FL_cm~age_yrs, data=Leroy_oto, col="gold1") # Overlay Leroy 2000 otolith data. 

# Fit VB model to otolith and tag-recapture data
(vb <- vbFuns(param="Typical"))
tt_jdat.starts<-list(Linf=81.8741, K=1, t0=-0.2)
tt_jdat.fit <- nls(len~vb(age,Linf,K,t0),data=tt_jdat_comb,start=tt_jdat.starts)
coef(tt_jdat.fit)
summary(tt_jdat.fit)
plot(resid(tt_jdat.fit)~tt_jdat_comb$age) # Plot residuals against age_yrs

# Predict length at age across full range of age estimates in otolith data
predict_l <- function(x) predict(x,newdata=data.frame(age=age))
age <- seq(min(tt_jdat_comb$age), max(tt_jdat_comb$age), length.out=100)
predict_l(tt_jdat.fit)

# Generate 95% bootstrapped CIs around mean estimates, and save as dataframe
tt_jdat.boot2 <- Boot(tt_jdat.fit,f=predict_l) 
tt_jdat.preds <- data.frame(age, predict_l(tt_jdat.fit), confint(tt_jdat.boot2))
names(tt_jdat.preds) <- c("age","fit","LCI","UCI")

# Make plot of data and predictions
plot(len~age, data=tt_jdat_comb, type="p")
lines(tt_jdat.preds$fit~tt_jdat.preds$age, type="l", lwd=2)
polygon(c(tt_jdat.preds$age,rev(tt_jdat.preds$age)),
        c(tt_jdat.preds$LCI,rev(tt_jdat.preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)


###########################################
# Moving forward...
###########################################

# As @ 15 June 2022, we need to make decisions about the alternative growth curves we take forward for
# for consideration in the 2022 SKJ assessment.
# Right now, we have i) 'WCPO_meta', ii) a growth cessation model (following Maunder et al. 2022) [see 'CGM.R' code], 
# and iii) the combined otolith and tag-recapture VB model presented here.
# To obtain the most appropriate model for iii), we have selected three datasets that we consider the most reliable/informative 
# for this purpose. These are ranked 1, 2 and 3 below. 


###########################################
# 1. PTTP - Shimizu
###########################################

# We have the most confidence in the release and recapture measurements for the PTTP - Shimizu data.
# so decided to move forward with this tag-recapture data alone to develop the second alternative growth curve: 
# 'Leroy 2000 & Shimizu'. 

# Plot of data and VB curve against other growth options for 2022 SKJ assessment
ages<-seq(0.25,4,by=0.25) # redefine ages if need be
pdf("SKJ_growth_options_2022_Shimizu_linf=81 and 71.pdf")
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)
lines(DM_data$length~DM_data$ages, col="red2", lwd=5, lend="butt") # Mean length-at-age from DM estimate
polygon(c(DM_data$ages,rev(DM_data$ages)),
        c(DM_data$length_lower,rev(DM_data$length_upper)),col=rgb(238,0,0, alpha=75, maxColorValue = 255),border=NA)
lines(y=VB_output[4,], x=ages, col="yellow", lwd=2, lend="butt")
polygon(c(ages,rev(ages)),
        c(VB_output[1,],rev(VB_output[3,])), col=rgb(255,255,0, alpha=50, maxColorValue = 255), border=NA)
segments(x0=tt$rel.age, y0=tt$lenRel, x1=tt$rec.age, y1=tt$lenRec, lwd=1, col=rgb(0,0,0, alpha=80, maxColorValue = 255))
points(tt$lenRec~tt$rec.age, col="black", pch=21, cex=0.6, bg="black") # recaptures
points(FL_cm~age_yrs, data=Leroy_oto, col="black", cex=0.6, pch=24, bg="white") # Overlay Leroy 2000 otolith data
points(tt$lenRel~tt$rel.age, col="black", pch=21, cex=0.6, bg="white") # releases

# Overlay VB curve for Leroy 2000 otolith data and PTTP - Shimizu data combined
age <- seq(0.25,4,by=0.25) 
tt.boot2 <- Boot(tt.fit,f=predict_l) 
tt.preds <- data.frame(age, predict_l(tt.fit), confint(tt.boot2))
names(tt.preds) <- c("age","fit","LCI","UCI")
lines(tt.preds$fit~tt.preds$age, type="l", lwd=2, lend="butt")
polygon(c(tt.preds$age,rev(tt.preds$age)),
        c(tt.preds$LCI,rev(tt.preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)

# Overlay curve and 95% CIs from GCM fitted to the Shimizu data, with Linf=81 cm
mcm_curve<-read.csv("curve.csv")
mcm_curve<-mcm_curve[mcm_curve$Year>=0.25 & mcm_curve$Year <=4,] # subset to fit MFCL age structure
lines(mcm_curve$Lhat~mcm_curve$Year, type="l", lwd=2, lty=2, lend="butt", col="darkblue")
polygon(c(mcm_curve$Year,rev(mcm_curve$Year)),
        c(mcm_curve$Lower,rev(mcm_curve$Upper)), col=rgb(0,0,139, alpha=50, maxColorValue = 255), border=NA)

# Overlay curve and 95% CIs from GCM fitted to the Shimizu data, with Linf=71 cm
mcm_curve<-read.csv("curve_31_71.csv")
mcm_curve<-mcm_curve[mcm_curve$Year>=0.25 & mcm_curve$Year <=4,] # subset to fit MFCL age structure
lines(mcm_curve$Lhat~mcm_curve$Year, type="l", lwd=2, lty=1, lend="butt", col="darkblue")
polygon(c(mcm_curve$Year,rev(mcm_curve$Year)),
        c(mcm_curve$Lower,rev(mcm_curve$Upper)), col=rgb(0,0,139, alpha=50, maxColorValue = 255), border=NA)

legend(x=2.5, y=40, legend=c("2022 DM", "WCPO meta", "Leroy 2000 & Shimizu", "Leroy 2000 data", "Tag release", "Tag recapture", "GCM - Linf=81cm", "GCM - Linf=71cm"), cex=0.95, 
       lty=c(1,1,1, NA, NA, NA, 2,1), col=c("red2", "yellow", "black", "black", "black", "black", "darkblue", "darkblue"), lwd=c(4,2,2, NA, NA, NA, 2, 2), pch=c(NA, NA, NA,24,21,21, NA, NA), 
       pt.bg=c(NA,NA,NA,"white","white", "black", NA, NA), seg.len = 1.6, bty="n")
dev.off()


###########################################
# 2. JPTP
###########################################

# The JPTP tag-recapture data has some uncertainties around quality control on release lengths, 
# or how recapture lengths and date reliability are recorded.
# That said, this data may be informative as they encompass release and recaptures in the northern
# region, a region not covered by PTTP/RTTP releases. This data also contains several useful datapoints with 
# long times at liberty - something we're missing in the Shimizu dataset.
# We refer to this as 'Leroy 2000 & JPTP'. 

# Plot of data and VB curve against other growth options for 2022 SKJ assessment
ages<-seq(0.25,4,by=0.25) # redefine ages if need be
pdf("SKJ_growth_options_2022_JPTP.pdf")
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)
lines(DM_data$length~DM_data$ages, col="red2", lwd=5, lend="butt") # Mean length-at-age from DM estimate
polygon(c(DM_data$ages,rev(DM_data$ages)),
        c(DM_data$length_lower,rev(DM_data$length_upper)),col=rgb(238,0,0, alpha=75, maxColorValue = 255),border=NA)
lines(y=VB_output[4,], x=ages, col="yellow", lwd=2, lend="butt")
polygon(c(ages,rev(ages)),
        c(VB_output[1,],rev(VB_output[3,])), col=rgb(255,255,0, alpha=50, maxColorValue = 255), border=NA)
segments(x0=jdat$rel.age, y0=jdat$lenRel, x1=jdat$rec.age, y1=jdat$lenRec, lwd=1, col=rgb(0,0,0, alpha=80, maxColorValue = 255))
points(jdat$lenRec~jdat$rec.age, col="black", pch=21, cex=0.6, bg="black") # recaptures
points(FL_cm~age_yrs, data=Leroy_oto, col="black", cex=0.6, pch=24, bg="white") # Overlay Leroy 2000 otolith data
points(jdat$lenRel~jdat$rel.age, col="black", pch=21, cex=0.6, bg="white") # releases

# Overlay VB curve for Leroy 2000 otolith data and JPTP data combined
age <- seq(0.25,4,by=0.25) 
jdat.boot2 <- Boot(jdat.fit,f=predict_l) 
jdat.preds <- data.frame(age, predict_l(jdat.fit), confint(jdat.boot2))
names(jdat.preds) <- c("age","fit","LCI","UCI")
lines(jdat.preds$fit~jdat.preds$age, type="l", lwd=2, lend="butt")
polygon(c(jdat.preds$age,rev(jdat.preds$age)),
        c(jdat.preds$LCI,rev(jdat.preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)

legend(x=2.5, y=40, legend=c("2022 DM", "WCPO meta", "Leroy 2000 & JPTP", "Leroy 2000 data", "Tag release", "Tag recapture"), cex=0.95, 
       lty=c(1,1,1, NA, NA, NA, 1), col=c("red2", "yellow", "black", "black", "black", "black"), lwd=c(4,2,2, NA, NA, NA, 2), pch=c(NA, NA, NA,24,21,21), 
       pt.bg=c(NA,NA,NA,"white","white", "black"), seg.len = 1.6, bty="n")
dev.off()


###########################################
# 3. PTTP - Shimizu and JPTP
###########################################

# Given we can confirm the quality of the JPTP data for growth estimation purposes, combing this with the 
# PTTP - Shimizu data would be an appropriate way forward.
# This combined dataset better encompasses the northern and equatorial regions included in the 2022 SKJ assessment,
# capturing potential growth differences we see in those regions, whilst having all the benefits of
# the JPTP data outlined in 2. above.

# Plot of data and VB curve against other growth options for 2022 SKJ assessment
ages<-seq(0.25,4,by=0.25) # redefine ages if need be
pdf("SKJ_growth_options_2022_Shimizu & JPTP.pdf")
plot(vb(t=ages, Linf=93.6, K=0.43, t0=-0.49)~ages, type="n", xlim=c(0,4), ylim=c(0,115), # set plotting region based on # Tanabe et al. 2003 parameters
     xlab="Age (years)", xaxt="n", ylab="Fork length (cm)", las=1)
a <- 1:4
c = a[seq(1, length(a),1)]
axis(side=1, at=seq(0.25,4,by=0.25), labels=F) 
axis(1, at=1:4, labels = c, las = 1)
lines(DM_data$length~DM_data$ages, col="red2", lwd=5, lend="butt") # Mean length-at-age from DM estimate
polygon(c(DM_data$ages,rev(DM_data$ages)),
        c(DM_data$length_lower,rev(DM_data$length_upper)),col=rgb(238,0,0, alpha=75, maxColorValue = 255),border=NA)
lines(y=VB_output[4,], x=ages, col="yellow", lwd=2, lend="butt")
polygon(c(ages,rev(ages)),
        c(VB_output[1,],rev(VB_output[3,])), col=rgb(255,255,0, alpha=50, maxColorValue = 255), border=NA)

segments(x0=tt$rel.age, y0=tt$lenRel, x1=tt$rec.age, y1=tt$lenRec, lwd=1, col=rgb(0,0,0, alpha=80, maxColorValue = 255))
segments(x0=jdat$rel.age, y0=jdat$lenRel, x1=jdat$rec.age, y1=jdat$lenRec, lwd=1, col=rgb(0,0,0, alpha=80, maxColorValue = 255))
points(tt$lenRec~tt$rec.age, col="black", pch=21, cex=0.6, bg="black") # tt recaptures
points(jdat$lenRec~jdat$rec.age, col="black", pch=21, cex=0.6, bg="black") # jdat recaptures
points(FL_cm~age_yrs, data=Leroy_oto, col="black", cex=0.6, pch=24, bg="white") # Overlay Leroy 2000 otolith data
points(tt$lenRel~tt$rel.age, col="black", pch=21, cex=0.6, bg="white") # tt releases
points(jdat$lenRel~jdat$rel.age, col="black", pch=21, cex=0.6, bg="white") # jdat releases

# Overlay VB curve for Leroy 2000 otolith data and PTTP - Shimizu and JPTP data combined
age <- seq(0.25,4,by=0.25) 
tt_jdat.boot2 <- Boot(tt_jdat.fit,f=predict_l) 
tt_jdat.preds <- data.frame(age, predict_l(tt_jdat.fit), confint(tt_jdat.boot2))
names(tt_jdat.preds) <- c("age","fit","LCI","UCI")
lines(tt_jdat.preds$fit~tt_jdat.preds$age, type="l", lwd=2, lend="butt")
polygon(c(tt_jdat.preds$age,rev(tt_jdat.preds$age)),
        c(tt_jdat.preds$LCI,rev(tt_jdat.preds$UCI)), col=rgb(0,0,0, alpha=50, maxColorValue = 255), border=NA)

legend(x=2.2, y=40, legend=c("2022 DM", "WCPO meta", "Leroy 2000, Shimizu & JPTP", "Leroy 2000 data", "Tag release", "Tag recapture"), cex=0.95, 
       lty=c(1,1,1, NA, NA, NA, 1), col=c("red2", "yellow", "black", "black", "black", "black"), lwd=c(4,2,2, NA, NA, NA, 2), pch=c(NA, NA, NA,24,21,21), 
       pt.bg=c(NA,NA,NA,"white","white", "black"), seg.len = 1.6, bty="n")
dev.off()


## END ##





