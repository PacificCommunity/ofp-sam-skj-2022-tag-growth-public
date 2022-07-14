## Run growth cessation analysis with otoliths, write model results

## Before: GCM_oto.cpp (bootstrap/software),
##         otoliths.csv, tags_jp.csv, tags_shi.csv (data)
## After:  gcm_oto_results.RData (model)

library(TAF)
library(TMB)

mkdir("model")

## Fixed       Linf, Afix, Lfix, sd_a, sd_MEb
## Estimated   age, K, rmax, sd_b

## Read data
shi <- read.taf("data/tags_shi.csv")
jp <- read.taf("data/tags_jp.csv")
tt <- rbind(shi[c("lenRel", "lenRec", "libertyYears")],
            jp[c("lenRel", "lenRec", "libertyYears")])
oto <- read.taf("data/otoliths.csv")

## Specify Lfix and Linf
Lfix_use <-  34
Linf_use <-  73

## Set initial age values
tt$age <- tt$lenRel / Lfix_use

## TMB data object
data <- list(
  Robust = 0,
  Afix = 0.25,
  Ntags = nrow(tt),
  Tags = as.matrix(tt[c("lenRel", "lenRec", "libertyYears")]),
  Aoto=oto$age,
  Loto=oto$len
)

## TMB parameter object
par <- list(
  ln_rmax = log(20),
  ln_K = log(0.6),
  ln_Linf = log(Linf_use),
  ln_Lfix = log(Lfix_use),
  ln_sd_a = log(0.0000001),
  ln_sd_b = log(0.01),
  ln_sd_MEb = log(0.0222),
  ln_age = log(tt$age)
)

## Not estimated
map <- list(
  ## ln_Linf = factor(NA),
  ln_Lfix = factor(NA),
  ln_sd_a = factor(NA),
  ln_sd_MEb = factor(NA)
)

## Compile model
cp("bootstrap/software/GCM_oto.cpp", "model")
compile("model/GCM_oto.cpp")
dyn.load(dynlib("model/GCM_oto"))

## Run model
obj <- MakeADFun(data, par, DLL="GCM_oto", map=map, silent=TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr,
              control=list(eval.max=2000,iter.max=2000))
objreport <- obj$report()
sdreport <- sdreport(obj, getReportCovariance=FALSE)

## Save results
save(obj, opt, objreport, sdreport, file="model/gcm_oto_results.RData")
