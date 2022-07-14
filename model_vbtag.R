## Run VBtag analysis, write model results

## Before: VBtag.cpp (bootstrap/software), tags_jp.csv, tags_shi.csv (data)
## After:  vbtag_results.RData (model)

library(TAF)
library(TMB)

mkdir("model")

## Fixed       Linf, t0, sigma_a, sigma_b
## Estimated   age, k

## Read data
shi <- read.taf("data/tags_shi.csv")
jp <- read.taf("data/tags_jp.csv")
tt <- rbind(shi[c("lenRel", "lenRec", "libertyYears")],
            jp[c("lenRel", "lenRec", "libertyYears")])

## Set initial age values
tt$age <- tt$lenRel / 25

## TMB data object
data <- list(
  Lrel = tt$lenRel,
  Lrec = tt$lenRec,
  liberty = tt$libertyYears,
  a = 1,
  b = 2
)

## TMB parameter object
par <- list(
  log_Linf = log(90),
  log_k = log(0.4),
  t0 = 0,
  log_sigma_a = log(1.5),
  log_sigma_b = log(2.0),
  log_age = log(tt$age)
)

## Not estimated
map <- list(
  ## log_Linf = factor(NA),
  ## t0 = factor(NA),
  log_sigma_a = factor(NA),
  log_sigma_b = factor(NA)
)

## Compile model
cp("bootstrap/software/VBtag.cpp", "model")
compile("model/VBtag.cpp")
dyn.load(dynlib("model/VBtag"))

## Run model
obj <- MakeADFun(data, par, DLL="VBtag", map=map, silent=TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr,
              control=list(eval.max=2000,iter.max=2000))
objreport <- obj$report()
sdreport <- sdreport(obj, getReportCovariance=FALSE)

## Save results
save(obj, opt, objreport, sdreport, file="model/vbtag_results.RData")
