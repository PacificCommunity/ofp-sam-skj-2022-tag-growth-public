## Fit basic von Bertalanffy model to otolith data, write model results

## Before: otoliths.csv (data)
## After:  vonbert_results.RData (model)

library(TAF)

mkdir("model")

## Read data
oto <- read.taf("data/otoliths.csv")

## Initial parameters
par <- c(Linf=80, k=1, t0=0)

## Model
vb <- function(par, data, output="neglogL")
{
  Linf <- par["Linf"]
  k <- par["k"]
  t0 <- par["t0"]
  age <- data$age
  len <- data$len
  len_hat <- Linf * (1 - exp(-k * (age - t0)))
  res <- len - len_hat
  n <- length(res)
  sigma <- sqrt(sum((res-mean(res))^2) / n)
  neglogL <- -sum(dnorm(len, len_hat, sigma, log=TRUE))
  fit <- data.frame(age, len, len_hat, res)
  if(output == "neglogL")
    neglogL
  else
    list(fit=fit, par=par, sigma=sigma, neglogL=neglogL)
}

opt <- nlminb(par, vb, data=oto)
report <- vb(opt$par, data=oto, output="full")

## Save results
save(opt, report, file="model/vonbert_results.RData")
