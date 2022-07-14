#include <TMB.hpp>
#include <iostream>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Lrel);        // length at release (tags)
  DATA_VECTOR(Lrec);        // length at recapture (tags)
  DATA_VECTOR(liberty);     // time at liberty (tags)
  DATA_VECTOR(Aoto);        // age (otoliths)
  DATA_VECTOR(Loto);        // length (otoliths)
  DATA_SCALAR(a);           // younger age where sd(length) is sigma_a
  DATA_SCALAR(b);           // older age where sd(length) is sigma_b
  int Ntags = Lrel.size();  // number of tags
  int Noto = Loto.size();   // number of otoliths

  PARAMETER(log_Linf);
  PARAMETER(log_k);
  PARAMETER(log_alpha);
  PARAMETER(log_beta);
  PARAMETER(log_sigma_a);
  PARAMETER(log_sigma_b);
  PARAMETER_VECTOR(log_age);

  Type Linf = exp(log_Linf);
  Type k = exp(log_k);
  Type alpha = exp(log_alpha);
  Type beta = exp(log_beta);
  Type sigma_a = exp(log_sigma_a);
  Type sigma_b = exp(log_sigma_b);
  Type sigma_slope = (log_sigma_b - log_sigma_a) / (b - a);
  Type sigma_intercept = log_sigma_a - a * sigma_slope;

  vector<Type> age(Ntags);
  vector<Type> Lrel_hat(Ntags);
  vector<Type> Lrec_hat(Ntags);
  vector<Type> Loto_hat(Noto);
  vector<Type> log_sigma_Lrel(Ntags);  // ensure sigmas are positive
  vector<Type> log_sigma_Lrec(Ntags);  // ensure sigmas are positive
  vector<Type> log_sigma_Loto(Noto);   // ensure sigmas are positive
  vector<Type> sigma_Lrel(Ntags);      // sigma for each Lrel datapoint
  vector<Type> sigma_Lrec(Ntags);      // sigma for each Lrec datapoint
  vector<Type> sigma_Loto(Noto);       // sigma for each Loto datapoint
  vector<Type> nll_Lrel(Ntags);        // nll for each Lrel datapoint
  vector<Type> nll_Lrec(Ntags);        // nll for each Lrec datapoint
  vector<Type> nll_Loto(Noto);         // nll for each Loto datapoint
  vector<Type> age_seq(10*365+1);      // ages to calculate growth curve
  vector<Type> curve(10*365+1);        // growth curve for ages 0->10 (confint)

  age = exp(log_age);
  Lrel_hat = Linf * pow(Type(1.0) - alpha * exp(-k*age), beta);
  Lrec_hat = Linf * pow(Type(1.0) - alpha * exp(-k*(age+liberty)), beta);
  Loto_hat = Linf * pow(Type(1.0) - alpha * exp(-k*Aoto), beta);
  log_sigma_Lrel = sigma_intercept + age * sigma_slope;
  log_sigma_Lrec = sigma_intercept + (age+liberty) * sigma_slope;
  log_sigma_Loto = sigma_intercept + Aoto * sigma_slope;
  sigma_Lrel = exp(log_sigma_Lrel);
  sigma_Lrec = exp(log_sigma_Lrec);
  sigma_Loto = exp(log_sigma_Loto);

  Type nll = 0.0;
  nll_Lrel = -dnorm(Lrel, Lrel_hat, sigma_Lrel, true);
  nll_Lrec = -dnorm(Lrec, Lrec_hat, sigma_Lrec, true);
  nll_Loto = -dnorm(Loto, Loto_hat, sigma_Loto, true);
  nll = sum(nll_Lrel) + sum(nll_Lrec) + sum(nll_Loto);

  for(int i=0; i<=10*365; i++)
    age_seq(i) = Type(i) / 365.0;
  curve = Linf * pow(Type(1.0) - alpha * exp(-k*age_seq), beta);

  ADREPORT(curve);
  REPORT(Linf);
  REPORT(k);
  REPORT(alpha);
  REPORT(beta);
  REPORT(age);
  REPORT(liberty);
  REPORT(Lrel);
  REPORT(Lrec);
  REPORT(Aoto);
  REPORT(Loto);
  REPORT(Lrel_hat);
  REPORT(Lrec_hat);
  REPORT(Loto_hat);
  REPORT(a);
  REPORT(b);
  REPORT(sigma_a);
  REPORT(sigma_b);
  REPORT(sigma_Lrel);
  REPORT(sigma_Lrec);
  REPORT(sigma_Loto);
  REPORT(nll_Lrel);
  REPORT(nll_Lrec);
  REPORT(nll_Loto);
  REPORT(curve);

  return nll;
}
