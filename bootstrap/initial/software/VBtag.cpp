#include <TMB.hpp>
#include <iostream>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Lrel);        // length at release (tags)
  DATA_VECTOR(Lrec);        // length at recapture (tags)
  DATA_VECTOR(liberty);     // time at liberty (tags)
  DATA_SCALAR(a);           // younger age where sd(length) is sigma_a
  DATA_SCALAR(b);           // older age where sd(length) is sigma_b
  int Ntags = Lrel.size();  // number of tags

  PARAMETER(log_Linf);
  PARAMETER(log_k);
  PARAMETER(t0);
  PARAMETER(log_sigma_a);
  PARAMETER(log_sigma_b);
  PARAMETER_VECTOR(log_age);

  Type Linf = exp(log_Linf);
  Type k = exp(log_k);
  Type sigma_a = exp(log_sigma_a);
  Type sigma_b = exp(log_sigma_b);
  Type sigma_slope = (log_sigma_b - log_sigma_a) / (b - a);
  Type sigma_intercept = log_sigma_a - a * sigma_slope;

  vector<Type> age(Ntags);
  vector<Type> Lrel_hat(Ntags);
  vector<Type> Lrec_hat(Ntags);
  vector<Type> log_sigma_Lrel(Ntags);  // ensure sigmas are positive
  vector<Type> log_sigma_Lrec(Ntags);  // ensure sigmas are positive
  vector<Type> sigma_Lrel(Ntags);      // ensure sigmas are positive
  vector<Type> sigma_Lrec(Ntags);      // sigma for each Lrec datapoint
  vector<Type> nll_Lrel(Ntags);        // nll for each Lrel datapoint
  vector<Type> nll_Lrec(Ntags);        // nll for each Lrec datapoint
  vector<Type> age_seq(10*365+1);      // ages to calculate growth curve
  vector<Type> curve(10*365+1);        // growth curve for ages 0->10 (confint)

  age = exp(log_age);
  Lrel_hat = Linf * (Type(1.0) - exp(-k * (age - t0)));
  Lrec_hat = Linf * (Type(1.0) - exp(-k * (age + liberty - t0)));
  log_sigma_Lrel = sigma_intercept + age * sigma_slope;
  log_sigma_Lrec = sigma_intercept + (age + liberty) * sigma_slope;
  sigma_Lrel = exp(log_sigma_Lrel);
  sigma_Lrec = exp(log_sigma_Lrec);

  Type nll = 0.0;
  nll_Lrel = -dnorm(Lrel, Lrel_hat, sigma_Lrel, true);
  nll_Lrec = -dnorm(Lrec, Lrec_hat, sigma_Lrec, true);
  nll = sum(nll_Lrel) + sum(nll_Lrec);

  for(int i=0; i<=10*365; i++)
    age_seq(i) = Type(i) / 365.0;
  curve = Linf * (Type(1.0) - exp(-k * (age_seq - t0)));

  ADREPORT(curve);
  REPORT(Linf);
  REPORT(k);
  REPORT(t0);
  REPORT(age);
  REPORT(liberty);
  REPORT(Lrel);
  REPORT(Lrec);
  REPORT(Lrel_hat);
  REPORT(Lrec_hat);
  REPORT(a);
  REPORT(b);
  REPORT(sigma_a);
  REPORT(sigma_b);
  REPORT(sigma_Lrel);
  REPORT(sigma_Lrec);
  REPORT(nll_Lrel);
  REPORT(nll_Lrec);
  REPORT(curve);

  return nll;
}
