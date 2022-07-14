#include <TMB.hpp>
#include <iostream>

//need to deal with measurement vs process error

template<class Type>
Type objective_function<Type>::operator() ()
{
  std::cout<<"*********** It is running ***********"<<std::endl;
  
   /* Data section */
  DATA_INTEGER(Robust) // 1 = robust, anything else is not  
  DATA_SCALAR(Afix)
  DATA_INTEGER(Ntags)
  DATA_ARRAY(Tags) //Lrel, Lrec, time at liberty
    
  /* Parameter section */
  PARAMETER(ln_rmax); 
  PARAMETER(ln_K); 
  PARAMETER(ln_Linf); 
  PARAMETER(ln_Lfix); 
  PARAMETER(ln_sd_a); 
  PARAMETER(ln_sd_b); 
  PARAMETER(ln_sd_MEb); 
  PARAMETER_VECTOR(ln_age);

  // ***** Set up parameters *****
  Type Lfix;
  Type Linf;
  Type K;
  Type L0;
  Type rmax;
  Type a50; 

  Type sd_a;
  Type sd_b;
  Type sd_MEb; 

  Lfix = exp(ln_Lfix);
  Linf = exp(ln_Linf);
  K = exp(ln_K);
  rmax = exp(ln_rmax);
  L0 = Lfix -rmax*Afix;
  a50 = log(exp((K*(Linf-L0))/rmax)-1)/K; 

  sd_a = exp(ln_sd_a);
  sd_b = exp(ln_sd_b);
  sd_MEb = exp(ln_sd_MEb); 

  vector<Type> age(Ntags); 
  age=exp(ln_age);

  vector<Type> Lrel_pred(Ntags); 
  vector<Type> Lrec_pred(Ntags); 
  vector<Type> Lrel_nll(Ntags); 
  vector<Type> Lrec_nll(Ntags); 
  vector<Type> Lrel_sd(Ntags); 
  vector<Type> Lrec_sd(Ntags); 

  vector<Type> Lrel_MEsd(Ntags); 
  vector<Type> Lrec_MEsd(Ntags); 

  vector<Type> Lrel_obs(Ntags); 
  vector<Type> Lrec_obs(Ntags); 
  vector<Type> tal(Ntags); 

  vector<Type> curve(10*365+1);  // growth curve for age [0->10] years (confint)

  for(int i=0; i<= Ntags-1; i++)
  {
  Lrel_obs(i) = Tags(i,0);
  Lrec_obs(i) = Tags(i,1);
  tal(i) = Tags(i,2);
  }

  Type ans=0;

    /* Procedure section */
  //Calculate the predicted lengths
  for(int i=0; i<= Ntags-1; i++)
  {
	    Lrel_pred(i) = L0 +rmax*((log(exp(-K*a50)+1)-log(exp(K*(age(i)-a50))+1))/K+age(i));
	    Lrec_pred(i) = L0 +rmax*((log(exp(-K*a50)+1)-log(exp(K*((age(i)+tal(i))-a50))+1))/K+(age(i)+tal(i)));
  }  

  //Calculate likelihood
  for(int i=0; i<= Ntags-1; i++)
  {
    Lrel_sd(i) = sd_a + sd_b*Lrel_pred(i); 
    Lrec_sd(i) = sd_a + sd_b*Lrec_pred(i);  

    Lrel_MEsd(i) = sd_MEb*Lrel_pred(i); 
    Lrec_MEsd(i) = sd_MEb*Lrec_pred(i);  

    if(Robust==1) {
    Lrel_nll(i) = -log(dnorm(Lrel_obs(i),Lrel_pred(i),Lrel_sd(i)+Lrel_MEsd(i),false) + 0.1);  
    Lrec_nll(i) = -log(dnorm(Lrec_obs(i),Lrec_pred(i),Lrec_sd(i)+Lrec_MEsd(i),false) + 0.1);
    }
    else {
      Lrel_nll(i) = -dnorm(Lrel_obs(i),Lrel_pred(i),Lrel_sd(i)+Lrel_MEsd(i),true);  //this is not the robust likelihood
      Lrec_nll(i) = -dnorm(Lrec_obs(i),Lrec_pred(i),Lrec_sd(i)+Lrec_MEsd(i),true);
    }
  }  
  ans=sum(Lrel_nll)+sum(Lrec_nll);
  for(int i=0; i<=10*365; i++)
  {
    Type aa = Type(i) / 365.0;
    curve(i) = L0 + rmax * ((log(exp(-K*a50)+1)-log(exp(K*(aa-a50))+1))/K+aa);
  }

  REPORT(age);
  REPORT(Lrel_pred);
  REPORT(Lrec_pred);
  REPORT(Lrel_sd);
  REPORT(Lrec_sd);
  REPORT(Lrel_nll);
  REPORT(Lrec_nll);
  REPORT(Lrel_obs);
  REPORT(Lrec_obs);
  REPORT(tal);  
  REPORT(Tags);

  REPORT(Lfix);
  REPORT(Linf);
  REPORT(K);
  REPORT(L0);
  REPORT(rmax);
  REPORT(a50);
  
  ADREPORT(curve);

  return ans;
}
