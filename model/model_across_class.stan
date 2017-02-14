data {
  int<lower=0> T;              // number of observation times
  int<lower=0> N_years;        // number of years
  int<lower=0> N_months;       // number of months
  
  int<lower=0> N_all[T]; // total jobs
  
  // Job ads by class, irrespective of disciplinarity
  int<lower=0> N_pd[T];  // number of postdoc jobs
  int<lower=0> N_gr[T];  // number of graduate jobs
  int<lower=0> N_tt[T];  // number of jobs
  
  real<lower=0> date[T];  // time 
  int<lower=0>  year[T];  // year 
  int<lower=0>  month[T]; // month 
  
  matrix<lower=0, upper=1>[T, N_months] X;
}

parameters {

  //real alpha_all; // constant shared
  vector[3] alpha_ind; // constant ind
  
  vector[N_months] month_ind[3]; // month ind

  vector[N_years] year_ind[3]; // year ind
  vector[N_years] year_shared; // year ind

  // Unique slope parameter by class.
  vector[3] time_slope_ind;  // time slope ind

  // Used for the error estimate for the model.
  vector<lower=0.001>[3] sigma;

  real mu_tt[T];
  real mu_pd[T];
  real mu_gr[T];
}
transformed parameters {
  
  real<lower=0, upper=1> theta_tt[T];
  real<lower=0, upper=1> theta_pd[T];
  real<lower=0, upper=1> theta_gr[T];
  
  for (t in 1:T){
    theta_tt[t] <- inv_logit(mu_tt[t]);
    theta_pd[t] <- inv_logit(mu_pd[t]);
    theta_gr[t] <- inv_logit(mu_gr[t]);
  }  
}

model {
  
  for (t in 1:T){
    mu_tt[t] ~ normal(alpha_ind[1] + X[t] * month_ind[1] + year_shared[year[t]] + year_ind[1][year[t]] + time_slope_ind[1] * t/T, sigma[1]);
    
    mu_pd[t] ~ normal(alpha_ind[2] + X[t] * month_ind[2] + year_shared[year[t]] + year_ind[2][year[t]] + time_slope_ind[2] * t/T, sigma[2]);
    
    mu_gr[t] ~ normal(alpha_ind[3] + X[t] * month_ind[3] + year_shared[year[t]] + year_ind[3][year[t]] + time_slope_ind[3] * t/T, sigma[3]);
  }

  for (t in 1:T){
    N_tt[t] ~ binomial(N_all[t], theta_tt[t]);
    N_pd[t] ~ binomial(N_all[t], theta_pd[t]);
    N_gr[t] ~ binomial(N_all[t], theta_gr[t]);
  }
}