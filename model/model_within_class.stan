data {
  int<lower=0> T;              // number of observation times
  int<lower=0> N_years;        // number of years
  int<lower=0> N_months;       // number of months

  int<lower=0> y_pd[T]; // number of non-interdisciplinary jobs
  int<lower=0> N_pd[T]; // total jobs
  
  int<lower=0> y_gr[T]; // number of non-interdisciplinary jobs
  int<lower=0> N_gr[T]; // total jobs
  
  int<lower=0> y_tt[T]; // number of non-interdisciplinary jobs
  int<lower=0> N_tt[T]; // total jobs
  
  real<lower=0> date[T]; // time 
  int<lower=0>  year[T];  // year 
  int<lower=0>  month[T]; // year 
  
  matrix<lower=0, upper=1>[T, N_months] X;
}
parameters {

  vector[3] alpha_ind;  // constant ind
  
  vector[3] time_slope_ind;  // time slope ind

  vector[N_years] year_ind[3]; // year ind
  vector[N_years] year_shared; // year ind
  
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
    mu_tt[t] ~ normal(alpha_ind[1] + year_shared[year[t]] + year_ind[1][year[t]] + time_slope_ind[1] * t/T, sigma[1]);
    mu_pd[t] ~ normal(alpha_ind[2] + year_shared[year[t]] + year_ind[2][year[t]] + time_slope_ind[2] * t/T, sigma[2]);
    mu_gr[t] ~ normal(alpha_ind[3] + year_shared[year[t]] + year_ind[3][year[t]] + time_slope_ind[3] * t/T, sigma[3]);
  }

  for (t in 1:T){
    y_tt[t] ~ binomial(N_tt[t], theta_tt[t]);
    y_pd[t] ~ binomial(N_pd[t], theta_pd[t]);
    y_gr[t] ~ binomial(N_gr[t], theta_gr[t]);
  }
}