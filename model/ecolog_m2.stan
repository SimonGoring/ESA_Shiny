data {
  int<lower=0> T;        // number of times
  int<lower=0> N_years;        // number of years
  int<lower=0> N_months;        // number of months
  
  int<lower=0> y_pd[T]; // number of non-interdisciplinary jobs
  int<lower=0> N_pd[T]; // total jobs
  
  int<lower=0> y_gr[T]; // number of non-interdisciplinary jobs
  int<lower=0> N_gr[T]; // total jobs
  
  int<lower=0> y_tt[T]; // number of non-interdisciplinary jobs
  int<lower=0> N_tt[T]; // total jobs
  
  real<lower=0> date[T]; // time 
  int<lower=0> year[T]; // year 
  int<lower=0> month[T]; // year 
  int<lower=0> K;        // times 
  
  matrix<lower=0, upper=1>[T, K] X;
}
parameters {
  
  real alpha_tt;
  real alpha_pd;
  real alpha_gr;
  
  real alpha_tt1;
  real alpha_pd1;
  real alpha_gr1;
  
  vector[K] beta_tt;
  vector[K] beta_pd;
  vector[K] beta_gr;
  
//     
//   vector[T] beta_tt;
//   vector[T] beta_pd;
//   vector[T] beta_gr;
//   
  vector[N_years] beta_year;
  //vector[N_months] beta_t;
  vector[K] beta_all;
  //vector[T] beta_t;
  //vector[N_months] beta_month;
  
  vector<lower=0.001>[3] sigma;
//   real<lower=0.001> sigma_pd;
//   real<lower=0.001> sigma_gr;
  
  //real<lower=0.001> sigma_month;
  
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
  
   // priors
   sigma ~ cauchy(0,5);
//   alpha ~ normal(0, 5);
//   beta_tt  ~ normal(0, 5);
//   beta_t ~ normal(0, 5);
  
  for (t in 1:T){
    mu_tt[t] ~ normal(alpha_tt + alpha_tt1 * t/T + X[t] * beta_all +  beta_tt[month[t]] + beta_year[year[t]], sigma[1]);
    mu_pd[t] ~ normal(alpha_pd + alpha_pd1 * t/T + X[t] * beta_all +  beta_pd[month[t]] + beta_year[year[t]], sigma[2]);
    mu_gr[t] ~ normal(alpha_gr + + alpha_gr1 * t/T + X[t] * beta_all +  beta_gr[month[t]] + beta_year[year[t]], sigma[3]);
  }

//  for(t in 1:T){
//    beta_tt[t] ~ normal(beta_month[month[t]], sigma_month);
//  }

  for (t in 1:T){
    y_tt[t] ~ binomial(N_tt[t], theta_tt[t]);
    y_pd[t] ~ binomial(N_pd[t], theta_pd[t]);
    y_gr[t] ~ binomial(N_gr[t], theta_gr[t]);
  }
}