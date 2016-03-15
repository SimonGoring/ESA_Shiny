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
  
  vector[K] beta_tt;
  vector[K] beta_pd;
  vector[K] beta_gr;
  
  //vector[N_years] beta_t;
  //vector[N_months] beta_t;
  vector[T] beta_t;
  //vector[N_months] beta_month;
  
//   real<lower=0.001> sigma_tt;
//   real<lower=0.001> sigma_pd;
//   real<lower=0.001> sigma_gr;
  
  real<lower=0.001> sigma;
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
  
//   // priors
//   alpha ~ normal(0, 5);
//   beta_tt  ~ normal(0, 5);
//   beta_t ~ normal(0, 5);
  
  for (t in 1:T){
    mu_tt[t] ~ normal(alpha_tt + X[t] * beta_tt + beta_t[t], sigma);
    mu_pd[t] ~ normal(alpha_pd + X[t] * beta_pd + beta_t[t], sigma);
    mu_gr[t] ~ normal(alpha_gr + X[t] * beta_gr + beta_t[t], sigma);
//     mu_tt[t] ~ normal(alpha_tt + X[t] * beta_tt + beta_t[year[t]], sigma);
//     mu_pd[t] ~ normal(alpha_pd + X[t] * beta_pd + beta_t[year[t]], sigma);
//     mu_gr[t] ~ normal(alpha_gr + X[t] * beta_gr + beta_t[year[t]], sigma);
//     mu_tt[t] ~ normal(alpha_tt + X[t] * beta_tt + beta_t[month[t]], sigma_tt);
//     mu_pd[t] ~ normal(alpha_pd + X[t] * beta_pd + beta_t[month[t]], sigma_pd);
//     mu_gr[t] ~ normal(alpha_gr + X[t] * beta_gr + beta_t[month[t]], sigma_gr);
//     mu_tt[t] ~ normal(alpha_tt + X[t] * beta_tt + X[t] * beta_t, sigma);
//     mu_pd[t] ~ normal(alpha_pd + X[t] * beta_pd + X[t] * beta_t, sigma);
//     mu_gr[t] ~ normal(alpha_gr + X[t] * beta_gr + X[t] * beta_t, sigma);
  }

//  for(t in 1:T){
//    beta_t[t] ~ normal(beta_month[month[t]], sigma_month);
//  }

  for (t in 1:T){
    y_tt[t] ~ binomial(N_tt[t], theta_tt[t]);
    y_pd[t] ~ binomial(N_pd[t], theta_pd[t]);
    y_gr[t] ~ binomial(N_gr[t], theta_gr[t]);
  }
}