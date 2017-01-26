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
  int<lower=0> year[T];  // year 
  int<lower=0> month[T]; // year 
  int<lower=0> K;        // times 
  
  matrix<lower=0, upper=1>[T, K] X;
}
parameters {

  real alpha_all;
  vector[3] alpha;
  vector[3] year_slope;
  vector[K] beta_month;
  vector[N_years] year_ind[3];
  
  vector<lower=0.001>[3] sigma;
  //real<lower=0.001> sigma_beta;
  
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
  //sigma ~ cauchy(0,5);
  //beta_t ~ normal(0, 5);
  //beta_re ~ normal(0, 3);
  
  for (t in 1:T){
//     mu_tt[t] ~ normal(alpha[1] + beta_slope[1] * t/T + beta_quad[1] * t * t * 1.0/T * 1.0/T + X[t] * beta[1] + X[t] * beta_month , sigma[1]);
//     mu_pd[t] ~ normal(alpha[2] + beta_slope[2] * t/T + beta_quad[2] *  t * t * 1.0/T * 1.0/T + X[t] * beta[2] + X[t] * beta_month, sigma[2]);
//     mu_gr[t] ~ normal(alpha[3] + beta_slope[3] * t/T + beta_quad[3] * t * t * 1.0/T * 1.0/T + X[t] * beta[3] + X[t] * beta_month, sigma[3]);
//     
    mu_tt[t] ~ normal(alpha_all + alpha[1] + year_slope[1] * year[t] + year_ind[1, year[t]] + X[t] * beta_month , sigma[1]);
    mu_pd[t] ~ normal(alpha_all + alpha[2] + year_slope[2] * year[t] + year_ind[2, year[t]] + X[t] * beta_month, sigma[2]);
    mu_gr[t] ~ normal(alpha_all + alpha[3] + year_slope[3] * year[t] + year_ind[3, year[t]] + X[t] * beta_month, sigma[3]);
  }

//    for(i in 1:3){
//      //alpha[i] ~ normal(alpha_all, sigma_alpha);
//      beta[i]  ~ normal(beta_all, sigma_beta);
//      //beta_t[i]  ~ normal(beta_t_all, sigma_beta_t);
//    }

  // for(i in 1:3){
  //    alpha[i] <- alpha_all + ;
  //    beta[i]  ~ normal(beta_all, sigma_beta);
  //    //beta_t[i]  ~ normal(beta_t_all, sigma_beta_t);
  //  }

  for (t in 1:T){
    y_tt[t] ~ binomial(N_tt[t], theta_tt[t]);
    y_pd[t] ~ binomial(N_pd[t], theta_pd[t]);
    y_gr[t] ~ binomial(N_gr[t], theta_gr[t]);
  }
}