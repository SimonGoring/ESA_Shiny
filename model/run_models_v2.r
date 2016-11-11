library(rstan)
library(reshape2)

all_dat = readRDS('../data/all_melt.RDS')

all_dat = all_dat[which(all_dat$class %in% c('Tenure Track', 'Postdoctoral', 'Graduate')),]
all_dat$class = droplevels(all_dat$class)
all_dat$year= as.numeric(substr(all_dat$date, 1, 4))

tots = aggregate(value ~ date + class, all_dat, sum)

int = all_dat[all_dat$int==TRUE,c('class', 'date', 'month', 'year', 'value' )]
dat_org = merge(int, tots, by = c('date', 'class'))
colnames(dat_org)[5:6] = c('int', 'total')
dat_org = data.frame(dat_org, nint = dat_org$total - dat_org$int)
# levels(dat_org$class) <- c('Graduate', 'Postdoctoral', 'Tenure Track')

classes = c('Tenure Track', 'Postdoctoral', 'Graduate')
years   = seq(2000, 2015)

######################################################################################################################################

gr_dat = dat_org[which(dat_org$class == 'Graduate'),]
gr_dat = gr_dat[order(gr_dat$date),]
gr_dat$nint_p = gr_dat$nint/gr_dat$total

pd_dat = dat_org[which(dat_org$class == 'Postdoctoral'),]
pd_dat = pd_dat[order(pd_dat$date),]
pd_dat$nint_p = pd_dat$nint/pd_dat$total

tt_dat = dat_org[which(dat_org$class == 'Tenure Track'),]
tt_dat = tt_dat[order(tt_dat$date),]
tt_dat$nint_p = tt_dat$nint/tt_dat$total

date  = tt_dat$date
month = tt_dat$month
year  = tt_dat$year
T     = length(date)

X <- matrix(0, nrow=length(month), ncol=length(unique(month)))
X[cbind(seq_along(month), month)] <- 1

all <- list(T = T, 
            N_years = length(unique(year)),
            N_months = length(unique(month)),
            K = 12,
            date = date,
            month = month,
            year  = year-2000+1,
            y_tt  = tt_dat$int,
            N_tt  = tt_dat$total,
            y_gr  = gr_dat$int,
            N_gr  = gr_dat$total,
            y_pd  = pd_dat$int,
            N_pd  = pd_dat$total)

########################################################################################################################################
## plotting functions
########################################################################################################################################

# only works when there are individual month effects
plot_month_ind <- function(month_ind, suff){
  # month_ind = post_m2b$month_ind
  month_ind = apply(month_ind, c(2,3), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
  month_ind = melt(month_ind)
  
  colnames(month_ind) = c('quant', 'class', 'month', 'value')
  
  month_ind$class = classes[month_ind$class]
  
  ymin = subset(month_ind, quant == c('2.5%'))$value
  ymax = subset(month_ind, quant == c('97.5%'))$value
  y = subset(month_ind, quant == c('50%'))$value
  limits = data.frame(subset(month_ind, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
  # limits = aes(ymin=ymin, ymax=ymax)
  
  ggplot() + geom_point(data=subset(month_ind, quant == c('50%')), aes(x=factor(month), y=value, colour=factor(class)), size=3) +
    geom_pointrange(data=limits, aes(x=factor(month), ymin=ymin, y=y, ymax=ymax, colour=factor(class))) 
  
  ggsave(paste0('figures/month_ind_', suff, '.pdf'))
  
}

# only works when there are individual month effects
plot_month_shared <- function(month_shared, suff){
  # month_ind = post_m2b$month_ind
  month_shared = apply(month_shared, c(2), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
  month_shared = melt(month_shared)
  
  colnames(month_shared) = c('quant', 'month', 'value')
  
  ymin = subset(month_shared, quant == c('2.5%'))$value
  ymax = subset(month_shared, quant == c('97.5%'))$value
  y = subset(month_shared, quant == c('50%'))$value
  limits = data.frame(subset(month_shared, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
  # limits = aes(ymin=ymin, ymax=ymax)
  
  ggplot() + geom_point(data=subset(month_shared, quant == c('50%')), aes(x=factor(month), y=value), size=3) +
    geom_pointrange(data=limits, aes(x=factor(month), ymin=ymin, y=y, ymax=ymax)) 
  
  ggsave(paste0('figures/month_shared_', suff, '.pdf'))
  
}


# only works when there are individual month effects
plot_time_shared <- function(time_shared, suff){
  # month_ind = post_m2b$month_ind
  time_shared = apply(time_shared, c(2), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
  time_shared = melt(time_shared)
  
  colnames(time_shared) = c('quant', 'time', 'value')
  
  ymin = subset(time_shared, quant == c('2.5%'))$value
  ymax = subset(time_shared, quant == c('97.5%'))$value
  y = subset(time_shared, quant == c('50%'))$value
  limits = data.frame(subset(time_shared, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
  # limits = aes(ymin=ymin, ymax=ymax)
  
  ggplot() + geom_point(data=subset(time_shared, quant == c('50%')), aes(x=factor(time), y=value), size=3) +
    geom_pointrange(data=limits, aes(x=factor(time), ymin=ymin, y=y, ymax=ymax)) 
  
  ggsave(paste0('figures/time_shared_', suff, '.pdf'))
  
}

plot_alphas <- function(alpha_ind, alpha_all, suff){
  
  alpha_ind = apply(alpha_ind, c(2), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
  alpha_ind = melt(alpha_ind)
  
  colnames(alpha_ind) = c('quant', 'class', 'value')
  
  alpha_ind$class = classes[alpha_ind$class]
  
  alpha_all = quantile(alpha_all, probs=c(0.025, 0.5, 0.975))
  alpha_all = melt(alpha_all)
  
  alpha_all = data.frame(quant=rownames(alpha_all), class=rep('all'), value=alpha_all)
  
  alpha = rbind(alpha_ind, alpha_all)
  
  ymin = subset(alpha, quant == c('2.5%'))$value
  ymax = subset(alpha, quant == c('97.5%'))$value
  y = subset(alpha, quant == c('50%'))$value
  limits = data.frame(subset(alpha, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
  # limits = aes(ymin=ymin, ymax=ymax)
  
  ggplot() + geom_point(data=subset(alpha, quant == c('50%')), aes(x=class, y=value), size=3) +
    geom_pointrange(data=limits, aes(x=class, ymin=ymin, y=y, ymax=ymax)) 
  
  ggsave(paste0('figures/alpha_', suff, '.pdf'))
}

plot_time_slopes <- function(time_slope_ind, time_slope_all, suff){
  
  time_slope_ind = apply(time_slope_ind, c(2), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
  time_slope_ind = melt(time_slope_ind)
  
  colnames(time_slope_ind) = c('quant', 'class', 'value')
  
  time_slope_ind$class = classes[time_slope_ind$class]
  
  time_slope_all = quantile(time_slope_all, probs=c(0.025, 0.5, 0.975))
  time_slope_all = melt(time_slope_all)
  
  time_slope_all = data.frame(quant=rownames(time_slope_all), class=rep('all'), value=time_slope_all)
  
  alpha = rbind(time_slope_ind, time_slope_all)
  
  ymin = subset(alpha, quant == c('2.5%'))$value
  ymax = subset(alpha, quant == c('97.5%'))$value
  y = subset(alpha, quant == c('50%'))$value
  limits = data.frame(subset(alpha, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
  # limits = aes(ymin=ymin, ymax=ymax)
  
  ggplot() + geom_point(data=subset(alpha, quant == c('50%')), aes(x=class, y=value), size=3) +
    geom_pointrange(data=limits, aes(x=class, ymin=ymin, y=y, ymax=ymax)) 
  
  ggsave(paste0('figures/time_slopes_', suff, '.pdf'))
}

plot_preds <- function(all, post, suff){
  
  dat = rbind(data.frame(date=date, value=colMeans(post[['theta_tt']]), class=rep('Tenure Track'), type='pred'),
              data.frame(date=date, value=colMeans(post[['theta_pd']]), class=rep('Postdoctoral'), type='pred'),
              data.frame(date=date, value=colMeans(post[['theta_gr']]), class=rep('Graduate'), type='pred'),
              data.frame(date=date, value=all$y_tt/all$N_tt, class=rep('Tenure Track'), type='data'),
              data.frame(date=date, value=all$y_pd/all$N_pd, class=rep('Postdoctoral'), type='data'),
              data.frame(date=date, value=all$y_gr/all$N_gr, class=rep('Graduate'), type='data'))
  
  ggplot() + geom_line(data=dat, aes(x=date, y=value, colour=type)) + facet_grid(class~.)
  
  ggsave(paste0('figures/preds_', suff, '.pdf'))
}

########################################################################################################################################
## run all the model 2s
########################################################################################################################################

fit_m2 <- stan(file    = 'model2.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m2   <- extract(fit_m2)

fit_m2a <- stan(file    = 'model2a.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m2a   <- extract(fit_m2a)

fit_m2b <- stan(file    = 'model2b.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m2b   <- extract(fit_m2b)


fit_m2c <- stan(file    = 'model2c.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m2c   <- extract(fit_m2c)

########################################################################################################################################
## plot parameters estimates 
########################################################################################################################################

plot_month_ind(post_m2$month_ind, '2')
plot_month_ind(post_m2a$month_ind, '2a')
plot_month_ind(post_m2b$month_ind, '2b')

# plot_month_shared(post_m2$month_all, '2')
# plot_month_shared(post_m2a$month_all, '2a')
# plot_month_shared(post_m2b$month_all, '2b')
# plot_month_shared(post_m2c$month_all, '2c')

plot_alphas(post_m2$alpha_ind, post_m2$alpha_all, '2')
plot_alphas(post_m2a$alpha_ind, post_m2a$alpha_all, '2a')
plot_alphas(post_m2b$alpha_ind, post_m2b$alpha_all, '2b')
plot_alphas(post_m2c$alpha_ind, post_m2c$alpha_all, '2c')

plot_time_slopes(post_m2$time_slope_ind, post_m2$time_slope_all, '2')
plot_time_slopes(post_m2a$time_slope_ind, post_m2a$time_slope_all, '2a')
plot_time_slopes(post_m2b$time_slope_ind, post_m2b$time_slope_all, '2b')
plot_time_slopes(post_m2c$time_slope_ind, post_m2c$time_slope_all, '2c')

plot_time_shared(post_m2$time_shared, '2')

########################################################################################################################################
## run all the model 4s
########################################################################################################################################

fit_m4 <- stan(file    = 'model4.stan', 
               data    = all, 
               iter    = 200, 
               chains  = 1, 
               control = list(max_treedepth=12))
post_m4   <- extract(fit_m4)

fit_m4a <- stan(file    = 'model4a.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m4a   <- extract(fit_m4a)

fit_m4b <- stan(file    = 'model4b.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m4b   <- extract(fit_m4b)


fit_m4c <- stan(file    = 'model4c.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m4c   <- extract(fit_m4c)

########################################################################################################################################
## plot parameters estimates 
########################################################################################################################################

plot_month_ind(post_m4$month_ind, '4')
plot_month_ind(post_m4a$month_ind, '4a')
plot_month_ind(post_m4b$month_ind, '4b')

plot_alphas(post_m4$alpha_ind, post_m4$alpha_all, '4')
plot_alphas(post_m4a$alpha_ind, post_m4a$alpha_all, '4a')
plot_alphas(post_m4b$alpha_ind, post_m4b$alpha_all, '4b')
plot_alphas(post_m4c$alpha_ind, post_m4c$alpha_all, '4c')

plot_time_slopes(post_m4$time_slope_ind, post_m4$time_slope_all, '4')
plot_time_slopes(post_m4a$time_slope_ind, post_m4a$time_slope_all, '4a')
plot_time_slopes(post_m4b$time_slope_ind, post_m4b$time_slope_all, '4b')
plot_time_slopes(post_m4c$time_slope_ind, post_m4c$time_slope_all, '4c')

plot_preds(all, post_m4, '4')
plot_preds(all, post_m4a, '4a')
plot_preds(all, post_m4b, '4b')
plot_preds(all, post_m4c, '4c')
# plot_time_shared(post_m4$time_shared, '4')
###################################################################################################################

pdf(file='figures/model2.pdf', width=10)
plot(fit_m2, pars=c('alpha_all', 'alpha'))
plot(fit_m2, pars=paste0('time_effect'))

library(reshape2)
beta = post_m2$beta
beta_mean = apply(beta, c(2,3), mean)
beta_mean = melt(beta_mean)

ggplot(beta_mean) + geom_point(aes(x=value, y=Var2, colour=factor(Var1)), size=3)

plot(fit_m2, pars=paste0('beta_month'))

par(mfrow=c(1,1))
plot(date, all$y_tt/all$N_tt, type='l')
lines(date, colMeans(post_m2[['theta_tt']]), col='blue')

plot(date,  all$y_pd/all$N_pd, type='l')
lines(date, colMeans(post_m2[['theta_pd']]), col='blue')

plot(date,  all$y_gr/all$N_gr, type='l')
lines(date, colMeans(post_m2[['theta_gr']]), col='blue')
dev.off()



########################################################################################################################################
##
########################################################################################################################################



pdf(file='figures/model2a.pdf', width=10)
plot(fit_m2a, pars=c('alpha_all', 'alpha_ind'))

library(reshape2)
beta = post_m2a$beta
beta_mean = apply(beta, c(2,3), mean)
beta_mean = melt(beta_mean)

ggplot(beta_mean) + geom_point(aes(x=value, y=Var2, colour=factor(Var1)), size=3)

time_e = post_m2a$time_effect
time_e_mean = apply(time_e, c(2,3), mean)
time_e_mean = melt(time_e_mean)
ggplot(time_e_mean) + geom_point(aes(x=Var2, y=value, colour=factor(Var1)), size=3)


plot(fit_m2a, pars=paste0('beta_month'))

par(mfrow=c(1,1))
plot(date, all$y_tt/all$N_tt, type='l')
lines(date, colMeans(post_m2a[['theta_tt']]), col='blue')

plot(date,  all$y_pd/all$N_pd, type='l')
lines(date, colMeans(post_m2a[['theta_pd']]), col='blue')

plot(date,  all$y_gr/all$N_gr, type='l')
lines(date, colMeans(post_m2a[['theta_gr']]), col='blue')
dev.off()

plot_month_ind(post_m2a$month_ind)


########################################################################################################################################
##
########################################################################################################################################



pdf(file='figures/model2b.pdf', width=10)
plot(fit_m2b, pars=c('alpha_all', 'alpha_ind'))

library(reshape2)
month_ind = post_m2b$month_ind
month_ind = apply(month_ind, c(2,3), mean)
month_ind = melt(month_ind)

ggplot(month_ind) + geom_point(aes(y=value, x=factor(Var2), colour=factor(Var1)), size=3)

time_e = post_m2b$time_effect
time_e_mean = apply(time_e, c(2,3), mean)
time_e_mean = melt(time_e_mean)
ggplot(time_e_mean) + geom_point(aes(x=factor(Var2), y=value, colour=factor(Var1)), size=3)


plot(fit_m2b, pars=paste0('month_all'))

par(mfrow=c(1,1))
plot(date, all$y_tt/all$N_tt, type='l')
lines(date, colMeans(post_m2b[['theta_tt']]), col='blue')

plot(date,  all$y_pd/all$N_pd, type='l')
lines(date, colMeans(post_m2b[['theta_pd']]), col='blue')

plot(date,  all$y_gr/all$N_gr, type='l')
lines(date, colMeans(post_m2b[['theta_gr']]), col='blue')
dev.off()

# plot individual month effects
plot_month_ind(post_m2b$month_ind)

########################################################################################################################################
##
########################################################################################################################################


pdf(file='figures/model1c.pdf', width=10)
plot(fit_m1c, pars=c('alpha_all', 'alpha'))
plot(fit_m1c, pars=c('year_slope'))

library(reshape2)
year_ind = post_m1b$year_ind
year_ind_mean = apply(year_ind, c(2,3), mean)
year_ind_mean = melt(year_ind_mean)
ggplot(year_ind_mean) + geom_point(aes(x=factor(Var2), y=value, colour=factor(Var1)), size=3)

plot(fit_m1c, pars=paste0('beta_month'))

par(mfrow=c(1,1))
plot(date, all$y_tt/all$N_tt, type='l')
lines(date, colMeans(post_m1c[['theta_tt']]), col='blue')

plot(date,  all$y_pd/all$N_pd, type='l')
lines(date, colMeans(post_m1c[['theta_pd']]), col='blue')

plot(date,  all$y_gr/all$N_gr, type='l')
lines(date, colMeans(post_m1c[['theta_gr']]), col='blue')
dev.off()

########################################################################################################################################
##
########################################################################################################################################

fit_m1d <- stan(file    = 'model1d.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m1d   <- extract(fit_m1d)

pdf(file='figures/model1d.pdf', width=10)
plot(fit_m1d, pars=c('alpha_all', 'alpha'))
plot(fit_m1d, pars=c('year_slope'))

beta = post_m1d$beta
beta_mean = apply(beta, c(2,3), mean)
beta_mean = melt(beta_mean)
beta_mean$Var1 = classes[beta_mean$Var1]
ggplot(beta_mean) + geom_point(aes(x=factor(Var2), y=value, colour=factor(Var1)), size=3) + xlab("Month") + ylab("Value")

year_ind = post_m1d$year_ind
year_ind_mean = apply(year_ind, c(2,3), mean)
year_ind_mean = melt(year_ind_mean)
year_ind_mean$Var2 = years[year_ind_mean$Var2]
year_ind_mean$Var1 = classes[year_ind_mean$Var1]
ggplot(year_ind_mean) + geom_point(aes(x=factor(Var2), y=value, colour=factor(Var1)), size=3) + xlab("Year")

plot(fit_m1d, pars=paste0('beta_month'))

par(mfrow=c(1,1))
plot(date, all$y_tt/all$N_tt, type='l')
lines(date, colMeans(post_m1d[['theta_tt']]), col='blue')

plot(date,  all$y_pd/all$N_pd, type='l')
lines(date, colMeans(post_m1d[['theta_pd']]), col='blue')

plot(date,  all$y_gr/all$N_gr, type='l')
lines(date, colMeans(post_m1d[['theta_gr']]), col='blue')
dev.off()

########################################################################################################################################
##
########################################################################################################################################

fit_m1e <- stan(file    = 'model1e.stan', 
                data    = all, 
                iter    = 200, 
                chains  = 1, 
                control = list(max_treedepth=12))
post_m1e   <- extract(fit_m1e)

pdf(file='figures/model1e.pdf', width=10)
plot(fit_m1e, pars=c('alpha_all', 'alpha'))
plot(fit_m1e, pars=c('year_slope'))
plot(fit_m1e, pars=c('year_effect'))

beta = post_m1e$beta
beta_mean = apply(beta, c(2,3), mean)
beta_mean = melt(beta_mean)
beta_mean$Var1 = classes[beta_mean$Var1]
ggplot(beta_mean) + geom_point(aes(x=factor(Var2), y=value, colour=factor(Var1)), size=3) + xlab("Month") + ylab("Value")

par(mfrow=c(1,1))
plot(date, all$y_tt/all$N_tt, type='l')
lines(date, colMeans(post_m1e[['theta_tt']]), col='blue')

plot(date,  all$y_pd/all$N_pd, type='l')
lines(date, colMeans(post_m1e[['theta_pd']]), col='blue')

plot(date,  all$y_gr/all$N_gr, type='l')
lines(date, colMeans(post_m1e[['theta_gr']]), col='blue')
dev.off()
########################################################################################################################################

# fit_m1 <- stan(file    = 'ecolog_m1.stan', 
#                data    = all, 
#                iter    = 200, 
#                chains  = 1, 
#                control = list(max_treedepth=12))
# post_m1   <- extract(fit_m1)
# 
# plot(fit_m1, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
# plot(fit_m1, pars=paste0('beta_t[', seq(1, T), ']'))
# plot(fit_m1, pars=paste0('beta_tt[', seq(1, 12), ']'))
# plot(fit_m1, pars=paste0('beta_pd[', seq(1, 12), ']'))
# plot(fit_m1, pars=paste0('beta_gr[', seq(1, 12), ']'))
# 
# plot(date, colMeans(post_m1[['beta_t']]), type='l')
# plot(month,colMeans(post_m1[['beta_t']]))
# acf(colMeans(post_m1[['beta_t']]))
# plot(post_m1[['beta_t']][,11], type='l')
# 
# pdf(file='figures/ecolog_m1.pdf', height=10, width=12)
# par(mfrow=c(1,1))
# plot(date, tt$y/tt$total, type='l')
# lines(date, colMeans(post_m1[['theta_tt']]), col='blue')
# 
# plot(date, pd$y/pd$total, type='l')
# lines(date, colMeans(post_m1[['theta_pd']]), col='blue')
# 
# plot(date, gr$y/gr$total, type='l')
# lines(date, colMeans(post_m1[['theta_gr']]), col='blue')
# dev.off()
# 
# beta_gr = colMeans(post_m1[['beta_gr']])
# beta_tt = colMeans(post_m1[['beta_tt']])
# beta_pd = colMeans(post_m1[['beta_pd']])
# 
# plot(seq(1,12), beta_gr, ylim=c(52,max(beta_pd)))
# points(seq(1,12), beta_tt, col='blue')
# points(seq(1,12), beta_pd, col='red')
# 
# #####################################################################################################################################
# 
# fit_m2 <- stan(file = 'ecolog_m2.stan', data = all, 
#             iter = 200, chains = 1, control = list(max_treedepth=13))
# post_m2   <- extract(fit_m2)
# 
# plot(fit_m2, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
# plot(fit_m2, pars=c('alpha_tt1', 'alpha_pd1', 'alpha_gr1'))
# 
# plot(fit_m2, pars=paste0('sigma[', seq(1, 3), ']'))
# plot(fit_m2, pars=paste0('beta_tt[', seq(1, 12), ']'))
# plot(fit_m2, pars=paste0('beta_pd[', seq(1, 12), ']'))
# plot(fit_m2, pars=paste0('beta_gr[', seq(1, 12), ']'))
# 
# pdf(file='figures/ecolog_m2.pdf', height=10, width=12)
# par(mfrow=c(1,1))
# plot(date, tt$y/tt$total, type='l')
# lines(date, colMeans(post_m2[['theta_tt']]), col='blue')
# 
# plot(date, pd$y/pd$total, type='l')
# lines(date, colMeans(post_m2[['theta_pd']]), col='blue')
# 
# plot(date, gr$y/gr$total, type='l')
# lines(date, colMeans(post_m2[['theta_gr']]), col='blue')
# dev.off()
# 
# 
# #####################################################################################################################################
# 
# fit_m3 <- stan(file = 'ecolog_m3.stan', data = all, 
#                iter = 200, chains = 1, control = list(max_treedepth=13))
# post_m3   <- extract(fit_m3)
# 
# plot(fit_m3, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
# plot(fit_m3, pars=c('alpha_tt1', 'alpha_pd1', 'alpha_gr1'))
# 
# plot(fit_m3, pars=paste0('sigma[', seq(1, 3), ']'))
# plot(fit_m3, pars=paste0('beta_tt[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_pd[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_gr[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_all[', seq(1, 12), ']'))
# 
# pdf(file='figures/ecolog_m3.pdf', height=10, width=12)
# par(mfrow=c(1,1))
# plot(date, all$y_tt/all$N_tt, type='l')
# lines(date, colMeans(post_m3[['theta_tt']]), col='blue')
# 
# plot(date, all$y_pd/all$N_pd, type='l')
# lines(date, colMeans(post_m3[['theta_pd']]), col='blue')
# 
# plot(date, all$y_gr/all$N_gr, type='l')
# lines(date, colMeans(post_m3[['theta_gr']]), col='blue')
# dev.off()
# 
# 
# plot(date, colMeans(post_m3[['beta_t']]), type='l')
# plot(month,colMeans(post_m3[['beta_t']]))
# acf(colMeans(post_m3[['beta_t']]))
# 
# 
# mes = data.frame(rbind(cbind(colMeans(post_m3$beta_pd), rep('Postdoctoral', 12), seq(1,12)), 
#                        cbind(colMeans(post_m3$beta_gr), rep('Graduate', 12), seq(1,12)), 
#                        cbind(colMeans(post_m3$beta_tt), rep('tt', 12), seq(1,12)))) 
# colnames(mes) = c('value', 'type', 'month')
# mes$value = as.numeric(mes$value)
# 
# mes$month = factor(mes$month, levels=seq(1,12))
# 
# ggplot(data=mes) + geom_point(aes(x=factor(month), y=value, colour=type)) #+ scale_y_continous(breaks=)
# 
# #####################################################################################################################################
# 
# fit_m5 <- stan(file = 'ecolog_m5.stan', data = all, 
#                iter = 200, chains = 1, control = list(max_treedepth=13))
# post_m5   <- extract(fit_m5)
# 
# plot(fit_m5, pars=c('alpha'))
# plot(fit_m5, pars=c('beta'))
# plot(fit_m5, pars=c('beta_t'))
# 
# plot(fit_m5, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
# plot(fit_m3, pars=c('alpha_tt1', 'alpha_pd1', 'alpha_gr1'))
# 
# plot(fit_m3, pars=paste0('sigma[', seq(1, 3), ']'))
# plot(fit_m3, pars=paste0('beta_tt[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_pd[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_gr[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_all[', seq(1, 12), ']'))
# 
# pdf(file='figures/ecolog_m5.pdf', height=10, width=12)
# par(mfrow=c(1,1))
# plot(date, all$y_tt/all$N_tt, type='l')
# lines(date, colMeans(post_m3[['theta_tt']]), col='blue')
# 
# plot(date, all$y_pd/all$N_pd, type='l')
# lines(date, colMeans(post_m3[['theta_pd']]), col='blue')
# 
# plot(date, all$y_gr/all$N_gr, type='l')
# lines(date, colMeans(post_m3[['theta_gr']]), col='blue')
# dev.off()
# 
# 
# plot(date, colMeans(post_m3[['beta_t']]), type='l')
# plot(month,colMeans(post_m3[['beta_t']]))
# acf(colMeans(post_m3[['beta_t']]))
# 
# 
# mes = data.frame(rbind(cbind(colMeans(post_m3$beta_pd), rep('Postdoctoral', 12), seq(1,12)), 
#                        cbind(colMeans(post_m3$beta_gr), rep('Graduate', 12), seq(1,12)), 
#                        cbind(colMeans(post_m3$beta_tt), rep('tt', 12), seq(1,12)))) 
# colnames(mes) = c('value', 'type', 'month')
# mes$value = as.numeric(mes$value)
# 
# mes$month = factor(mes$month, levels=seq(1,12))
# 
# ggplot(data=mes) + geom_point(aes(x=factor(month), y=value, colour=type)) #+ scale_y_continous(breaks=)
# 
# #####################################################################################################################################
# 
# fit_m6 <- stan(file = 'ecolog_m6.stan', data = all, 
#                iter = 200, chains = 1, control = list(max_treedepth=13))
# post_m6   <- extract(fit_m6)
# 
# fit_m7 <- stan(file = 'ecolog_m7.stan', data = all, 
#                iter = 200, chains = 1, control = list(max_treedepth=13))
# post_m7   <- extract(fit_m7)
# 
# plot(fit_m5, pars=c('alpha'))
# plot(fit_m5, pars=c('beta'))
# plot(fit_m5, pars=c('beta_t'))
# 
# plot(fit_m5, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
# plot(fit_m3, pars=c('alpha_tt1', 'alpha_pd1', 'alpha_gr1'))
# 
# plot(fit_m3, pars=paste0('sigma[', seq(1, 3), ']'))
# plot(fit_m3, pars=paste0('beta_tt[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_pd[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_gr[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta_all[', seq(1, 12), ']'))
# 
# pdf(file='figures/ecolog_m5.pdf', height=10, width=12)
# par(mfrow=c(1,1))
# plot(date, all$y_tt/all$N_tt, type='l')
# lines(date, colMeans(post_m3[['theta_tt']]), col='blue')
# 
# plot(date, all$y_pd/all$N_pd, type='l')
# lines(date, colMeans(post_m3[['theta_pd']]), col='blue')
# 
# plot(date, all$y_gr/all$N_gr, type='l')
# lines(date, colMeans(post_m3[['theta_gr']]), col='blue')
# dev.off()
# 
# 
# plot(date, colMeans(post_m3[['beta_t']]), type='l')
# plot(month,colMeans(post_m3[['beta_t']]))
# acf(colMeans(post_m3[['beta_t']]))
# 
# 
# mes = data.frame(rbind(cbind(colMeans(post_m3$beta_pd), rep('Postdoctoral', 12), seq(1,12)), 
#                        cbind(colMeans(post_m3$beta_gr), rep('Graduate', 12), seq(1,12)), 
#                        cbind(colMeans(post_m3$beta_tt), rep('tt', 12), seq(1,12)))) 
# colnames(mes) = c('value', 'type', 'month')
# mes$value = as.numeric(mes$value)
# 
# mes$month = factor(mes$month, levels=seq(1,12))
# 
# ggplot(data=mes) + geom_point(aes(x=factor(month), y=value, colour=type)) #+ scale_y_continous(breaks=)
# 
# 
# #####################################################################################################################################
# 
# sigma = rep(0.1, 3)
# beta_slope = rep(1, 3)
# beta_year  = matrix(rnorm(3*all$N_years, mean=0, sd=0.5)) 
# 
# beta_year
# beta_all
# beta_month
# sigma_beta
# 
# # inits = list()
# 
# fit_m3 <- stan(file    = 'ecolog_m8.stan', 
#                data    = all, 
#                iter    = 200, 
#                chains  = 1, 
#                control = list(max_treedepth=13, adapt_delta=0.9))
# post_m3   <- extract(fit_m3)
# 
# plot(fit_m3, pars='beta')
# plot(fit_m3, pars=paste0('beta_month[', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta[1,', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta[2,', seq(1, 12), ']'))
# plot(fit_m3, pars=paste0('beta[3,', seq(1, 12), ']'))
# 
# plot(fit_m3, pars='alpha')
# plot(fit_m3, pars='beta_slope')
# 
# plot(fit_m3, pars='beta_year')
# 
# 
# # plot(fit_m3, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
# # plot(fit_m3, pars=c('alpha_tt1', 'alpha_pd1', 'alpha_gr1'))
# # 
# # plot(fit_m3, pars=paste0('sigma[', seq(1, 3), ']'))
# # plot(fit_m3, pars=paste0('beta_tt[', seq(1, 12), ']'))
# # plot(fit_m3, pars=paste0('beta_pd[', seq(1, 12), ']'))
# # plot(fit_m3, pars=paste0('beta_gr[', seq(1, 12), ']'))
# 
# pdf(file='figures/ecolog_m8.pdf', height=10, width=12)
# par(mfrow=c(1,1))
# plot(date, all$y_tt/all$N_tt, type='l')
# lines(date, colMeans(post_m3[['theta_tt']]), col='blue')
# 
# plot(date, all$y_pd/all$N_pd, type='l')
# lines(date, colMeans(post_m3[['theta_pd']]), col='blue')
# 
# plot(date, all$y_gr/all$N_gr, type='l')
# lines(date, colMeans(post_m3[['theta_gr']]), col='blue')
# dev.off()
# 
# 
# plot(date, colMeans(post_m3[['beta_re']]), type='l')
# plot(month,colMeans(post[['beta_t']]))
# acf(colMeans(post[['beta_t']]))