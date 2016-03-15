library(rstan)

all_dat = readRDS('../data/all_melt.RDS')
dat = all_dat[,c(1,2,3,7,8, 4)]

tots = aggregate(value ~ date + class, dat, sum)

int = dat[dat$int==TRUE,c('class', 'date', 'month', 'year', 'value' )]
dat_org = merge(int, tots, by = c('date', 'class'))
colnames(dat_org)[5:6] = c('int', 'total')
dat_org = data.frame(dat_org, nint = dat_org$total - dat_org$int)
levels(dat_org$class) <- c('GR', 'PD', 'TT')


######################################################################################################################################

gr_dat = dat_org[which(dat_org$class == 'GR'),]
gr_dat = gr_dat[order(gr_dat$date),]
gr_dat$nint_p = gr_dat$nint/gr_dat$total

pd_dat = dat_org[which(dat_org$class == 'PD'),]
pd_dat = pd_dat[order(pd_dat$date),]
pd_dat$nint_p = pd_dat$nint/pd_dat$total

tt_dat = dat_org[which(dat_org$class == 'TT'),]
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

fit_m1 <- stan(file    = 'ecolog_m1.stan', 
               data    = all, 
               iter    = 200, 
               chains  = 1, 
               control = list(max_treedepth=12))
post_m1   <- extract(fit_m1)

plot(fit_m1, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
plot(fit_m1, pars=paste0('beta_t[', seq(1, T), ']'))
plot(fit_m1, pars=paste0('beta_tt[', seq(1, 12), ']'))
plot(fit_m1, pars=paste0('beta_pd[', seq(1, 12), ']'))
plot(fit_m1, pars=paste0('beta_gr[', seq(1, 12), ']'))

plot(date, colMeans(post[['beta_t']]), type='l')
plot(month,colMeans(post[['beta_t']]))
acf(colMeans(post[['beta_t']]))

pdf(file='figures/ecolog_m1.pdf', height=10, width=12)
par(mfrow=c(1,1))
plot(date, tt$y/tt$total, type='l')
lines(date, colMeans(post_m1[['theta_tt']]), col='blue')

plot(date, pd$y/pd$total, type='l')
lines(date, colMeans(post_m1[['theta_pd']]), col='blue')

plot(date, gr$y/gr$total, type='l')
lines(date, colMeans(post_m1[['theta_gr']]), col='blue')
dev.off()

#####################################################################################################################################

fit_m2 <- stan(file = 'ecolog_m2.stan', data = all, 
            iter = 200, chains = 1, control = list(max_treedepth=13))
post_m2   <- extract(fit_m2)

plot(fit_m2, pars=c('alpha_tt', 'alpha_pd', 'alpha_gr'))
plot(fit_m2, pars=c('alpha_tt1', 'alpha_pd1', 'alpha_gr1'))

plot(fit_m2, pars=paste0('sigma[', seq(1, 3), ']'))
plot(fit_m2, pars=paste0('beta_tt[', seq(1, 12), ']'))
plot(fit_m2, pars=paste0('beta_pd[', seq(1, 12), ']'))
plot(fit_m2, pars=paste0('beta_gr[', seq(1, 12), ']'))

pdf(file='figures/ecolog_m2.pdf', height=10, width=12)
par(mfrow=c(1,1))
plot(date, tt$y/tt$total, type='l')
lines(date, colMeans(post_m2[['theta_tt']]), col='blue')

plot(date, pd$y/pd$total, type='l')
lines(date, colMeans(post_m2[['theta_pd']]), col='blue')

plot(date, gr$y/gr$total, type='l')
lines(date, colMeans(post_m2[['theta_gr']]), col='blue')
dev.off()











# 
# ######################################################################################################################################
# gr_dat = dat_org[which(dat_org$class == 'GR'),]
# gr_dat = gr_dat[order(gr_dat$date),]
# gr_dat$nint_p = gr_dat$nint/gr_dat$total
# 
# date  = gr_dat$date
# month = gr_dat$month
# year  = gr_dat$year
# T     = length(date)
# 
# 
# X <- matrix(0, nrow=length(month), ncol=length(unique(month)))
# X[cbind(seq_along(month), month)] <- 1
# 
# # y = matrix(cbind(gr_dat$int, gr_dat$total-gr_dat$int), ncol=2)
# # X = matrix(rep(1, nrow(y)))
# 
# gr <- list(T = T, 
#            K = 12,
#            date = date,
#            month = month,
#            year  = year,
#            y     = gr_dat$int,
#            total = gr_dat$total)
# 
# fit_gr <- stan(file = 'gr_bin_mes.stan', data = gr, 
#                iter = 100, chains = 1, control = list(max_treedepth=12, adapt_delta=0.9))
# 
# fit  <- fit_gr
# post <- extract(fit_gr)
# 
# pdf(file='figures/gr_ar_bin.pdf', height=10, width=12)
# plot(fit)
# plot(fit, pars=c('sigma'))
# plot(fit, pars=c('alpha'))
# plot(fit, pars=c('beta[1]', 'beta[2]','beta[3]','beta[4]','beta[5]',
#                  'beta[6]','beta[7]','beta[8]','beta[9]','beta[10]',
#                  'beta[11]','beta[12]'))
# 
# # plot(post[['sigma']], type='l')
# theta = post[['theta']]
# 
# library(boot)
# plot(date, 1-gr_dat$nint_p, type='l')
# lines(date, colMeans(post[['theta']]), col='blue')
# dev.off()
# 
# ######################################################################################################################################
# 
# pd_dat = dat_org[which(dat_org$class == 'PD'),]
# pd_dat = pd_dat[order(pd_dat$date),]
# pd_dat$nint_p = pd_dat$nint/pd_dat$total
# 
# date  = pd_dat$date
# month = pd_dat$month
# year  = pd_dat$year
# T     = length(date)
# 
# 
# X <- matrix(0, nrow=length(month), ncol=length(unique(month)))
# X[cbind(seq_along(month), month)] <- 1
# 
# # y = matrix(cbind(gr_dat$int, gr_dat$total-gr_dat$int), ncol=2)
# # X = matrix(rep(1, nrow(y)))
# 
# gr <- list(T = T, 
#            K = 12,
#            date = date,
#            month = month,
#            year  = year,
#            y     = gr_dat$int,
#            total = gr_dat$total)
# 
# pd <- list(T = T, 
#            K = 12,
#            date = date,
#            month = month,
#            year  = year,
#            y     = pd_dat$int,
#            total = pd_dat$total)
# 
# fit <- stan(file = 'gr_bin_mes.stan', data = pd, 
#             iter = 200, chains = 1, control = list(max_treedepth=12))
# post <- extract(fit)
# fit_pd <- fit
# 
# pdf(file='figures/pd_ar_bin.pdf', height=10, width=12)
# # plot(fit, pars=c('sigma'))
# # plot(fit, pars=c('beta_t'))
# plot(fit_pd, pars=c('alpha'))
# plot(fit_pd, pars=c('beta[1]', 'beta[2]','beta[3]','beta[4]','beta[5]',
#                     'beta[6]','beta[7]','beta[8]','beta[9]','beta[10]',
#                     'beta[11]','beta[12]'))
# 
# # plot(post[['sigma']], type='l')
# theta = post[['theta']]
# 
# # library(boot)
# plot(date, pd$y/pd$total, type='l')
# lines(date, colMeans(post[['theta']]), col='blue')
# dev.off()
# 
# ######################################################################################################################################
# 
# tt_dat = dat_org[which(dat_org$class == 'TT'),]
# tt_dat = tt_dat[order(tt_dat$date),]
# tt_dat$nint_p = tt_dat$nint/tt_dat$total
# 
# date  = tt_dat$date
# month = tt_dat$month
# year  = tt_dat$year
# T     = length(date)
# 
# 
# X <- matrix(0, nrow=length(month), ncol=length(unique(month)))
# X[cbind(seq_along(month), month)] <- 1
# 
# tt <- list(T = T, 
#            K = 12,
#            date = date,
#            month = month,
#            year  = year,
#            y     = tt_dat$int,
#            total = tt_dat$total)
# 
# fit <- stan(file = 'gr_bin_mes.stan', data = tt, 
#             iter = 200, chains = 1, control = list(max_treedepth=12))
# fit_tt <- fit
# post   <- extract(fit_tt)
# 
# pdf(file='figures/tt_ar_bin.pdf', height=10, width=12)
# # plot(fit, pars=c('sigma'))
# # plot(fit, pars=c('beta_t'))
# plot(fit_tt, pars=c('alpha'))
# plot(fit_tt, pars=c('beta[1]', 'beta[2]','beta[3]','beta[4]','beta[5]',
#                     'beta[6]','beta[7]','beta[8]','beta[9]','beta[10]',
#                     'beta[11]','beta[12]'))
# 
# # plot(post[['sigma']], type='l')
# theta = post[['theta']]
# 
# # library(boot)
# plot(date, tt$y/tt$total, type='l')
# lines(date, colMeans(post[['theta']]), col='blue')
# dev.off()
# 
