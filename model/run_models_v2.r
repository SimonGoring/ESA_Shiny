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
  
  #ggsave(paste0('figures/month_ind_', suff, '.pdf'))
  
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
    geom_pointrange(data=limits, aes(x=factor(month), ymin=ymin, y=y, ymax=ymax)) + 
    ggtitle('Month shared')
  
  #ggsave(paste0('figures/month_shared_', suff, '.pdf'))
  
}


# # only works when there are individual month effects
# plot_time_shared <- function(time_shared, suff){
#   # month_ind = post_m2b$month_ind
#   time_shared = apply(time_shared, c(2), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
#   time_shared = melt(time_shared)
#   
#   colnames(time_shared) = c('quant', 'time', 'value')
#   
#   ymin = subset(time_shared, quant == c('2.5%'))$value
#   ymax = subset(time_shared, quant == c('97.5%'))$value
#   y = subset(time_shared, quant == c('50%'))$value
#   limits = data.frame(subset(time_shared, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
#   # limits = aes(ymin=ymin, ymax=ymax)
#   
#   ggplot() + geom_point(data=subset(time_shared, quant == c('50%')), aes(x=factor(time), y=value), size=3) +
#     geom_pointrange(data=limits, aes(x=factor(time), ymin=ymin, y=y, ymax=ymax)) + 
#     ggtitle('Time shared')
#   
#   #ggsave(paste0('figures/time_shared_', suff, '.pdf'))
#   
# }

# only works when there are individual month effects
plot_time_shared <- function(post, suff){
  time_shared = post$time_all
  time_shared = apply(time_shared, c(2), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
  time_shared = melt(time_shared)
  
  colnames(time_shared) = c('quant', 'time', 'value')
  
  ymin = subset(time_shared, quant == c('2.5%'))$value
  ymax = subset(time_shared, quant == c('97.5%'))$value
  y = subset(time_shared, quant == c('50%'))$value
  limits = data.frame(subset(time_shared, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
  # limits = aes(ymin=ymin, ymax=ymax)
  
  ggplot() + geom_point(data=subset(time_shared, quant == c('50%')), aes(x=factor(time), y=value), size=3) +
    geom_pointrange(data=limits, aes(x=factor(time), ymin=ymin, y=y, ymax=ymax)) + 
    ggtitle('Time shared')
  
  #ggsave(paste0('figures/time_shared_', suff, '.pdf'))
  
}

# only works when there are individual month effects
plot_time_individual <- function(post, suff){
  time_ind = post$time_ind
  time_ind = apply(time_ind, c(2,3), function(x) quantile(x, probs=c(0.025, 0.5, 0.975)))
  time_ind = melt(time_ind)
  
  colnames(time_ind) = c('quant', 'class', 'time', 'value')
  
  ymin = subset(time_ind, quant == c('2.5%'))$value
  ymax = subset(time_ind, quant == c('97.5%'))$value
  y = subset(time_ind, quant == c('50%'))$value
  limits = data.frame(subset(time_ind, quant == c('2.5%'))[,1:3], ymin=ymin, ymax=ymax, y=y)
  # limits = aes(ymin=ymin, ymax=ymax)
  
  ggplot() + geom_point(data=subset(time_ind, quant == c('50%')), aes(x=factor(time), y=value), size=3) +
    geom_pointrange(data=limits, aes(x=factor(time), ymin=ymin, y=y, ymax=ymax)) + 
    ggtitle('Time ind')
  
  #ggsave(paste0('figures/time_shared_', suff, '.pdf'))
  
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
    geom_pointrange(data=limits, aes(x=class, ymin=ymin, y=y, ymax=ymax)) + 
    ggtitle('Constants')
  
  #ggsave(paste0('figures/alpha_', suff, '.pdf'))
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
    geom_pointrange(data=limits, aes(x=class, ymin=ymin, y=y, ymax=ymax))  + 
    ggtitle('Time slope')
  
  #ggsave(paste0('figures/time_slopes_', suff, '.pdf'))
}

plot_preds <- function(all, post, suff){
  
  dat = rbind(data.frame(date=date, value=colMeans(post[['theta_tt']]), class=rep('Tenure Track'), type='pred'),
              data.frame(date=date, value=colMeans(post[['theta_pd']]), class=rep('Postdoctoral'), type='pred'),
              data.frame(date=date, value=colMeans(post[['theta_gr']]), class=rep('Graduate'), type='pred'),
              data.frame(date=date, value=all$y_tt/all$N_tt, class=rep('Tenure Track'), type='data'),
              data.frame(date=date, value=all$y_pd/all$N_pd, class=rep('Postdoctoral'), type='data'),
              data.frame(date=date, value=all$y_gr/all$N_gr, class=rep('Graduate'), type='data'))
  
  ggplot() + geom_line(data=dat, aes(x=date, y=value, colour=type)) + facet_grid(class~.) + 
    ggtitle('Interdisciplinary jobs by class')
  
  #ggsave(paste0('figures/preds_', suff, '.pdf'))
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

pdf(file=paste0('figures/model_4.pdf'))
plot_month_ind(post_m4$month_ind, '4')
plot_alphas(post_m4$alpha_ind, post_m4$alpha_all, '4')
plot_time_slopes(post_m4$time_slope_ind, post_m4$time_slope_all, '4')
plot_preds(all, post_m4, '4')
plot_time_shared(post_m4, '4')
dev.off()

pdf(file=paste0('figures/model_4a.pdf'))
plot_month_ind(post_m4a$month_ind, '4a')
plot_alphas(post_m4a$alpha_ind, post_m4a$alpha_all, '4a')
plot_time_slopes(post_m4a$time_slope_ind, post_m4a$time_slope_all, '4a')
plot_preds(all, post_m4a, '4a')
plot_time_individual(post_m4a, '4a')
dev.off()

pdf(file=paste0('figures/model_4b.pdf'))
plot_month_ind(post_m4b$month_ind, '4b')
plot_alphas(post_m4b$alpha_ind, post_m4b$alpha_all, '4b')
plot_time_slopes(post_m4b$time_slope_ind, post_m4b$time_slope_all, '4b')
plot_preds(all, post_m4b, '4b')
dev.off()

pdf(file=paste0('figures/model_4c.pdf'))
plot_alphas(post_m4c$alpha_ind, post_m4c$alpha_all, '4c')
plot_time_slopes(post_m4c$time_slope_ind, post_m4c$time_slope_all, '4c')
plot_preds(all, post_m4c, '4c')
dev.off()
