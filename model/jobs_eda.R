library(astsa)

all_dat = readRDS('data/all_melt.RDS')

dat = all_dat[,c(1,2,3,4)]
# dat = data.frame(dat, date=all_dat$year+all_dat$week_dec)

tots = aggregate(value ~ date + class, dat, sum)

int = dat[dat$int==TRUE,c('class', 'date', 'value')]
dat_org = merge(int, tots, by = c('date', 'class'))
colnames(dat_org)[3:4] = c('int', 'total')
dat_org = data.frame(dat_org, nint = dat_org$total - dat_org$int)

# totals
ggplot(dat_org) + geom_line(data=dat_org, aes(x=date, y=total)) + facet_grid(class~.)
# int only
ggplot(dat_org) + geom_line(data=dat_org, aes(x=date, y=int)) + facet_grid(class~.)
# nint only
ggplot(dat_org) + geom_line(data=dat_org, aes(x=date, y=total-int)) + facet_grid(class~.)
# prop nint to total
ggplot(dat_org) + geom_line(data=dat_org, aes(x=date, y=(total-int)/total)) + facet_grid(class~.)
# prop int to total
ggplot(dat_org) + geom_line(data=dat_org, aes(x=date, y=(int)/total)) + facet_grid(class~.)

# prop nint to total
ggplot(dat_org) + geom_line(data=dat_org, aes(x=date, y=(total-int)/total, colour=class)) #+ facet_grid(class~.)
# prop int to total
ggplot(dat_org) + geom_line(data=dat_org, aes(x=date, y=int/total, colour=class)) #+ facet_grid(class~.)

levels(dat_org$class) <- c('GR', 'PD', 'TT')

#####################################################################################################################
## GR
#####################################################################################################################
gr_dat = dat_org[which(dat_org$class == 'GR'),]
gr_dat = gr_dat[order(gr_dat$date),]

gr_dat$nint_p = gr_dat$nint/gr_dat$total

plot(gr_dat$date, gr_dat$nint_p, type='l') # decreasing trend
acf(gr_dat$nint_p, lag.max=40) # see the annual periodicity
pacf(gr_dat$nint_p, lag.max=40) #
lag.plot(gr_dat$nint_p, lags=12)

FF = abs(fft(gr_dat$nint_p)/sqrt(180))^2
P = (4/180)*FF[1:65]  # only need the first (n/2)+1 values of the FFT result.
f = (0:64)/180        # creates harmonic frequencies
t = 1/f
plot(f, P, type="l")               # periodogram
plot(t, P, type="l", xlim=c(0,16), ylim=c(0, 0.0012)) # periodogram shows peaks at 6, 12
abline(v=12, col='blue', lty= 2)
abline(v=6, col='blue', lty=2)

# 12th differences to look at non-sesonal behavior
gr12 = diff(gr_dat$nint_p, 12)
acf(gr12, lag.max=40) # spike at lag 12
pacf(gr12, lag.max=40) # pacf tapers at multiples of 12

m1 <- sarima(gr_dat$nint_p, 0, 0, 0, 0, 1, 1, 12)
plot(resid(m1$fit))
sarima.for(gr_dat$nint_p, 40, 1, 1, 0, 0, 1, 1, 12)
# sarima.for(gr_p, -24, 2, 0, 0, 0, 1, 1, 12)

#####################################################################################################################
## PD
#####################################################################################################################
pd_dat = dat_org[which(dat_org$class == 'PD'),]
pd_dat = pd_dat[order(pd_dat$date),]

pd_dat$nint_p = pd_dat$nint/pd_dat$total

plot(pd_dat$date, pd_dat$nint_p, type='l') 
acf(pd_dat$nint_p, lag.max=40) 
pacf(pd_dat$nint_p, lag.max=40)
lag.plot(pd_dat$nint_p, lags=12)

#####################################################################################################################
## TT
#####################################################################################################################
tt_dat = dat_org[which(dat_org$class == 'TT'),]
tt_dat = tt_dat[order(tt_dat$date),]

tt_dat$nint_p = tt_dat$nint/tt_dat$total

plot(tt_dat$date, tt_dat$nint_p, type='l') 
acf(tt_dat$nint_p, lag.max=40) 
pacf(tt_dat$nint_p, lag.max=40)
lag.plot(tt_dat$nint_p, lags=12)