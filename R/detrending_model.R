# Detrending the model:

all_melt$week <- round(all_melt$date %% 1 * 52, 0) + 1
all_melt$year <- floor(all_melt$date)
all_melt$intweek <- all_melt$week + (all_melt$year - 2000) * 52

colnames(all_melt)[2] <- "class"

all_melt$full_class <- factor(paste0(all_melt$class, "_", all_melt$int))

all_melt$class <- factor(all_melt$class) # This needs to be a class to get the "by" to work:

# Modelling with a 52 week cycle model.
# The Model is binomial to account for the difference in total postings per year.
# These models are big & take a long time to run, but (I think) we're running these 
# accurately.  
#  The total number of graduate postings as a proportion of messages is increasing strongly in
#  Ecolog.  The number of postdoc positions is increasing, but not nearly by as much.

if('modeled.RDS' %in% list.files('data')){
  modeled <- readRDS('data/modeled.RDS')  
} else {
  modeled <- gamm(value ~ s(week, by = full_class, bs= 'cc', k = 12) +
                   s(date, by = full_class, k = 15),
                 random = list(class=~1), 
                 method = 'REML',
                 data = all_melt,
                 family = poisson,
                 niterPQL=200, sing.tol=1e-20)
  
  saveRDS(object = modeled, file = 'data/modeled.RDS')
}
  
if('modeled1.RDS' %in% list.files('data')){
  modeled1 <- readRDS('data/modeled1.RDS')  
} else {
  modeled1 <- gamm(value ~ s(week, by = full_class, bs= 'cc', k = 25) +
                 s(date, by = full_class, k = 7),
                 random = list(class=~1),
                 method = 'REML',
                 correlation = corARMA(form = ~1|date, p = 1),
               data = all_melt,
               family = poisson,
               niterPQL=200)
  saveRDS(object = modeled1, file = 'data/modeled1.RDS')
}

if('modeled2.RDS' %in% list.files('data')){
  modeled2 <- readRDS('data/modeled2.RDS')  
} else {
  modeled2 <- gamm(value ~ s(week, by = full_class, bs= 'cc', k = 52) +
                     s(date, by = full_class),
                   random = list(class=~1),
                   method = 'REML',
                   correlation = corARMA(form = ~1|date, p = 2),
                   data = all_melt,
                   family = poisson,
                   niterPQL=200)
  saveRDS(object = modeled2, file = 'data/modeled2.RDS')
}

#layout(matrix(1:2, ncol = 2))
#res <- resid(modeled1$lme, type = "normalized")
#acf(res, lag.max = 1000, main = "ACF - AR(2) errors")
#pacf(res, lag.max = 1000, main = "pACF- AR(2) errors")

# Shows model 1 is best.
BIC(modeled$lme, modeled1$lme)

pred.output <- expand.grid(year = 2000:2015, 
                           week = 1:52,
                           full_class = unique(all_melt$full_class))

pred.output$class <- substr(pred.output$full_class, 1, 2)
pred.output$inter <- substr(pred.output$full_class, 4, 1000)
pred.output$date  <- pred.output$year + (pred.output$week/52 - 1)

p1 <- predict(modeled1$gam, newdata = pred.output, type = 'terms', se.fit = TRUE)

pred.output.p <- data.frame(pred.output, p1)
pred.output.p[pred.output.p == 0] <- NA

pred_melt <- melt(pred.output.p, id.vars = c('year', 'week', 'date', 'full_class', 'class', 'inter'), na.rm = TRUE)

pred_melt$class <- factor(pred_melt$class, labels = c("Graduate", 'Postdoctoral', 'Tenure Track'))

# Weekly plot:
week_plot <- ggplot(subset(pred_melt, regexpr('^fit.s.week', variable)>0 & year == 2000),
       aes(x = week, y = value, group = full_class, color = inter)) + 
  geom_path(size = 2) +
  scale_color_brewer(type = 'qual', 
                     guide  = guide_legend(title = "Interdisciplinary")) +
  scale_x_continuous(breaks = c(8,20,32,44)) +
  facet_wrap(~class) + 
  theme_bw() +
  xlab('Week of the Year') +
  ylab('Contribution') +
  coord_cartesian(xlim=c(1, 52), ylim=c(-1.15, 1.25), expand = FALSE) +
  theme(axis.text = element_text(family='serif', size = 16),
        axis.title = element_text(family='serif', size = 18, face = 'bold'),
        #legend.text = element_text(family='serif', size = 14),
        strip.text =  element_text(family='serif', size = 16, face = 'bold'))

ggsave(plot = week_plot, filename = 'figures/week_plot.tiff', width = 6, height = 4, dpi = 150)

ann_plot <- ggplot(subset(pred_melt, regexpr('^fit.s.date', variable)>0 & week == 1),
       aes(x = date, y = value, group = full_class, color = inter)) + 
  geom_path(size = 2) +
  scale_color_brewer(type = 'qual', 
                     guide  = guide_legend(title = "Interdisciplinary")) +
  facet_wrap(~class) + theme_bw() +
  xlab('Year') +
  ylab('Contribution') +
  coord_cartesian(xlim=c(2000, 2014), ylim=c(-1.15, 1.25), expand = FALSE) +
  theme(axis.text = element_text(family='serif', size = 16),
        axis.title = element_text(family='serif', size = 18, face = 'bold'),
        #legend.text = element_text(family='serif', size = 14),
        strip.text =  element_text(family='serif', size = 16, face = 'bold'))

ggsave(plot = ann_plot, filename = 'figures/ann_plot.tiff', width = 6, height = 4, dpi = 150)