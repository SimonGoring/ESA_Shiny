#  Building the random forest models:

curr_RDS <- list.files('data', pattern = '*evals[0-9][0-9]*', full.names = TRUE)
old_RDS  <- readRDS('data/RDS_list.RDS')

if(any(!curr_RDS %in% old_RDS)){
  source('R/merge_RDS.R')
  source('R/load_term.R')
  
  dtm.s99 <- as.data.frame(as.matrix(dtm.s99))
  
  # To build the model:
  library(randomForest)
  
  all_runs <- readRDS(file = 'data/responses.RDS')
  
  tenure_track <- na.omit(data.frame(tt.job = factor(all_runs$jt_tt==1),
                                     dtm.s99[all_runs$msg,]>0))
  
  postdoc      <- na.omit(data.frame(postdo = factor(all_runs$jt_pd==1),
                                         dtm.s99[all_runs$msg,]>0))
  
  grad_student <- na.omit(data.frame(gradst = factor(all_runs$jt_gp==1),
                                     dtm.s99[all_runs$msg,]>0))
  
  interdiscip  <- na.omit(data.frame(interd = factor(all_runs$inter==1),
                                     dtm.s99[all_runs$msg,]>0))
  
  dtm.pred <- data.frame(dtm.s99>0)
  
  colnames(dtm.s99)[432] <- "driver.s"
  colnames(dtm.s99)[472] <- "else."
  colnames(dtm.s99)[627] <- "function."
  colnames(dtm.s99)[1017] <- "next."
  
  #  Models are very slow!
  is.job.rf <- randomForest(tt.job ~ ., data = tenure_track, sampsize = c(30, 30))
  is.pdc.rf <- randomForest(postdo ~ ., data = postdoc, sampsize = c(30, 30))
  is.gra.rf <- randomForest(gradst ~ ., data = grad_student, sampsize = c(30, 30))
  is.int.rf <- randomForest(interd ~ ., data = interdiscip, sampsize = c(30, 30))

  
  predict_tt <- predict(is.job.rf, dtm.pred)
  predict_pd <- predict(is.pdc.rf, dtm.pred)
  predict_gr <- predict(is.gra.rf, dtm.pred)
  predict_in <- predict(is.int.rf, dtm.pred)
  
  rf_models <- list(predict_tt, predict_pd, predict_gr, predict_in)
  r_preds   <- data.frame(msg = 1,
                          predict_tt,
                          predict_pd,
                          predict_gr,
                          predict_in)
  
  confusion <- apply(r_preds[,2:4], 1, function(x)sum(as.logical(x)))

  pred_good <- (confusion * as.logical(r_preds$predict_in) * 2)^2 + 1
  
  saveRDS(pred_good, file = 'data/pred_good.RDS')
  saveRDS(r_preds,   file = 'data/predictions.RDS')
  saveRDS(rf_models, file = 'data/models.RDS')
} else{
  r_preds   <- readRDS('data/predictions.RDS')
  rf_models <- readRDS('data/models.RDS')
}
