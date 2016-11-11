#  Building the random forest models:
#  This file loads all RDS files following the system file pattern as defined
#  in the shiny scripts (server.R and ui.R).

#  During this process we build the term matrix for the top 99% of terms
#  all run in the `load_term.R` file.

#  Using the hand coded Ecolog posts we then build RF models for the individual 
#  messages.  This is partly to facilitate hand coding, since mesages with a 
#  high confusion rate are given a high likelihood of being selected for hand
#  coding in the Shiny app.

curr_RDS <- list.files('data', pattern = '*evals[0-9][0-9]*', full.names = TRUE)
old_RDS  <- readRDS('data/RDS_list.RDS')

if(any(!curr_RDS %in% old_RDS)){
  source('R/merge_RDS.R')
  source('R/load_term.R')
  
  dtm.s99 <- as.data.frame(as.matrix(dtm.s99))
  
  # To build the model:
  library(randomForest)
  library(glmnet)
  
  all_runs <- readRDS(file = 'data/responses.RDS')
  
  #  Clean out messages that were skipped (had no classification effort)
  all_runs <- all_runs[rowSums(all_runs[,-1]) > 0 & !(rowSums(all_runs[,-1]) > 0 & all_runs$uncer),]
  
  #  In these groups there is potential for overlap, but we assign a class:
  tenure_track <- na.omit(data.frame(tt.job = factor(all_runs$jt_tt == 1),
                                     dtm.s99[all_runs$msg,] > 0))
  
  postdoc      <- na.omit(data.frame(postdo = factor(all_runs$jt_pd == 1),
                                         dtm.s99[all_runs$msg,] > 0))
  
  grad_student <- na.omit(data.frame(gradst = factor(all_runs$jt_gp == 1),
                                     dtm.s99[all_runs$msg,] > 0))
  
  interdiscip  <- na.omit(data.frame(interd = factor(all_runs$inter == 1),
                                     dtm.s99[all_runs$msg,] > 0))
  
  unpaid       <- na.omit(data.frame(unpaid = factor(all_runs$jt_se == 1 & all_runs$jp_na == 1),
                                     dtm.s99[all_runs$msg,] > 0))
  
  full_class <- rep(NA, nrow(all_runs))
  
  ## Classifying the messages into text strings.
  full_class[all_runs$jt_tt & !(all_runs$jt_pd & all_runs$jt_gp & all_runs$inter)] <- "TT"
  full_class[all_runs$jt_tt & all_runs$inter & !(all_runs$jt_pd & all_runs$jt_gp)] <- "TT_Int"
  
  full_class[all_runs$jt_pd & !(all_runs$jt_tt & all_runs$jt_gp & all_runs$inter)] <- "PD"
  full_class[all_runs$jt_pd & all_runs$inter & !(all_runs$jt_tt & all_runs$jt_gp)] <- "PD_Int"
  
  full_class[all_runs$jt_gp & !(all_runs$jt_tt & all_runs$jt_pd & all_runs$inter)] <- "GR"
  full_class[all_runs$jt_gp & all_runs$inter & !(all_runs$jt_tt & all_runs$jt_pd)] <- "GR_Int"
  
  full_class[all_runs$jt_se == 1 & all_runs$jp_na == 1] <- "SE_unp"
  
  full_class[!(all_runs$jt_tt | all_runs$jt_pd | all_runs$jt_gp) & 
               (all_runs$jt_ad | all_runs$jt_se | all_runs$jt_in) & is.na(full_class)] <- "Other job"
  
  full_class[all_runs$jt_na | is.na(full_class)] <- "Non-job"
  
  full_df      <- na.omit(data.frame(fm = full_class,
                                     dtm.s99[all_runs$msg,] > 0))
  
  dtm.pred <- data.frame(dtm.s99 > 0)
  
  # randomForest
  #  Models are very slow!
  is.job.rf <- randomForest(tt.job ~ ., data = tenure_track, sampsize = c(30, 30))
  is.pdc.rf <- randomForest(postdo ~ ., data = postdoc, sampsize = c(30, 30))
  is.gra.rf <- randomForest(gradst ~ ., data = grad_student, sampsize = c(30, 30))
  is.int.rf <- randomForest(interd ~ ., data = interdiscip, sampsize = c(30, 30))
  is.unp.rf <- randomForest(unpaid ~ ., data = unpaid, sampsize = c(30, 30))
  
  # testing_model <- tuneRF(full_df[,-1], full_df[,1], stepFactor = 1.5)
  full.rf   <- randomForest(fm     ~ ., data = full_df, 
                            sampsize = rep(20,length(unique(full_class))), # balance the classes.
                            mtry = 127) # mtry comes from tuneRF.
  
  predict_tt <- predict(is.job.rf, dtm.pred)
  predict_pd <- predict(is.pdc.rf, dtm.pred)
  predict_gr <- predict(is.gra.rf, dtm.pred)
  predict_in <- predict(is.int.rf, dtm.pred)
  predict_up <- predict(is.unp.rf, dtm.pred)
  predict_al <- predict(full.rf, dtm.pred)
  
  rf_models <- list(predict_tt, predict_pd, predict_gr, predict_in)
  r_preds   <- data.frame(msg = 1,
                          predict_tt,
                          predict_pd,
                          predict_gr,
                          predict_in, 
                          predict_up)
  
  # confusion winds up as a numeric vector indicating how many classes
  # a message is predicted to cover:
  #  * 0 (is not a tt, pd, gr or in job ad) ~39k
  #  * 1 (is only one, and not any others)  ~ 7k
  #  * 2 (is a couple of these)             ~ 6k
  #  * 3                                    ~ 3k
  
  confusion <- apply(r_preds[,2:4], 1, function(x)sum(as.logical(x)))

  # pred_good gets valuation based on whether or not there is confusion,
  # whether a post is interdisciplinary (using the logical +1, so interdisc.
  # posts get values twice as much as others), and then squares this to
  # more highly prioritize these posts, to improve classification.
  
  predict_prob <- predict(full.rf, dtm.pred, type = 'prob')
  
  # Okay, there are classes I want to push upwards:
  pred_good <- rep(0, nrow(predict_prob))
  pred_good[all_runs$msg[!all_runs$msg %in% 55778]] <- pred_good[all_runs$msg[!all_runs$msg %in% 55778]] - 1
  pred_good[predict_prob[,"Non-job"] > 0.6] <- pred_good[predict_prob[,"Non-job"] > 0.6] + .25
  pred_good[predict_prob[,"Other job"] > 0.6] <- pred_good[predict_prob[,"Other job"] > 0.6] + .25
  pred_good[predict_prob[,"PD"] + predict_prob[,"PD_Int"] > 0.6] <- pred_good[predict_prob[,"PD"] + predict_prob[,"PD_Int"] > 0.6] + 3
  pred_good[predict_prob[,"TT"] + predict_prob[,"TT_Int"] > 0.6] <- pred_good[predict_prob[,"TT"] + predict_prob[,"TT_Int"] > 0.6] + 3
  pred_good <- pred_good + abs(min(pred_good)) + 0.25
  
  pred_good <- pred_good / max(pred_good)
  
  pred_good <- data.frame(default = pred_good,
                          postdoc = predict(is.pdc.rf, dtm.pred, type = 'prob')[,2], 
                          interdisciplinary = predict(is.int.rf, dtm.pred, type = 'prob')[,2],
                          tenure = predict(is.job.rf, dtm.pred, type = 'prob')[,2],
                          grad = predict(is.gra.rf, dtm.pred, type = 'prob')[,2],
                          unpaid = predict(is.unp.rf, dtm.pred, type = 'prob')[,2])
  
  saveRDS(pred_good, file = 'data/pred_good.RDS')
  saveRDS(r_preds,   file = 'data/predictions.RDS')
  saveRDS(rf_models, file = 'data/models.RDS')
  saveRDS(predict_al, file = 'data/predict_al.RDS')

  saveRDS(is.job.rf, file = 'data/is_job.RDS')
  saveRDS(is.pdc.rf, file = 'data/is_pdc.RDS')
  saveRDS(is.gra.rf, file = 'data/is_era.RDS')
  saveRDS(is.int.rf, file = 'data/is_int.RDS')
  saveRDS(is.unp.rf, file = 'data/is_unp.RDS')
  saveRDS(full.rf,   file = 'data/full.RDS')
  
    
} else{
  
  
  all_runs <- readRDS(file = 'data/responses.RDS')
  
  is.job.rf <- readRDS(file = 'data/is_job.RDS')
  is.gra.rf <- readRDS(file = 'data/is_era.RDS')
  is.pdc.rf <- readRDS(file = 'data/is_pdc.RDS')
  is.int.rf <- readRDS(file = 'data/is_int.RDS')
  full.rf   <- readRDS(file = 'data/full.RDS')
  
  r_preds   <- readRDS('data/predictions.RDS')
  predict_al   <- readRDS('data/predict_al.RDS')
  rf_models <- readRDS('data/models.RDS')

}
