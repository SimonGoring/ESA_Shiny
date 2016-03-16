#  Testing the effect of hand classification:
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

source('R/merge_RDS.R')
source('R/load_term.R')

dtm.s99 <- as.data.frame(as.matrix(dtm.s99))

# To build the model:
library(randomForest)

all_runs <- readRDS(file = 'data/responses.RDS')

#  Clean out messages that were skipped (had no classification effort)
all_runs <- all_runs[rowSums(all_runs[,-1]) > 0 & !(rowSums(all_runs[,-1]) > 0 & all_runs$uncer),]

#  In these groups there is potential for overlap, but we assign a class:
tenure_track <- na.omit(data.frame(tt.job = factor(all_runs$jt_tt==1),
                                   dtm.s99[all_runs$msg,]>0))

postdoc      <- na.omit(data.frame(postdo = factor(all_runs$jt_pd==1),
                                   dtm.s99[all_runs$msg,]>0))

grad_student <- na.omit(data.frame(gradst = factor(all_runs$jt_gp==1),
                                   dtm.s99[all_runs$msg,]>0))

interdiscip  <- na.omit(data.frame(interd = factor(all_runs$inter==1),
                                   dtm.s99[all_runs$msg,]>0))

full_class <- rep(NA, nrow(all_runs))

## Classifying the messages into a single factor level based on some boolean choices.
full_class[all_runs$jt_tt & !(all_runs$jt_pd & all_runs$jt_gp & all_runs$inter)] <- "TT"
full_class[all_runs$jt_tt & all_runs$inter & !(all_runs$jt_pd & all_runs$jt_gp)] <- "TT_Int"

full_class[all_runs$jt_pd & !(all_runs$jt_tt & all_runs$jt_gp & all_runs$inter)] <- "PD"
full_class[all_runs$jt_pd & all_runs$inter & !(all_runs$jt_tt & all_runs$jt_gp)] <- "PD_Int"

full_class[all_runs$jt_gp & !(all_runs$jt_tt & all_runs$jt_pd & all_runs$inter)] <- "GR"
full_class[all_runs$jt_gp & all_runs$inter & !(all_runs$jt_tt & all_runs$jt_pd)] <- "GR_Int"

full_class[!(all_runs$jt_tt | all_runs$jt_pd | all_runs$jt_gp) & 
             (all_runs$jt_ad | all_runs$jt_se | all_runs$jt_in)] <- "Other job"

full_class[all_runs$jt_na | is.na(full_class)] <- "Non-job"

full_df      <- na.omit(data.frame(fm = full_class,
                                   dtm.s99[all_runs$msg,]>0))

dtm.pred <- data.frame(dtm.s99>0)

conf_drop <- rep(NA, 10)
  
for(i in 1:10){
  # The messages are in classified order.  We have the actual message dates, but I'm more interested
  #  in how well the messages classify as a function of training set size:
  
  test_df <- full_df[1:(nrow(full_df)/10 * i),]
  
  freq <- min(table(test_df$fm))
  
  test.rf   <- randomForest(fm     ~ ., data = test_df, 
                            sampsize = rep(freq,length(unique(test_df[,1]))), # balance the classes.
                            mtry = 127) # mtry comes from tuneRF.
  
  conf_drop[i] <- sum((test.rf$confusion[,9] * rowSums(test.rf$confusion[,-9])))/sum(test.rf$confusion[,-9])
}

conf_drop <- data.frame(samples = (nrow(full_df)/10 * i),
                        confusion = conf_drop)

saveRDS(conf_drop, 'data/conf_drop.RDS')
