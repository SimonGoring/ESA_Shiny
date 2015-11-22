#  A script to merge all the RDS files run from the eval operation:

#  Each of the RDS files is structured as a list, the list includes:
#  1.  A two row head.
#  2.  A list of all the samples that the user has checked for each element.
#    1. The message ID
#    2. Job type: Any of "Job Ad" = 1, 
#                        "Grad Position" = 2, 
#                        "Internship" = 3, 
#                        "Seasonal" = 4, 
#                        "Tenure Track" = 5, 
#                        "Postdoc" = 6, 
#                        "Not an Ad" = 7
#    3. Job pay: Any of  "Salary/Hourly" = 1, 
#                        "Scholarship" = 2,
#                        "Stipend" = 3, 
#                        "Unpaid" = 4
#    4. Job qual: Any of "Faculty" = 1, 
#                        "Postdoc+" = 2,
#                        "Grad Student" = 3, 
#                        "Undergrad" = 4
#    5. Interdis:  If yes then assign a 1 (else NA)
#    6. Uncertain: If yes then assign a 1 (else NA)
#    7. "Flag"

all_RDS <- list.files('../data', pattern = '*evals[0-9][0-9]*', full.names = TRUE)

parse_lists <- function(x){
  
  #  There are 18 elements in the table, the message number, and then each of the 
  #  values, stored as logicals:
  
  job_type <- 1:7 %in% x[[2]]
  job_pay  <- 1:4 %in% x[[3]]
  job_qual <- 1:4 %in% x[[4]]
  interdis <- 1   %in% x[[5]]
  uncert   <- 1   %in% x[[6]]
  
  output <- data.frame(msg = x[[1]],
                       jt_ad = job_type[1], 
                       jt_gp = job_type[2], 
                       jt_in = job_type[3], 
                       jt_se = job_type[4], 
                       jt_tt = job_type[5], 
                       jt_pd = job_type[6], 
                       jt_na = job_type[7], 
                       jp_sa = job_pay[1],
                       jp_sc = job_pay[2],
                       jp_st = job_pay[3],
                       jp_na = job_pay[4],
                       jq_fa = job_qual[1],
                       jq_pd = job_qual[2],
                       jq_gr = job_qual[3],
                       jq_ug = job_qual[4],
                       inter = interdis, 
                       uncer = uncert)
  output  

}

parse_files <- function(x){
  input <- readRDS(x)
  if(class(input) == 'data.frame'){
    return(data.frame(msg = NA,
                      jt_ad = NA, 
                      jt_gp = NA, 
                      jt_in = NA, 
                      jt_se = NA, 
                      jt_tt = NA, 
                      jt_pd = NA, 
                      jt_na = NA, 
                      jp_sa = NA,
                      jp_sc = NA,
                      jp_st = NA,
                      jp_na = NA,
                      jq_fa = NA,
                      jq_pd = NA,
                      jq_gr = NA,
                      jq_ug = NA,
                      inter = NA, 
                      uncer = NA))
  }
  input[[1]] <- NULL
  
  output_frame <- do.call(rbind.data.frame, lapply(input, parse_lists))
  output_frame[,1] <- c(NA, output_frame[-nrow(output_frame),1])
  
  output_frame
}


all_runs <- na.omit(do.call(rbind.data.frame, lapply(all_RDS, parse_files)))

not_job <- all_runs$msg[which(all_runs$jt_na == TRUE)]

saveRDS(object = all_RDS, file = '../data/RDS_list.RDS')
saveRDS(object = not_job, file = '../data/not_jobs.RDS')
saveRDS(object = all_runs, file = '../data/responses.RDS')
