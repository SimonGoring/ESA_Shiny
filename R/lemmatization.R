#  I thought this might work better, but it seems like it's going to
#  be very difficult to work through.

library(tm)
library(SnowballC)

all.messages <- readRDS(file = 'data/all.messages.RDS') 

all_msg_unlist <- unlist(lapply(all.messages, function(x)lapply(x, function(x)ifelse(length(x)>1,x[[2]],NA))))
subjects <- unlist(lapply(all.messages, function(x)lapply(x, function(x)ifelse(length(x)>1,x[[1]],NA))))

tm.corp <- Corpus(VectorSource(all_msg_unlist))

# There is a pattern used to mask identifying information.  It only works after the
#  numbers are removed because the numbers specifically point to a date (or something)
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = FALSE))

#  Remove full URLs:
tm.corp <- tm_map(tm.corp, toSpace, "(ht|f)tp(s?)\\:\\/\\/[0-9a-zA-Z]([-.\\w]*[0-9a-zA-Z])*(:(0-9)*)*(\\/?)([a-zA-Z0-9\\-\\.\\?\\,\\'\\/\\\\+&amp;%\\$#_]*)?")
#  Remove partial URLs:
tm.corp <- tm_map(tm.corp, toSpace, "[0-9a-zA-Z]*\\.*\\.[0-9a-zA-Z]*")
tm.corp <- tm_map(x=tm.corp, FUN='removeNumbers')

#  This gets rid of all href tags, and any hyperlinked text.
#  This should help reduce the memory overhead by about 58mb.
tm.corp <- tm_map(tm.corp, toSpace, "<a.+/a>")

# before we remove punctuation we have to make sure that we replace some objects 
# with spaces, this includes HTML entities like &lt;:
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
tm.corp <- tm_map(tm.corp, toSpace, "\\&[a-z0-9A-Z]+\\;")
tm.corp <- tm_map(tm.corp, 'stripWhitespace')

lemmized <- rep(NA, length(all_msg_unlist))
changes <- list()
fixed <- list()

for(i in 1:length(tm.corp)){
  
  path <- ('C:\\Users\\Simon Goring\\morphadorner')
  
  oldwd <- getwd()
  
  setwd(path)
  
  fileCon <- file(paste0('out_dist/',i,'_textfile.txt'))
  newfile <- writeChar(iconv(as.character(tm.corp[[i]]), to = 'latin1'), fileCon)
  close(fileCon)
}
  
#  This takes about two hours right now.
system('adornplaintext out_run out_dist/*.*')

all_output <- list.files('out_run/', full.names = TRUE)

all_sort <- sapply(all_output, function(x) as.numeric(substr(x, 9, regexpr('_text', x)-1)))

for(i in 1:length(all_output)){
  
  ## Right now this takes a long time, but it's something that is 
  ## trivial to parallelize.
  # There is some bizzare encoding that this solves:
  con <- textConnection(system(paste0('cat -e ',all_output[i]), intern=TRUE))
  output <- read.table(con, quote = "", fill=TRUE, row.names=NULL)[,-c(1,6)]
  colnames(output) <- c('token', 'POS', 'lemma', 'spelling')
  
  close(con)
  
  # Text symbols from NUPOS are expained here:
  # http://morphadorner.northwestern.edu/documentation/nupos/
  # This line just gets rid of weird symbols.
  output <- subset(output, !POS %in% 'sy')
  
  if(nrow(output)>0){
    output$count <- 1
    
    lemmized[all_sort[i]] <- paste(output$spelling, collapse = ' ')
#    changes[[all_sort[i]]]  <- subset(output, !token %in% spelling)[,c(1,4)]
#    fixed[[all_sort[i]]]    <- subset(output,  token %in% spelling)[,c(1,4)]
  }
#  cat(i, 'and ', all_sort[i],'\n')
}
