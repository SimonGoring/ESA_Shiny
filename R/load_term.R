#  This file is to load and parse all the ecolog messages:

library(tm)
library(SnowballC)

if('all.messages.RDS' %in% list.files('data')){
  all.messages <- readRDS(file = 'data/all.messages.RDS') 
} else {
  source('R/Scrape_ECOLOG.R')
}

if('term_frame.RDS' %in% list.files('data')){
  dtm_both <- readRDS('data/term_frame.RDS')
  dtm     <- dtm_both[[1]]
  dtm.s99 <- dtm_both[[2]]
} else {
  #  The purpose of the following commands is to use the functionality of the `tm` 
  #  to parse all the messages.  The list contains two items, a subject line and
  #  the message text proper, so we need to select every other object:
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
  tm.corp <- tm_map(tm.corp, 'removePunctuation')
  tm.corp <- tm_map(tm.corp, 'stripWhitespace')
  tm.corp <- tm_map(tm.corp, content_transformer(tolower))
  tm.corp <- tm_map(tm.corp, removeWords, stopwords("english"))
  tm.corp <- tm_map(tm.corp, stemDocument)
  
  #  Note, I tried stemming, but it does a pretty rotten job on the data.
  
  dtm   <- DocumentTermMatrix(tm.corp)
  dtm.s99 <- removeSparseTerms(dtm, 0.99)
  saveRDS(list(dtm, dtm.s99), file = 'data/term_frame.RDS')
}
