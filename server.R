#  Load the message data and evaluation data to date:

all.messages <- readRDS('data/all.messages.RDS')
evals        <- list(readRDS('data/evals.RDS'))
things       <- readRDS('data/secret.RDS')
not_jobs     <- readRDS('data/not_jobs.RDS')
pred_good    <- readRDS('data/pred_good.RDS')

#  Extract the messages and subject headings:
messages <- unlist(lapply(all.messages, function(x)lapply(x, function(x)ifelse(length(x)>1,x[[2]],NA))))
subjects <- unlist(lapply(all.messages, function(x)lapply(x, function(x)ifelse(length(x)>1,x[[1]],NA))))

messages <- c(messages, readChar('data/initialmessage.txt', file.info('data/initialmessage.txt')$size))
subjects <- c(subjects, '')

change.words <- function(x){
  x <- gsub('interdisciplinary', '<b><u>interdisciplinary</u></b>', x)
  x <- gsub('multidisciplinary', '<b><u>multidisciplinary</u></b>', x)
  x <- gsub('transdisciplinary', '<b><u>transdisciplinary</u></b>', x)

  x <- gsub('Interdisciplinary', '<b><u>Interdisciplinary</u></b>', x)
  x <- gsub('Multidisciplinary', '<b><u>Multidisciplinary</u></b>', x)
  x <- gsub('Transdisciplinary', '<b><u>Transdisciplinary</u></b>', x)
  
  x <- gsub('\\n', '<br/>', x)
  
  x <- paste0(x, '<br/>', readChar('data/footer.txt', file.info('data/footer.txt')$size))
  
  return(iconv(enc2utf8(x), sub = 'byte'))
}


shinyServer(function(input, output, session) {

  #  The 'flag' starts out as 'x'
  counter <<- 0
  flag <<- 'x'
  randVal <- formatC(round(runif(1,0,1000000),0), width = 6, format = "d", flag = "0")
  dateTime <- gsub('[ |:]', '_', as.character(Sys.time()))
  #initialState <- reactiveValues(A = length(messages))
  
  values <- reactive({
    input$click
    isolate({if(flag == 'x'){
        return(length(messages))
      }
      
      samples <- (1:length(subjects))[!(1:length(subjects))%in%(not_jobs+1)]
      tester <- sample(samples, 1, 
                       prob = c(1,pred_good)[!(1:length(subjects))%in%(not_jobs+1)]^2)
      
      while(is.na(messages[tester])){
        tester <- sample(samples, 1)  
      }
      
      return(tester)})
    
    })
  
  output$table <- renderUI({
    if (input$click == 0){
      tail(evals)
    }
    isolate({  
      counter <- counter + 1
      new <- isolate(list(values(), input$jobType, input$jobPay, 
                                     input$jobQuals, input$interDiscip,
                                     input$certainty))
      new$flag <- flag

      flag <<- 'a'
      evals[[length(evals)+1]] <<- new
      saveRDS(evals, file=paste0('data/evals',randVal, dateTime,'.RDS'))

      updateCheckboxGroupInput(session, "jobType",
                         choices = list("Job Ad" = 1, "Grad Position" = 2,
                                        "Internship" = 3, "Seasonal" = 4,
                                        "Tenure Track" = 5, "Postdoc" = 6,
                                        "Not an Ad" = 7),
                         selected = NULL)
      updateCheckboxGroupInput(session, "jobPay",
                         choices = list("Salary/Hourly" = 1, "Scholarship" = 2,
                                        "Stipend" = 3, "Unpaid" = 4),
                         selected = NULL)    
      updateCheckboxGroupInput(session, "jobQuals", 
                         choices = list("Faculty" = 1, "Postdoc+" = 2,
                                        "Grad Student" = 3, "Undergrad" = 4),
                         selected = NULL)
      updateCheckboxGroupInput(session, "interDiscip", 
                         choices = list("Interdisciplinary" = 1),
                         selected = NULL)
      updateCheckboxGroupInput(session, "certainty", 
                        choices = list("Something I'm not sure of" = 1),
                        selected = NULL)
      
      HTML(paste0('Msg:', values(), ' - ', subjects[[values()]],'<br/>',
                          change.words(messages[[values()]])))
    })
  })

})