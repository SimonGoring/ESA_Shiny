#  Load the message data and evaluation data to date:
all.messages <- readRDS('data/all.messages.RDS')
evals        <- readRDS('data/evals.RDS')

#  Extract the messages and subject headings:
messages <- unlist(lapply(all.messages, function(x)lapply(x, function(x)ifelse(length(x)>1,x[[2]],NA))))
subjects <- unlist(lapply(all.messages, function(x)lapply(x, function(x)ifelse(length(x)>1,x[[1]],NA))))

messages <- c(messages, 'Click confirm to start')
subjects <- c(subjects, '')

shinyServer(function(input, output, session) {

  #  The 'flag' starts out as 'x'
  flag <<- 'x'
  randVal <- formatC(round(runif(1,0,1000000),0), width = 6, format = "d", flag = "0")
  dateTime <- gsub(' ', '_', as.character(Sys.time()))
  #initialState <- reactiveValues(A = length(messages))
  
  values <- reactive({
    input$click
    isolate({if(flag == 'x'){
      return(length(messages))
      }
      return(sample(1:length(subjects), 1))})
    })
  
  output$table <- renderUI({
    if (input$click == 0){
      tail(evals)
    }
    isolate({  
      
      new <- evals[1,]
      new[1,1] <- isolate(values())
      new[1,1 + as.numeric(isolate(input$checkGroup))] <- 1
      new$flag <- flag

      flag <<- 'a'
      evals <<- rbind(evals, new)
      saveRDS(evals, file=paste0('data/evals',randVal, dateTime,'.RDS'))
      
      updateCheckboxGroupInput(session, 'checkGroup', 
                               choices=list("Interdisciplinary" = 1, 
                                            "Job Ad" = 2,
                                            "Seasonal" = 3,
                                            "Internship" = 4,
                                            "Tenure Track" = 5,
                                            "Postdoc" = 6,
                                            "Grad student" = 7,
                                            "Undergraduate" = 8,
                                            "Scholarship" = 9,
                                            "Unpaid" = 10,
                                            "Small Stipend" = 11,
                                            "Some Uncertainty (Reviewer)" = 12), 
                               selected=NULL)
      
      HTML(paste0('Msg:', values(), ' - ', subjects[[values()]],'<br/>',
                          messages[[values()]]))
    })
  })

})