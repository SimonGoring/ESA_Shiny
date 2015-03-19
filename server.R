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
  dateTime <- gsub('[ |:]', '_', as.character(Sys.time()))
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
      
      new <- isolate(list(values(), input$jobType, input$jobPay, 
                                     input$jobQuals, input$interDiscip,
                                     input$certainty))
      new$flag <- flag

      flag <<- 'a'
      evals <<- list(evals, new)
      saveRDS(evals, file=paste0('data/evals',randVal, dateTime,'.RDS'))

      updateCheckboxGroupInput(session, "jobType",
                         choices = list("Job Ad" = 1, "Grad Position" = 2,
                                        "Internship" = 3, "Seasonal" = 4,
                                        "Tenure Track" = 5, "Postdoc" = 6),
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
                          gsub('[inter|multi|trans]disciplin*', '<b><u>interdisciplinary</u></b>', 
                               messages[[values()]])))
    })
  })

})