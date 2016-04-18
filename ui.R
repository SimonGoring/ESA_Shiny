shinyUI(pageWithSidebar(
  headerPanel('Classifying ECOLOG'),
  sidebarPanel(
    fixedRow(
      column(4, checkboxGroupInput("jobType", 
                         label = h3("Job Type"), 
                         choices = list("Job Ad" = 1, "Grad Position" = 2,
                                        "Internship" = 3, "Seasonal" = 4,
                                        "Tenure Track" = 5, "Postdoc" = 6,
                                        "Not an Ad" = 7)),
             checkboxGroupInput("jobPay", 
                                label = h3("Job Pay"), 
                                choices = list("Salary/Hourly" = 1, "Scholarship" = 2,
                                               "Stipend" = 3, "Unpaid" = 4))),
      
      column(4, checkboxGroupInput("jobQuals", 
                                   label = h3("Job Qualifications"), 
                                   choices = list("Faculty" = 1, "Postdoc+" = 2,
                                                  "Grad Student" = 3, "Undergrad" = 4)),
             checkboxGroupInput("interDiscip", 
                                label = h3("Other"), 
                                choices = list("Interdisciplinary" = 1)),
             checkboxGroupInput("certainty", 
                                label = h3("Are You Sure?"), 
                                choices = list("Something I'm not sure of" = 1))
             )),
              
    selectInput('selectOrder', label = "Priority",
                choices = c('default', 'postdoc', 
                            'interdisciplinary','tenure track',
                            'grad position', 'unpaid'),
                selected = 'default',
                width = '50%'),
  actionButton(inputId = 'click', label = 'Confirm')),
    
  mainPanel(
    htmlOutput('table')
  )
))