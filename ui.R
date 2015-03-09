shinyUI(pageWithSidebar(
  headerPanel('Download Example'),
  sidebarPanel(
    
    checkboxGroupInput("checkGroup", 
                       label = h3("Checkbox group"), 
                       choices = list("Interdisciplinary" = 1, 
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
                                      "Some Uncertainty (Reviewer)" = 12)),
    
    actionButton(inputId='click', label='Confirm')
  ),
  mainPanel(
    htmlOutput('table')
  )
))