library(shiny)
library(markdown)

navbarPage(theme = shinythemes::shinytheme("cerulean"),"Applied Statistics & Machine Learning",
           
##############DESCRIPTIVE STATISTICS########################################################

           tabPanel("Descriptive Statistics",
                    sidebarLayout(
                      sidebarPanel(
                        
                        fileInput("file2", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                        
                        # Horizontal line ----
                        tags$hr(),
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),
                        # Input: Select separator ----
                        radioButtons("sep", "Separator",
                                     choices = c(Comma = ",",
                                                 Tab = "\t"),
                                     selected = ","),
                        # Input: Select quotes ----
                        radioButtons("quote", "Quote",
                                     choices = c("Double Quote" = "",
                                                 "None" = '"'),
                                     selected = '"'),
                        # Horizontal line ----
                        tags$hr(),
                        
                      ),
                      
                      mainPanel(
                        
                        DT::dataTableOutput('ex1'),
                        verbatimTextOutput("c2summary")
                      
                      )
                    )
           ),


##############CONTINUOUS PROBABILITY########################################################

           tabPanel("Continuous Probability Model",
                    
                    sidebarPanel( 
                      
                      selectInput("conmodel", "Select Model", 
                                  
                                  choices = c("Normal" = "normal", 
                                              "Exponential" = "exponential", 
                                              "Uniform" = "uniform"), 
                                  selected = "normal" 
                                  
                      ), 
                      selectInput("dataset", "Select Data", 
                                  choices = c("Seat Belts" = "Seatbelts", 
                                              "USArrests" = "USArrests"
                                  ), 
                                  selected = "Seatbelts" 
                                  
                      ), 
                      
                      conditionalPanel( 
                        
                        condition = "input.dataset == 'Seatbelts'", 
                        selectInput("column1", "Select Column:",  
                                    choices=colnames(Seatbelts)) 
                        

                      ), 
                      
                      conditionalPanel( 
                        
                        condition = "input.dataset == 'USArrests'", 
                        selectInput("column2", "Select Column:",  
                                    choices=colnames(USArrests)) 
                        
                      ), 

                      
                      sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10), 
                      conditionalPanel(
                        condition = "input.conmodel == 'uniform'"
                      )

                    ), 
                    mainPanel(  
                      DT::dataTableOutput('summary'), 
                      verbatimTextOutput("prob"),
                      verbatimTextOutput("ErrorMessage"),
                      
                      ####PLOT############################
                      plotOutput("histogram"),  
                      tableOutput("tab")  
                      #verbatimTextOutput("prt") 
                      
                    )          
           ),
 ##############CONTINUOUS PROBABILITY########################################################
  
           tabPanel("Machine Learning",
                    sidebarPanel( 
                          selectInput("mlmodel", "Select Model", 
                                    
                                      choices = c("Naive Bayes" = "NB", 
                                                  "SVM" = "SVM", 
                                                  "MLR" = "MLR",
                                                  "ALL 3 with monte carlo" = "ALL"), 
                                      selected = "NB" 
                                     ),
                             mainPanel(
                               plotOutput("ctx") 
                             )        

                    ),
                            mainPanel(
                              h4("Applying Classification models on PimaIndianDiabetic2 dataset"),
                              verbatimTextOutput('ml'),
                              h4("PimaIndianDiabetic2 dataset"),
                              DT::dataTableOutput('pima')
                            )
           )
)