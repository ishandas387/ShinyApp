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
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = ","),
                        
                        # Input: Select quotes ----
                        radioButtons("quote", "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = '"'),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Select number of rows to display ----
                        radioButtons("disp", "Display",
                                     choices = c(Head = "head",
                                                 All = "all"),
                                     selected = "head")
                      ),
                      
                      mainPanel(
                        
                        # Output: Data file ----
                        # tableOutput("c2"),
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
                                  
                                  selected = "exponential" 
                                  
                      ), 
                      
                      
                      
                      selectInput("dataset", "Select Data", 
                                  
                                  choices = c("IRIS" = "iris", 
                                              
                                              "USArrests" = "USArrests",
                                              
                                              "Other Data" = "OtherData"
                                              
                                              
                                  ), 
                                  
                                  selected = "IRIS" 
                                  
                      ), 
                      
                      
                      
                      
                      
                      conditionalPanel( 
                        
                        condition = "input.dataset == 'iris'", 
                        
                        selectInput("column1", "Select Column:",  
                                    
                                    choices=colnames(iris)) 
                        
                        
                        
                      ), 
                      
                      
                      
                      conditionalPanel( 
                        
                        condition = "input.dataset == 'USArrests'", 
                        
                        selectInput("column2", "Select Column:",  
                                    
                                    choices=colnames(USArrests)) 
                        
                        
                        
                      ), 
                      
                      
                      
                      
                      
                      sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10), 
                      
                      
                      
                      conditionalPanel( 
                        
                        condition = "input.conmodel == 'uniform'" 
                        
                        ### Intentionally left as empty for student exercise  
                        
                      ) 
                      
                      
                    ), 
                    
                    
                    mainPanel(  
                      
                      #tableOutput('prob'),
                      
                      DT::dataTableOutput('summary'), 
                      verbatimTextOutput("prob"),
                      verbatimTextOutput("ErrorMessage")
                      
                      #verbatimTextOutput("prt") 
                      
                    )          
           )


##############CONTINUOUS PROBABILITY########################################################
)