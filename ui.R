library(shiny)
library(markdown)

navbarPage(theme = shinythemes::shinytheme("cerulean"),"Applied Statistics & Machine Learning",
           
           
           tabPanel("Descriptive Stats",
                    
                    #sidebar
                    
                    mainPanel(
                      
                      # Output: Data file ----
                      verbatimTextOutput("summary")
                      
                      
                    )
                    
           ),
           tabPanel("Ds stats with fileupload",
                    verbatimTextOutput("Machine"),
                    sidebarLayout(
                      sidebarPanel(
                        # radioButtons("plotType", "Plot type",
                        #              c("Scatter"="p", "Line"="l"),
                        # ),
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
                        tableOutput("c2"),
                        verbatimTextOutput("c2summary")
                        
                      )
                    )
           ),
           tabPanel("Discrete",
                    
                    sidebarPanel(
                      selectInput("dismodel", "Select Model", 
                                  
                                  choices = c("Binomial" = "binomial", 
                                              "Poisson" = "poisson", 
                                              "Geometric" = "geometric"), 
                                  selected = "binomial" 
                                  
                      ), 
                      conditionalPanel( 
                        condition = "input.dismodel == 'binomial'", 
                        numericInput("n", "parameter n in Binomial" , value = 10), 
                        numericInput("p", "parameter p in Binomial" , value = 0.5) 
                      ), 
                      
                      conditionalPanel(     
                        condition = "input.dismodel == 'poisson'", 
                        numericInput("lam", "parameter lambda in Poisson" , value = 1) 
                      ), 
                      
                      conditionalPanel(     
                        condition = "input.dismodel == 'geometric'", 
                        numericInput("p", "parameter p in Geometric" , value = 0.5) 
                      ), 
                      numericInput("max", "upper limit for x" , value = 5),  
                      sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10),
                      conditionalPanel( 
                        condition = "input.dismodel == 'binomial'", 
                        numericInput("j1", "j for Bin" , value = 1) 
                      ), 
                      conditionalPanel( 
                        condition = "input.dismodel == 'poisson'", 
                        numericInput("j2", "j for Poisson" , value = 1) 
                      ), 
                      conditionalPanel( 
                        condition = "input.dismodel == 'geometric'", 
                        numericInput("j3", "j for geometric" , value = 1) 
                      ) 
                      
                      
                      
                      
                      
                    ),  
                    mainPanel(  
                      plotOutput("histogram"),  
                      tableOutput('tab')  
                    )                  
           )
)