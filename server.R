library(shiny)
library(pastecs)
library(data.table)
function(input, output, session) {
  
#################Generate a summary of the dataset#############################

  rendingStuff <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file2)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file2$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        print("Error")
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else 
    #   {
    #   return(df)
    #}
    return(df)
  })
  
  #####################Converting into data table########################
  # output$c2 <- renderTable({
  #   rendingStuff()})
  
  #####################Converting into data table########################
  output$c2summary <- renderPrint({
      summary(rendingStuff())
  })
  
  ####################################################
  output$ex1 <- DT::renderDataTable(
    DT::datatable(rendingStuff(), options = list(pageLength = 10))
  )

  #########################CONTINUOUS PROBABILITY###################################
  
  
  output$summary = DT::renderDataTable({ 
    
    if (input$dataset =='iris') { dispdata <- iris } 
    
    else if (input$dataset =='USArrests') { dispdata <- USArrests } 
    
    else if (input$dataset =='OtherData' && is.null(input$file2) ) 
    {
      output$ErrorMessage <- renderPrint("Kindly select the data from the descriptive tab to populate here") } else
      {
        dispdata <- rendingStuff()
      }
    
    
    
    
    # print(dispdata)
    
    DT::datatable(data.frame(dispdata), options = list(lengthChange = TRUE)) 
  }) 
  
  
  # output$prt = renderPrint({ 
  #   
  #   if (input$dataset =='iris') { dispdata <- iris } 
  #   
  #   if (input$dataset =='USArrests') { dispdata <- USArrests } 
  #   
  #   print(dispdata) 
  #   
  # })   
  
  
  output$prob <- renderPrint({ 
    
    if (input$dataset =='iris') { 
      
      print(input$column1)   
      
      if (input$column1 == 'Sepal.Length') { x <- iris$Sepal.Length} 
      
      if (input$column1 == 'Sepal.Width') { x <- iris$Sepal.Width} 
      
      if (input$column1 == 'Petal.Length') { x <- iris$Petal.Length} 
      
      if (input$column1 == 'Petal.Width') { x <- iris$Petal.Width} 
      
      
      
      if (input$column1 == 'Species')  
        
      {  
        
        print('Species is not the right column to predict') 
        
        x <- 0  
        
      }     
      
    } 
    
    if (input$dataset =='USArrests') { 
      
      print(input$column2) 
      
      if (input$column2 == 'Murder') { x <- USArrests$Murder} 
      
      if (input$column2 == 'Assault') { x <- USArrests$Assault} 
      
      if (input$column2 == 'UrbanPop') { x <- USArrests$UrbanPop} 
      
      if (input$column2 == 'Rape') { x <- USArrests$Rape} 
      
    } 
    
    
    
    # normal 
    
    
    
    if (input$conmodel == 'normal')  
      
    {  
      
      print(mean(rnorm(input$s,mean(x), sd(x)))) 
      
    } 
    
    
    
    # exponential 
    
    
    
    if (input$conmodel == 'exponential')  
      
    { 
      
      print(mean(rexp(input$s,1/mean(x)))) 
      
    } 
    
    
    
    
    
    
    
    
    
    if (input$conmodel == 'uniform') { 
      
      print("Under Construction") 
      
      
      
    } 
    
  })    

}
