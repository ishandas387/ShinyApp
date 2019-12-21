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
    
    if (input$dataset =='Seatbelts') { dispdata <- Seatbelts } 
    
    else if (input$dataset =='USArrests') { dispdata <- USArrests } 
    
    # else if (input$dataset =='OtherData' && is.null(input$file2) ) 
    # {
    #   output$ErrorMessage <- renderPrint("Kindly select the data from the descriptive tab to populate here") } 
    # 
    # else
    # 
    #     {
    #     dispdata <- rendingStuff()
    #   }
    
    
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
    
    if (input$dataset =='Seatbelts') { 
      
      print(input$column1)   
      
      if (input$column1 == 'DriversKilled') { x <- data.frame(Seatbelts)$DriversKilled} 
      
      if (input$column1 == 'drivers') { x <- data.frame(Seatbelts)$drivers} 
      
      if (input$column1 == 'front') { x <- data.frame(Seatbelts)$front} 
      
      if (input$column1 == 'rear') { x <- data.frame(Seatbelts)$rear} 
      
      if (input$column1 == 'kms') { x <- data.frame(Seatbelts)$kms}
      
      if (input$column1 == 'PetrolPrice') { x <- data.frame(Seatbelts)$PetrolPrice}
      
      if (input$column1 == 'VanKilled') { x <- data.frame(Seatbelts)$VanKilled}
      
      
      if (input$column1 == 'law')  
        
      {  
        
        print('law is not the right column to predict') 
        
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
      
      # output$prob <- renderPrint({
      #   
      #   mean(rnorm(input$s,mean(x), sd(x)))
      # })
      
      d <- (mean(rnorm(input$s,mean(x), sd(x))))
      print(d)
      output$histogram <- renderPlot({

        plot(d, main="Kernel Density of generated data")
        
        # hist(d, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))
        
        


      })
      
      
      # d <- (mean(rnorm(input$s,mean(x), sd(x))))
      # print(d)
      # output$tab <- renderPlot({
      #   
      #   polygon(d, col="red", border="blue")
      #   
      # })
      
      
    } 
    

    
    
    # exponential 
    
    
    if (input$conmodel == 'exponential')  
      
    { 
      
      print(mean(rexp(input$s,1/mean(x)))) 
      
    } 
    
    
    if (input$conmodel == 'uniform') 
      
      {
      
      
      print(mean(runif(input$s, min = 0, max = 1))) 
      
      
      
    } 
    
  })    

}
