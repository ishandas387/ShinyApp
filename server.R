library(shiny)
library(pastecs)
function(input, output, session) {
  
  
  # output$plot <- renderPlot({
  #   plot(cars, type=input$plotType)
  # })
  
 #output$c8 <- renderText({
  #   print("xyz")
   #         print(summary(mtcars))
    #        res<-stat.desc(mtcars)
     #       df<-round(res, 2)
      #      print(df)
       #     return (df)

            
 #})
 # Generate a summary of the dataset (or subset by Iris.Species)
  output$summary <- renderPrint({
    dataset <- mtcars
     res<-stat.desc(dataset)
     df<-round(res, 2)
    return(df)
  })
  
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
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  output$c2 <- renderTable({
    rendingStuff()})


  output$c2summary <- renderPrint({
      summary(rendingStuff())

  })

  output$c1 <- renderTable({
    print("Entered")
    
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
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })



  output$sum <- renderTable({

  })
  output$histogram <- renderPlot({ 

     

      # binomial  

      if (input$dismodel == 'binomial') { 

      par(mfrow=c(1,2))  

      d <- density(rbinom(1000,input$n,input$p))  

      plot(d, main="Kernel Density of generated data")  

      polygon(d, col="red", border="blue") 

      x=0:input$n  

      plot(x,dbinom(x,input$n,input$p))  

       

      } 

       

       

      # poisson 

       

      if (input$dismodel == 'poisson') { 

      par(mfrow=c(1,2))   

      D=rpois(input$s, input$lam)  

      tab=table(D)  

      barplot(tab,col='blue')  

      x1=0:input$max  

      y1=dpois(x1,input$lam)  

      plot(x1,y1,type='b')  

      } 

          ##  

       

        

       

      # geometric  

      if (input$dismodel == 'geometric') { 

      par(mfrow=c(1,2)) 

      D=rgeom(input$s, input$p)  

      tab=table(D)  

      barplot(tab,col='blue')  

      x2=0:input$max  

      y2=dgeom(x2,input$p)  

      plot(x2,y2,type='b')  

      } 

        

    })    

      

    output$tab <- renderTable({  

       

      p1=dbinom(input$j1,input$n, input$p)  

      p2=dpois(input$j2,input$lam)  

      p3=dgeom(input$j3,input$p)  

       

       

      c(p1,p2,p3) 

       

       

      })  
}
