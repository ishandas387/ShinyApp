library(shiny)
library(pastecs)
library(data.table)

returnPimaIndianDataset <- function(){
      data("PimaIndiansDiabetes2", package = "mlbench")
      PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
      dataset=PimaIndiansDiabetes2
      return(dataset)  
}

runNaiveBayesForTheDataset <- function (trainset,testset,actual){
              nb_model <- naiveBayes(diabetes~ ., data=trainset) 
              pred_nb= predict(nb_model, testset) 
              pred_nb
              #confusionMatrix(pred_nb,actual)
              accuracy_nb=mean(pred_nb==actual)
              return (accuracy_nb)
}

runSvmForTheDataSet <- function(trainset,testset,actual){
      #build model linear kernel and C-classification (soft margin) with default cost (C=1) 
      svm_model <- svm(diabetes~ ., data=trainset, method="nu-classification", kernel="linear") 
      pred_svm= predict(svm_model, testset, type='response') 
      accuracy_svm=mean(pred_svm==actual) 
      return(accuracy_svm)
}

runMultiNomialLogisticRegrsForDataset <- function(trainset,testset,actual){

        mlr_model <- multinom(diabetes~ ., data=trainset) 
        pred_mlr= predict(mlr_model, testset) 
        accuracy_mlr=mean(pred_mlr==actual) 
        return(accuracy_mlr)
}

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
    DT::datatable(data.frame(dispdata), options = list(lengthChange = TRUE)) 
  }) 
   output$pima = DT::renderDataTable({ 
    DT::datatable(returnPimaIndianDataset(), options = list(lengthChange = TRUE))
   })
  
  output$ml <- renderPrint({

      library(e1071) 
      library(nnet) 
      dataset=returnPimaIndianDataset()  
      n=nrow(dataset) 
      indexes = sample(n,n*(80/100)) 
      trainset = dataset[indexes,] 
      testset = dataset[-indexes,] 
      actual=testset$diabetes
      acc=c(SVM =0,NB= 0, MLR =0); 
      mc=100 
      if (input$mlmodel =='NB') { 
             accuracy_nb =runNaiveBayesForTheDataset(trainset,testset,actual)
              print(accuracy_nb)
      }
      else if(input$mlmodel =='SVM'){
         accuracy_svm = runSvmForTheDataSet(trainset,testset,actual)
          print(accuracy_svm)
      }
      else if(input$mlmodel =='MLR'){
         accuracy_mlr = runMultiNomialLogisticRegrsForDataset(trainset,testset,actual)
          print(accuracy_mlr)
      }
      else{
          for(i in 1:mc){ 
            n=nrow(dataset) 
            indexes = sample(n,n*(80/100)) 
            trainset = dataset[indexes,] 
            testset = dataset[-indexes,] 
            # NB 
            accuracy_nb =runNaiveBayesForTheDataset(trainset,testset,actual)
            #SVM
            accuracy_svm = runSvmForTheDataSet(trainset,testset,actual)
            # Multinomial Logistic reg 
            accuracy_mlr = runMultiNomialLogisticRegrsForDataset(trainset,testset,actual)
            vect_accuracy=c(accuracy_svm,accuracy_nb,accuracy_mlr) 
            acc=acc+(1/mc)*vect_accuracy 
            } 
        plotdf <- as.data.frame(acc) 
        print(plotdf)
      }

  })
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
