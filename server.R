library(shiny)
library(pastecs)
library(data.table)
library(caret)
library(e1071) 
library(nnet) 


############################## Functions ################################################

isMontecarlo = FALSE
returnPimaIndianDataset <- function(){
      data("PimaIndiansDiabetes2", package = "mlbench")
      PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
      dataset=PimaIndiansDiabetes2
      return(dataset)  
}

runNaiveBayesForTheDataset <- function (trainset,testset,actual){
              if(!isMontecarlo){
                set.seed(1000)
              }
              nb_model <- naiveBayes(diabetes~ ., data=trainset) 
              pred_nb= predict(nb_model, testset) 
               ctx <- confusionMatrix(pred_nb,actual)
              return (ctx)
}

runSvmForTheDataSet <- function(trainset,testset,actual){
    if(!isMontecarlo){
                set.seed(1001)
              }
      #build model linear kernel and C-classification (soft margin) with default cost (C=1) 
      svm_model <- svm(diabetes~ ., data=trainset, method="nu-classification", kernel="linear") 
      pred_svm= predict(svm_model, testset, type='response') 
      ctx_svm <- confusionMatrix(pred_svm,actual)
      return(ctx_svm)
}

runMultiNomialLogisticRegrsForDataset <- function(trainset,testset,actual){
        if(!isMontecarlo){
                set.seed(1002)
              }
        mlr_model <- multinom(diabetes~ ., data=trainset,trace=FALSE) 
        pred_mlr= predict(mlr_model, testset) 
        ctx_mlr <- confusionMatrix(pred_mlr,actual)
        return(ctx_mlr)
}

returnAccuracyFromConfusionMatrix <- function(confusionMatrix){
          accuracy <- confusionMatrix$overall['Accuracy'] 
          return (round(accuracy*100,2))
}

draw_confusion_matrix <- function(cmtrx, headerName) {

                total <- sum(cmtrx$table)
                res <- as.numeric(cmtrx$table)
                # Generate color gradients. Palettes come from RColorBrewer.
                greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
                redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
                getColor <- function (greenOrRed = "green", amount = 0) {
                if (amount == 0)
                return("#FFFFFF")
                palette <- greenPalette
                if (greenOrRed == "red")
                palette <- redPalette
                colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
                }
                # set the basic layout
                layout(matrix(c(1,1,2)))
                par(mar=c(2,2,2,2))
                plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
                title(paste('CONFUSION MATRIX ',headerName,sep=" "), cex.main=2)
                # create the matrix
                classes = colnames(cmtrx$table)
                rect(150, 430, 240, 370, col=getColor("green", res[1]))
                text(195, 435, classes[1], cex=1.2)
                rect(250, 430, 340, 370, col=getColor("red", res[3]))
                text(295, 435, classes[2], cex=1.2)
                text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
                text(245, 450, 'Actual', cex=1.3, font=2)
                rect(150, 305, 240, 365, col=getColor("red", res[2]))
                rect(250, 305, 340, 365, col=getColor("green", res[4]))
                text(140, 400, classes[1], cex=1.2, srt=90)
                text(140, 335, classes[2], cex=1.2, srt=90)
                # add in the cmtrx results
                text(195, 400, res[1], cex=1.6, font=2, col='white')
                text(195, 335, res[2], cex=1.6, font=2, col='white')
                text(295, 400, res[3], cex=1.6, font=2, col='white')
                text(295, 335, res[4], cex=1.6, font=2, col='white')
                # add in the specifics
                plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
                text(10, 85, names(cmtrx$byClass[1]), cex=1.2, font=2)
                text(10, 70, round(as.numeric(cmtrx$byClass[1]), 3), cex=1.2)
                text(30, 85, names(cmtrx$byClass[2]), cex=1.2, font=2)
                text(30, 70, round(as.numeric(cmtrx$byClass[2]), 3), cex=1.2)
                text(50, 85, names(cmtrx$byClass[5]), cex=1.2, font=2)
                text(50, 70, round(as.numeric(cmtrx$byClass[5]), 3), cex=1.2)
                text(70, 85, names(cmtrx$byClass[6]), cex=1.2, font=2)
                text(70, 70, round(as.numeric(cmtrx$byClass[6]), 3), cex=1.2)
                text(90, 85, names(cmtrx$byClass[7]), cex=1.2, font=2)
                text(90, 70, round(as.numeric(cmtrx$byClass[7]), 3), cex=1.2)
                # add in the accuracy information
                text(30, 35, names(cmtrx$overall[1]), cex=1.5, font=2)
                text(30, 20, round(as.numeric(cmtrx$overall[1]), 3), cex=1.4)
                text(70, 35, names(cmtrx$overall[2]), cex=1.5, font=2)
                text(70, 20, round(as.numeric(cmtrx$overall[2]), 3), cex=1.4)
  }

function(input, output, session) {
  
################# Returning Data Frame #############################

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
    
    return(df)
  })
  
  
  ##################### Summary #################################
  
  output$c2summary <- renderPrint({
      summary(rendingStuff())
  })
  
  ###################Plot Data Table ############################
  
  output$ex2 <- renderPlot({

     x  <- rendingStuff()
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #
    hist(x$Score, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })

  ############################# Convert into Datatable #########################################
  
  output$ex1 <- DT::renderDataTable(
    DT::datatable(rendingStuff(), options = list(pageLength = 10))
  )

  
  ######################### MACHINE LEARNING ###################################
   output$pima = DT::renderDataTable({ 
    DT::datatable(returnPimaIndianDataset(), options = list(lengthChange = TRUE))
   })

  output$scatterPima  <- renderPlot({
      dataset=returnPimaIndianDataset()  
      if(input$xaxis != input$yaxis && (input$xaxis !="diabetes" && input$yaxis !="diabetes" )){
        ggplot(dataset, aes_string(x = input$xaxis, y = input$yaxis)) +
        geom_point(aes(color = factor(diabetes)))
      }else{
        #textInput("defaultwarning", placeholder = "What year(s) does this survey cover?")
        ggplot(dataset, aes(x = glucose, y = age)) +
        geom_point(aes(color = factor(diabetes)))
      }
  })
  
output$xaxis <- renderUI({
    dataset<- returnPimaIndianDataset()
    selectInput("xaxis",  choices = colnames(dataset), label = "X-Axis")
  })

output$yaxis <- renderUI({
    dataset<- returnPimaIndianDataset()
    selectInput("yaxis",  choices = colnames(dataset), label = "Y-Axis")
  })

  output$ml <- renderPrint({

      
      dataset=returnPimaIndianDataset()  
      n=nrow(dataset) 
      indexes = sample(n,n*(80/100)) 
      trainset = dataset[indexes,] 
      testset = dataset[-indexes,] 
      actual=testset$diabetes
      accuracyMatrix=c(SVM =0,NB= 0, MLR =0); 
      mc=100 
      if (input$mlmodel =='NB') { 
              set.seed(120)
             ctx =runNaiveBayesForTheDataset(trainset,testset,actual)
             if(!isMontecarlo){
               output$ctx <- renderPlot({
                 draw_confusion_matrix(ctx, "-NB")
               })
             }
              print(returnAccuracyFromConfusionMatrix(ctx))
      }
      else if(input$mlmodel =='SVM'){
         ctx_svm = runSvmForTheDataSet(trainset,testset,actual)
          if(!isMontecarlo){
               output$ctx <- renderPlot({
                 draw_confusion_matrix(ctx_svm,"-SVM")
               })
             }
           print(returnAccuracyFromConfusionMatrix(ctx_svm))
      }
      else if(input$mlmodel =='MLR'){
         ctx_svm_mlr = runMultiNomialLogisticRegrsForDataset(trainset,testset,actual)
         if(!isMontecarlo){
               output$ctx <- renderPlot({
                 draw_confusion_matrix(ctx_svm_mlr,"-MLR")
               })
             }
            print(returnAccuracyFromConfusionMatrix(ctx_svm_mlr))
      }
      else{
          isMontecarlo = TRUE
          for(i in 1:mc){ 
            n=nrow(dataset) 
            indexes = sample(n,n*(80/100)) 
            trainset = dataset[indexes,] 
            testset = dataset[-indexes,] 
            # NB 
            accuracy_nb =returnAccuracyFromConfusionMatrix(runNaiveBayesForTheDataset(trainset,testset,actual))
            #SVM
            accuracy_svm = returnAccuracyFromConfusionMatrix(runSvmForTheDataSet(trainset,testset,actual))
            # Multinomial Logistic reg 
            accuracy_mlr = returnAccuracyFromConfusionMatrix(runMultiNomialLogisticRegrsForDataset(trainset,testset,actual))
            vect_accuracy=c(accuracy_svm,accuracy_nb,accuracy_mlr) 
            accuracyMatrix=accuracyMatrix+(1/mc)*vect_accuracy 
            } 
        plotdf <- as.data.frame(accuracyMatrix) 
        print(plotdf)
        output$ctx <- renderPlot({
       p <-ggplot(plotdf, aes(x=seq_along(accuracyMatrix),y=accuracyMatrix))+
        geom_bar(stat="identity")+theme_minimal()

        })
      }

  })


  
  
  
#################################### CONTINUOUS PROBABILITY #######################################

  
  output$summary = DT::renderDataTable({ 
    
    if (input$dataset =='Seatbelts') { dispdata <- Seatbelts } 
    else if (input$dataset =='USArrests') { dispdata <- USArrests } 
    DT::datatable(data.frame(dispdata), options = list(lengthChange = TRUE)) 
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
        
        #hist(d, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))
        
        


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
