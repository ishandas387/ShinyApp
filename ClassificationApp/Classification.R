library(shiny)
library(e1071) 
library(ggplot2)




#---------------------
# Define Clusters
#---------------------



iris.true <- iris[,5]
iris.train <- iris[,-5] # no cheating



# Naive Bayes
nb.classifier <- naiveBayes(iris.train, iris.true)
nb.clusters <- factor(as.integer(predict(nb.classifier,iris.train)))



# SVM
svm.classifier <- svm(iris.train, iris.true)
svm.clusters <- factor(as.integer(predict(svm.classifier,iris.train)))


# LR
# lr.classifier <- glm(iris.train, iris.true,family = 'binomial')
# lr.clusters <- factor(as.integer(predict(lr.classifier,iris.train)))



iris.result <- cbind(iris.train, nb.clusters, svm.clusters)



#---------------------
# Module Code
#---------------------




clusterUI <- function(id, train){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    selectInput(ns("x"), label = "X Variable", choices=colnames(train)),
    selectInput(ns("y"), label = "Y Variable", choices=colnames(train))
  )
  
}



cluster <- function(input, output,session,result, column){
  output$plot<- renderPlot({
    ggplot(result,aes_string(x=input$x, y=input$y,color=column)) + geom_point()
  })
  
}



#---------------------
# Application Code
#---------------------




ui <- fixedPage(
  titlePanel("Classification Comparison Tool"),
  p("SVM and Naive Bayes algorithms were used to cluster the iris dataset. Here we are plotting the 'fitted' results."),
  column(width = 6, 
         h1("Support Vector Machine"),
         clusterUI("svm", iris.train)
         
  ),
  column(width=6,
         h1("Naive Bayes"),
         clusterUI("nb", iris.train)
         
  ),

  column(width=6,
         h1("Logistic Regression"),
         clusterUI("lr", iris.train)
  )
  
)



server <- function(input, output){
  callModule(cluster,"svm", iris.result, "svm.clusters")
  callModule(cluster, "nb", iris.result, "nb.clusters")
  callModule(cluster, "lr", iris.result, "lr.clusters")
}



shinyApp(ui, server)