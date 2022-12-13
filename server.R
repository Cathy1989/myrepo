library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(shinydashboard)
library(caret)
library(leaps)
library(randomForest)
cars <- read_csv("cars.csv")

shinyServer(function(input, output, session) {
  #Second tab content
  getData <- reactive({
    origins <- input$origin
    
    newData <- cars %>% filter(Origin == origins)
    newData
  })
  
  #create plot
  output$MPGPlot <- renderPlot({
    #get data
    carsData <- getData()
    
    #base plotting object
    g <- ggplot(carsData, aes(x = Weight, y = MPG))
    g + geom_point()+
      geom_smooth(method = "lm")

  })
  
  output$barPlot <- renderPlot({
    newData2<- cars %>% filter(Year %in% c(70, 75, 80) & Cylinders %in% c(4, 6, 8))
    g <- ggplot(newData2, aes(x = Origin))  
    
    if(input$plot == "Just Origin"){
      g + geom_bar()
    } else if(input$plot == "Origin and Year"){ 
      g + geom_bar(aes(fill = as.factor(Year)), position="dodge")
    } else if(input$plot == "Origin and Cylinders"){ 
      g + geom_bar(aes(fill = as.factor(Cylinders)), position="dodge")
    }
  })
  
  output$summary <- DT::renderDataTable({
    var <- input$var
    tab <- cars %>% 
    group_by(Origin) %>%
      summarize(
        mean = mean(get(var)),
        median= median(get(var)), 
        variance= var(get(var))
        )
    tab
    })
  #Third tab content  
  
  output$ex1 <- renderUI({
    withMathJax(
      helpText("The multiple linear regression equation is as follows: $$Y= b_0+b_1X_1+b_2X_2+...+ b_kX_k $$")
    )
  })
      
      output$ex2 <- renderUI({
        withMathJax(
          helpText("The mean squared error (MSE): $$MSE=1/N*\\Sigma(f_i-y_i)^2$$")
     
    )
  })
      
      output$ex3 <- renderUI({
        withMathJax(
          helpText("For a binary response, within a given node (p=P(correct classification)). Usually use Gini index: $$2p*(1-p)$$")
        )
      })
      
      
      
      output$summary2 <- DT::renderDataTable({
        
        
        cars$Origin <- factor(cars$Origin)
        carsData<- cars %>% select (-Model)
        trainIndex <- createDataPartition(carsData$Displacement, p = 0.7, list = FALSE)
        trainData <- carsData[trainIndex, ]
        testData <- carsData[-trainIndex, ]
        
        fit_LM <- train(MPG ~ Weight + Year +  Origin , data = trainData,
                        method = "lm",
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "cv", number = 10))
        tab<- fit_LM$results
        tab
        
      })
      output$summary3 <- DT::renderDataTable({
        
        
        cars$Origin <- factor(cars$Origin)
        carsData<- cars %>% select (-Model)
        trainIndex <- createDataPartition(carsData$Displacement, p = 0.7, list = FALSE)
        trainData <- carsData[trainIndex, ]
        testData <- carsData[-trainIndex, ]
        
      fit_tree <- train(MPG ~ Weight + Year +  Origin, data = trainData, 
                        method = "rpart",
                        trControl = trainControl(method = "cv", number = 10),
                        preProcess = c("center", "scale"),
                        tuneGrid = data.frame(cp = seq(0, 0.1, 0.001)))
      tab<- fit_tree$results
      tab
      })
  
      output$summary4 <- renderPlot({
        
        
        cars$Origin <- factor(cars$Origin)
        carsData<- cars %>% select (-Model)
        trainIndex <- createDataPartition(carsData$Displacement, p = 0.7, list = FALSE)
        trainData <- carsData[trainIndex, ]
        testData <- carsData[-trainIndex, ]
        
      fit_forest <- train(MPG ~ Weight + Year +  Origin, data = trainData, 
                          method = "rf",
                          trControl = trainControl(method = "cv", number = 10),
                          preProcess = c("center", "scale"))
      
      rf <- randomForest(MPG ~ Weight + Year +  Origin, data = trainData, ntree=1000, keep.forest=FALSE,
                         importance=TRUE)
      b<-varImpPlot(rf)
      b
      
      })
      output$summary5 <- DT::renderDataTable({
        pred <- predict(fit, newdata = testData) 
        m1<- postResample(pred, obs = testData$MPG)
        pred_tree <- predict(fit_tree, newdata = testData)
        m2<-postResample(pred_tree, testData$MPG)
        pred_forest <- predict(fit_forest, newdata = testData)
        m3<- postResample(pred_forest, testData$MPG)
        lm<- tibble(model = c("lm"), RMSE = c(m1[[1]]), Rsquared = c(m1[[2]]))
        
        fit_tree<- tibble(model = c("fit_tree"), RMSE = c(m2[[1]]), Rsquared = c(m2[[2]]))
        
        randomForest<- tibble(model = c("randomForest"), RMSE = c(m3[[1]]), Rsquared = c(m3[[2]]))
        
        comparison<- rbind(lm, fit_tree, randomForest)
        comparison
      })
      
      
  #Fourth tab content
  output$table1 <- DT::renderDataTable({
    #get data
    tab<- cars
    tab
  })
  
  output$table2 <- DT::renderDataTable({
    data <- cars
    if (input$ori != "All") {
      data <- data[data$Origin == input$ori,]
    }
    
    if (input$mpg != "All") {
      data <- data[data$MPG == input$mpg,]
    }
    if (input$cyl != "All") {
      data <- data[data$Cylinders == input$cyl,]
    }
    data
  })
  
}
)
  
  
  