library(ggplot2)
library(tidyverse)
library(DT)
library(shinydashboard)
library(caret)
library(leaps)
library(randomForest)
cars <- read_csv("cars.csv")

shinyUI<- dashboardPage(
  skin = "green",
        dashboardHeader(title= "Cars dashboard"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("About Page", tabName = "about"),
            menuItem("Data Exploration Page", tabName = "Data_exp"),
            menuItem("Modeling Page",
                     menuSubItem("Modeling Info", tabName = "Model_Information"),
                     menuSubItem("Model Fitting", tabName = "Model_Fitting"), 
                     menuSubItem("Prediction", tabName = "Model_Prediction")
                     ),
            menuItem("Data Page", tabName = "Data")
          )
        ),
        dashboardBody(
          
          tabItems(
            # First tab content
            tabItem(tabName = "about",
                    titlePanel("Basic Introduction"),
                    fluidRow(
                      box(
                        title = "The purpose of the app", 
                        width = "6 col-lg-4",
                        tags$p(
                          "This a dataset about cars. We have night different variables. They are",
                          " model, MPG, Cylinders, Displacement, Horsepower, Weight, Acceleration,", 
                          " Year, and Origin. We're trying to provide users multiple ways to analysis",
                          "the data and use three different model types to fit the models. At last,",
                          "users could get a conclusion by themself to determine which factors have",
                          "the greatest impact on MPG, and what are the most important factors when",
                          "buying a car."
                        )
                        ),
                      box(
                        title= "The data and its source",
                        width = "6 col-lg-4",
                        tags$p(
                          "This data is from our course database 'cars.csv'."
                        )
                      ),
                      box(
                        title= "The purpose of each tab of the app",
                        width = "6 col-lg-4",
                        tags$p(
                          "The About page is talking about the purpose of each page and why we created this app.",
                          "The Data Exploration page includes several data analysis on different variables, and",
                          "the result could help users to see the trend between variables.",
                          "The Modeling page have three tabs: Modeling Info tab, Model Fitting tab, and Prediction",
                          " tab. Users could have a lot of choices to design their model and get the results.",
                          "The Data page foucus on the subset users would like to get."
                        )
                      ),
                      box(
                        title = "About this Cars dataset",
                        # status = "primary",
                        width = "6 col-lg-4",
                        tags$p(
                          class = "text-center",
                          tags$a(
                            target = "_blank",
                            tags$img(class = "image-responsive",
                                     src = "https://user-images.githubusercontent.com/66736646/88664543-06f24500-d0ff-11ea-89f3-6e7cb8966ae9.png",
                                     style = "max-width: 200px;"
                            )
                  ))))),
            # Second tab content
              tabItem(tabName = "Data_exp",
                      titlePanel("Explore Data Analysis"),
                      sidebarLayout(
                        sidebarPanel(
                          h4("Select the origin of cars:"),
                          selectInput("origin", "Origin", selected = "US", choices = levels(as.factor(cars$Origin))),
                          br(),
                          h4("You can create a few bar plots using the radio buttons below."),
                          radioButtons("plot", "Select the Plot Type", choices = list("Just Origin", "Origin and Year", "Origin and Cylinders"), selected = "Just Origin"),
                          br(),
                          h4("You can find the", strong("sample mean"), " for a few variables below:"),
                          selectInput("var", label = "Variables to Summarize", 
                                      choices = c("MPG", "Displacement", "Acceleration", "Weight"),
                                      selected = "MPG")),
                        
                        # Show outputs
                        
                        mainPanel(
                          plotOutput("MPGPlot"),
                          plotOutput("barPlot"),
                          dataTableOutput("summary")
                        )
                      )  
          ),
            # Third tab content
              tabItem(tabName = "Model_Information",
                      titlePanel("Introduction about Three Models"),
                      
                     fluidRow(
                        box(
                          title = "Multiple Linear Regression", 
                          tags$p("Multiple linear regression is a regression model that estimates the relationship between",
                                 "a quantitative dependent variable and two or more independent variables using a straight",
                                 "line. There are two main advantages to analyzing data using a multiple regression model.",
                                 "The first is the ability to determine the relative influence of one or more predictor" ,
                                 "variables to the criterion value. The second advantage is the ability to identify ",
                                 "outliers, or anomalies. Multiple regression will not be good at explaining the ",
                                 "relationship of the independent variables to the dependent variables if those",
                                 "relationships are not linear.",
                                 withMathJax(),
                                 uiOutput("ex1")
                      
          )),
                        box(
                           title = "Classification Tree", 
                           tags$p("A Classification tree labels, records, and assigns variables to discrete classes.",
                                  "It can also provide a measure of confidence that the classification is correct.",
                                  "Compared to other algorithms, classification tree requires less effort for data", 
                                  "preparation during pre-processing, and it does not require normalization of data. In addition,",
                                  "it does not require scaling of data as well. The disadvantage of classification tree is a small change",
                                  "in the data can cause a large change in the structure of the Classification tree causing instability.",
                                  "Sometimes, calculation can go far more complex compared to other algorithms. It often involves higher",
                                  "time to train the model.",
                                  withMathJax(),
                                  uiOutput("ex2")
                                  
            )),
                        box(
                           title = "Random Forest Model", 
                           tags$p("Random Forest is a powerful algorithm in Machine Learning. It is based on the Ensemble Learning",
                                  "technique (bagging). The advantages of Random Forest is that it reduces overfitting problem in",
                                  "decision trees and also reduces the variance and therefore improves the accuracy. It can also be",
                                  "used to solve both classification as well as regression problems. Random Forest works well with both",
                                  "categorical and continuous variables. The disadvantages are Random Forest creates a lot of trees.",
                                  "To do so, this algorithm requires much more computational power and resources. It also require much",
                                  "more time to train as compared to decision trees as it generates a lot of trees and makes decision on the majority of votes.",
                                  withMathJax(),
                                  uiOutput("ex3")
                   
            )))),
          
             tabItem(tabName = "Model_Fitting",
                     titlePanel("Fit the Three Models"),
                     sidebarLayout(
                       sidebarPanel(
                         h4("Choose the proportion of training dataset:"),
                         sliderInput("size", "Size of training dataset",
                                     min = 0, max = 1.0, value = 0.7, step = 0.1),
                         br(),
                         h4("Choose model settings:"),
                         selectizeInput("settings", "Settings", selected = "leapSeq", choices = c("leapSeq", "leapForward", "leapBackward")), 
                         br(),
                         h4("Select the variables used as predictors:"),
                         selectizeInput("variable", "Variables", selected = "Weight", choices = c("Displacement", "Weight", "Year", "Origin", "Cylinders", "Horsepower", "Acceleration")), 
                         br(),
                         actionButton("fit", label = "Fit Models")
                       ),
                       mainPanel(
                         dataTableOutput("summary2"),
                         dataTableOutput("summary3"),
                         plotOutput("summary4"),
                         dataTableOutput("summary5")
                       ))
                         
                       
                  
          
        ),
             tabItem(tabName = "Model_Prediction",
                     titlePanel("Three Models Predictions "),
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput("cyl", "Cylinders",
                                     min = 3, max = 8, value = 4, step = 1),
                         sliderInput("dis", "Displacement",
                                     min = 68, max = 455, value = 110, step = 30),
                         sliderInput("hor", "Horsepower",
                                     min = 46, max = 230, value = 80, step = 20),
                         sliderInput("wei", "Weight",
                                     min = 1613, max = 5140, value = 2230, step = 350),
                         sliderInput("acc", "Acceleration",
                                     min = 8, max = 24.8, value = 14, step = 1.5),
                         sliderInput("yea", "Year",
                                     min = 70, max = 82, value = 75, step = 1)
                       ),
                       mainPanel(
                         dataTableOutput("prediction")  
                         
                  
          )
                     )
             ),
            # Fourth tab content
        tabItem(tabName = "Data",
                titlePanel("Basic DataTable"),
                
                navbarPage(
                  "shinythemes",
                  tabPanel("Navbar 1",
                fluidRow(
                  column(4,
                         selectInput("ori",
                                     "Origin:",
                                     c("All",
                                       unique(as.character(cars$Origin))))
                  ),
                  column(4,
                         selectInput("mpg",
                                     "MPG:",
                                     c("All",
                                       unique(as.character(cars$MPG))))
                  ),
                  column(4,
                         selectInput("cyl",
                                     "Cylinders:",
                                     c("All",
                                       unique(as.character(cars$Cylinders))))
                  ),
                
               
                ),
                
                mainPanel(
                  dataTableOutput("table1")
                )
                  ),
                tabPanel("Navbar 2",
                         mainPanel(   
                  dataTableOutput("table2")
                
        )
                )
        
                ),
        downloadButton("downloadData", "Download")
                             
                      
          )

)
)
)





