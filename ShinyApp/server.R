library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)


# Read in the full titanic data set
titanic.data <- read.csv("data/titanic-all.csv", header = TRUE, na.strings = c(""))

# Create a new variable called Fate based on the "survived" column
titanic.data <- mutate(titanic.data, Fate = ifelse(survived == 1, "Survived", "Perished"))
titanic.data$survived <- NULL

titanic.data<- mutate(titanic.data, Age = age%/%1)  # Create an integer Age variable

titanic.model <- readRDS("model/titanic.model.rds") # Read in the prediction model

# Shiny Server
shinyServer(function(input, output) {
  
  # reactive variable that filters based on the sex, age and class inputs.
  filtered <- reactive({
    titanic.data %>%
      filter(
        sex %in% input$sexInput,
        age >= input$ageInput[1],
        age <= input$ageInput[2],
        pclass %in% input$classInput
      )
  })
  
  
  #################### Explore ####################
  
  # Generate chart of Fate versus Sex
  output$plot1 <- renderChart2({
    dataT <- data.frame(table(filtered()$Fate, filtered()$sex))
    names(dataT) <- c("Fate", "Sex", "Freq")
    r1 <- nPlot(Freq ~ Fate, group = "Sex", data = dataT, type = 'multiBarChart')
    return(r1)
  })
  
  # Generate chart of Fate versus Class
  output$plot2 <- renderChart2({
    dataT <- data.frame(table(filtered()$Fate, filtered()$pclass))
    names(dataT) <- c("Fate", "Class", "Freq")
    r2 <- nPlot(Freq ~ Fate, group = "Class", data = dataT, type = 'multiBarChart')
    return(r2)
  })
  
  # Generate histogram of ages
  output$plot3 <- renderChart2({
    histg <- hist(filtered()$Age, breaks = 10, plot = FALSE)
    print(histg)
    ages <- data.frame("Ages" = histg$breaks[-1], "Freq" = histg$counts)
    Histogram1 <- nPlot(x = "Ages", y = "Freq", data=ages,type='discreteBarChart')
    Histogram1
  })
  
  
  #################### Predict ####################
  
  # reactive variable that filters based on the inputs and then makes 
  # a prediction using the model read in from titanic.model.rds.
  prediction.text <- reactive({
    input.data <- data.frame(
      Class = factor(input$classInput2),
      Sex = factor(input$sexInput2),
      Age = input$ageInput2,
      Family = factor(input$familyInput2),
      Title = factor(input$titleInput2),
      Embarked = factor(input$embarkedInput2)
    )
  })
  
  # We need to differentiate between Male and Female since the titles will be different.
  output$predictionText <- renderText({
    if (input$sexInput2 == "male") {
      input.data <- data.frame(
        Class = factor(input$classInput2),
        Sex = factor(input$sexInput2),
        Age = input$ageInput2,
        Family = factor(input$familyInput2),
        Title = factor(input$titleInput2),
        Embarked = factor(input$embarkedInput2)
      )
    } else {
      input.data <- data.frame(
        Class = factor(input$classInput2),
        Sex = factor(input$sexInput2),
        Age = input$ageInput2,
        Family = factor(input$familyInput2),
        Title = factor(input$titleInput3),
        Embarked = factor(input$embarkedInput2)
      )
      
    }
    prediction <- predict(titanic.model, input.data, "prob") # Make the prediction
    paste("With the chosen attributes, this passenger has a ", round(prediction[,2] * 100, digits = 2), 
          "% chance of surviving the titanic disaster.")
  })
  
  #################### Table ####################
  # Print out a table of the data.
  output$table <- renderDataTable({
    titanic.data
  }, options = list(bFilter = FALSE, iDisplayLength = 50))
  
})