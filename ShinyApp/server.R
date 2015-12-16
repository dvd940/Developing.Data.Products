library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)


# Read in the full titanic data set
titanic.data <-
  read.csv("data/titanic-all.csv", header = TRUE, na.strings = c(""))

# Create a new variable called Fate based on the "survived" column
titanic.data <-
  mutate(titanic.data, Fate = ifelse(survived == 1, "Survived", "Perished"))
titanic.data$survived <- NULL

# Read in the prediction model
titanic.model <- readRDS("model/titanic.model.rds")


shinyServer(function(input, output) {
  filtered <- reactive({
    if (is.null(input$sexInput)) { 
      return(NULL)
    }
    
    
    titanic.data %>%
      filter(
        sex %in% input$sexInput,
        age >= input$ageInput[1],
        age <= input$ageInput[2],
        pclass %in% input$classInput
      )
  })
  
  
  # Explore
  output$plot1 <- renderPlot({
    if (is.null(input$sexInput)) { 
      return(NULL)
    }
    pl <- ggplot(filtered(), aes(factor(Fate), fill = sex))
    pl <- pl + geom_bar(width = .5)
    pl <- pl + ylim(0,1000)
    pl
  })
  
  output$plot2 <- renderPlot({
    pl <- ggplot(filtered(), aes(factor(sex), fill = Fate))
    pl <- pl + geom_bar(width = .5)
    pl <- pl + ylim(0,1000)
    pl
    
    
  })
  
  ## Predict
  
  prediction.text <- reactive({
    input.data <- data.frame(
      Class = factor(input$classInput2),
      Sex = factor(input$sexInput2),
      Age = input$ageInput2,
      Family = factor(input$familyInput2),
      Title = factor(input$titleInput2),
      Embarked = factor(input$embarkedInput2)
    )
    # print(input.data)
    prediction <- predict(titanic.model, input.data, "prob")
    
  })
  
  
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
    prediction <- predict(titanic.model, input.data, "prob")
    paste("With the chosen attributes, this passenger has a ", round(prediction[,2] * 100, digits = 2), "% chance of surviving the titanic disaster.")
  })
  
  ## Table
  output$table <- renderDataTable({
    titanic.data
  }, options = list(bFilter = FALSE, iDisplayLength = 50))
  
})