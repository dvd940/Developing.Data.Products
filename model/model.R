##################################################################################

#  Model Prep

#################################################################################

cross.val <- trainControl(method = "repeatedcv", repeats = 3,
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE)
set.seed(1245)

##################################################################################

#  Model

#################################################################################


rf.grid <- data.frame(.mtry = c(2, 3))
# myFit <- train(Fate ~ Class + Sex + I(Title == "Mr") + I(Title == "Sir") + Family + Age
myFit <- train(Fate ~ Class + Sex + Title + Family + Age + Embarked,
              data = my.train,
              method = "rf",
              metric = "ROC",
              tuneGrid = rf.grid,
              trControl = cross.val)


saveRDS(myFit, "titanic.model.rds")

