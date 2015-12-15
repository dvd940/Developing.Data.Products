## Import Libraries
library(plyr)
library(dplyr)
library(caret)

source("titanic.functions.R")


############################################

train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv

train.data <- read.csv("data/train.csv", header = TRUE, na.strings=c(""), colClasses = train.column.types)
test.data <- read.csv("data/test.csv", header = TRUE, na.strings=c(""), colClasses = test.column.types)

train.raw <- train.data  # save original
test.raw <- test.data  # save original

# Drop columns we won't use
drops <- c("Ticket")
train.data <- train.data[, !(names(train.data) %in% drops)]
test.data <- test.data[, !(names(test.data) %in% drops)]

# Feature Engineering
combined.file <- combine.files(train.data, test.data)
engineered.file <- get.titles(combined.file)
engineered.file <- impute.age(engineered.file)
engineered.file <- part.of.family(engineered.file)
engineered.file <- is.child(engineered.file)
engineered.file <- women.children.first(engineered.file)
engineered.file <- impute.embarked(engineered.file)
engineered.file <- update.class(engineered.file)
engineered.file <- update.survived(engineered.file)
names(engineered.file) <- make.names(names(engineered.file))
nas <- sapply(engineered.file,function(x) sum(is.na(x)))

#split our sets again.
train.data <- engineered.file[1:891,]
test.data <- engineered.file[892:1309,]


# Split Data into Train and Test partitions for analysis
train.split <- createDataPartition(y = train.data$Survived, p = 0.7, list = FALSE)  # a 70/30 split
my.train <- train.data[train.split,]
my.test <- train.data[-train.split,]



##################################################################################

#  Model

#################################################################################


source("model.R")



# ## Test the model
# C <- "First"
# S <- "female"
# A <- 24
# E <- "S"
# Tle <- "Mrs"
# Fam <- "Yes"
# 
# 
# my.test.data <- data.frame(Class = factor(C), Sex = factor(S), Age = A, 
#                            Embarked = factor(E), Title = factor(Tle), Family = factor(Fam))
# 
# 
# my.prediction <- predict(myFit, my.test.data, "prob")
# my.prediction
# 






