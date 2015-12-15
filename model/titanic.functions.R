### Feature Engineering ###

# Combine files to get a more complete set for feature engineeing
combine.files <- function(train.file, test.file){
  test.file$Survived <- NA
  combined <- rbind(train.file, test.file)
  return(combined)
}


# Combine certain titles based on social status
get.titles <- function(input.file){
  input.file$Name <- as.character(input.file$Name)
  input.file$Title <- sapply(input.file$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  input.file$Title <- sub(' ', '', input.file$Title)  # Strip off spaces
  
#   input.file$Title[input.file$Title %in% c('Mme', 'Mlle', "Miss")] <- 'Mlle'
#   input.file$Title[input.file$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Rev', 'Col')] <- 'Sir'
#   input.file$Title[input.file$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
#   input.file$Title[input.file$Title %in% c('Dr.')] <- 'Dr'
  
  input.file$Title <- as.factor(input.file$Title)
  return(input.file)
}

# Is this person part of a family
part.of.family <- function(input.file){
  input.file$Family <- "No"
  input.file$Family[which(input.file$SibSp != 0 | input.file$Parch != 0)] <- "Yes"
  input.file$Family <- as.factor(input.file$Family)
  return(input.file)
}

# Fill in Age NAs based on people of same "Title"
impute.age <- function(input.file){
  # Sir & Lady do not have NA for age.
  Mlle <- filter(input.file, Title == "Mlle")
  input.file$Age[is.na(input.file$Age)] <- mean(Mlle$Age,na.rm=TRUE)
  
  Dr <- filter(input.file, Title == "Dr")
  input.file$Age[is.na(input.file$Age)] <- mean(Dr$Age,na.rm=TRUE)
  
  Mr <- filter(input.file, Title == "Mr")
  input.file$Age[is.na(input.file$Age)] <- mean(Mr$Age,na.rm=TRUE)
  
  Master <- filter(input.file, Title == "Master")
  input.file$Age[is.na(input.file$Age)] <- mean(Master$Age,na.rm=TRUE)
  
  Mrs <- filter(input.file, Title == "Mrs")
  input.file$Age[is.na(input.file$Age)] <- mean(Mrs$Age,na.rm=TRUE)
  
  Ms <- filter(input.file, Title == "Ms")
  input.file$Age[is.na(input.file$Age)] <- mean(Ms$Age,na.rm=TRUE)
  
  return(input.file)
  
}

get.deck <- function(input.file) {
  input.file$Deck <- "0"
  input.file$Deck<-sapply(input.file$Cabin, function(x) strsplit(x,NULL)[[1]][1])
  input.file$Deck <- as.factor(input.file$Deck)
  return(input.file)
}
  
# Fill in embarkation NAs
impute.embarked <- function(input.file){
  # Most people embarked from Southhampton so set NA to 'S'
  input.file$Embarked[is.na(input.file$Embarked)] <- "S"
  input.file$Embarked <- as.factor(input.file$Embarked)
  return(input.file)
}

# Is this person a child
is.child <- function(input.file){
  input.file$Child <- "No"
  input.file$Child[input.file$Age < 14] <- "Yes"
  input.file$Child <- as.factor(input.file$Child)
  return(input.file)
}

# Woman or Child?
women.children.first <- function(input.file){
  input.file$Women.and.child <- "No"
  input.file$Women.and.child[which(input.file$Sex == "female" | input.file$Child == "Yes")] <- "Yes"
  input.file$Women.and.child <- as.factor(input.file$Women.and.child)
  return(input.file)
}

# Update the Pclass
update.class <- function(input.file) {
  input.file$Class <- input.file$Pclass 
  input.file$Class <- revalue(input.file$Class, c("1"="First", "2"="Second", "3"="Third"))
  input.file$Class <- as.factor(input.file$Class)
  return(input.file)
}

update.survived <- function(input.file) {
  input.file$Fate <- input.file$Survived
  input.file$Fate <- revalue(input.file$Fate, c("1" = "Survived", "0" = "Perished"))
  input.file$Fate <- as.factor(input.file$Fate)
  return(input.file)
}

# Drop columns we used to engineer new variables but won't be used further.
cleanup.file <- function(input.file){
  # Drop columns that won't be used
  drop <- c("SibSp", "Name", "Parch")
  input.file <- input.file[, !(names(input.file) %in% drop)]
  return(input.file)
}
