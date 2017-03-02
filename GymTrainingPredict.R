# Update Library

# Set working directory
setwd("~/Desktop/R_PROJECT/DS/08_PRACTICAL_MACHINE_LEARING_PROJECT")

# Read in training data set
GymTrainData <- read.csv("./data/pml-training.csv")
GymTestData <- read.csv("./data/pml-testing.csv")

# It is very important to reduce the length of the variable names associated
attach(GymTrainData)

# View a summary of the Classe field data
table(GymTrainData$classe)

# List 50 headings of the data set

head(names(GymTrainData),180)

# View some data in Historgram
par(mfrow=c(2,2))
hist(roll_belt, main = "roll_belt")
hist(roll_forearm, main = "roll_forearm")
hist(roll_arm, main = "roll_arm")

# Find out which columns are in Factors
ColumnNames <- c()
nCounts = ncol(GymTrainData) - 1
for(i in 1:nCounts) 
{
  if(is.factor(GymTrainData[,i]))
  {
    ColumnNames <- c(ColumnNames, i)
  }
}
# Removed all Factor Columns
GymTrainData <- GymTrainData[,-ColumnNames]

# Removing 1-7 Associations fields which is not needed
GymTrainData <- GymTrainData[,-c(1,2,3,4,5,6,7)]

# Removed all 'NA'
GymTrainData <- GymTrainData[, colSums(is.na(GymTrainData)) == 0]


# Using Random Forest to Classify
# Load Random Forest Library
library(randomForest)
library(caret)

# Seed
SeedNum <- 13
Acccuracies <- c()
for(i in 1:1)
{
  set.seed(SeedNum)
  SeedNum <- SeedNum+1
  TrainIndex <- createDataPartition(y=GymTrainData$classe, p=0.75, list=FALSE)
  GymTrainSet <- GymTrainData[TrainIndex,]
  GymTestSet <- GymTrainData[-TrainIndex,]
  # Fit in Random Forest Model
  fit <- randomForest(classe ~., data = GymTrainSet, importance = T)
  # Prediction
  prediction <- predict(fit, GymTestSet)
  GymTestSet$rightPred <- prediction == GymTestSet$classe
  t <- table(prediction, GymTestSet$classe)
  print(t)
  # Accuracies
  accuracy <- sum(GymTestSet$rightPred)/nrow(GymTestSet)
  accuracies <- c(accuracies,accuracy)
  print(accuracy)
}

table(prediction, GymTestSet$classe)
plot(fit)
importance(fit)
varImpPlot(fit)
margins.rf=margin(fit, GymTrainSet)
plot(margins.rf)
hist(margins.rf, main="Margins of Random Forest for Gym Training Dataset")
boxplot(margins.rf~GymTrainData$classe, main="Margins of Random Forest for Gym Training Dataset by classe")

#
library(party)
classe.cforest = cforest(classe ~ ., data = GymTrainSet, controls = cforest_unbiased(ntree=1000, mtry=5))
class.cforest

# Applying the Random Forest into 20 Test Cases
fit <- randomForest(classe ~., data = GymTrainData)
nrow(GymTestData)
prediction <- predict(fit, GymTestData)
prediction

