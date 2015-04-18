require('ggplot2')
require('doBy')
require('caret')
require('rpart')
#fill NA with mean of column
fillMean <- function(col) {
  col[is.na(col)] = mean(col, na.rm=TRUE)
  return(col)
}

#Load Data
#############################
readData <- function(file.name, column.types, missing.types) {
  read.csv(file.name, colClasses= column.types, na.strings=missing.types)
}

train.data.file <- "train.csv"  #training data
test.data.file <- "test.csv"    #test data
missing.types <- c("NA","")     #what to fill for missing types

#define the data types we want to use for each column
train.column.types <- c(
  'integer',   # PassengerId
  'factor',    # Survived
  'numeric',    # Pclass
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
#test data doesn't include the Survived column
test.column.types <- train.column.types[-2]

#load data
train.raw <- readData(train.data.file, train.column.types, missing.types)
test.raw <- readData(test.data.file,test.column.types,missing.types)

df.train = train.raw
df.test = test.raw

#format training data
df.train$Age = fillMean(df.train$Age)


#Format test data
#########################
#get rid of NA values in the testing set
df.test$Age = fillMean(df.test$Age)

#passenger 1044 (row 153) has the only NA fare value
#he belongs to Pclass == 3, so fill it with the average of all other Pclass == 3 passengers
df.test[is.na(df.test$Fare), ]$Fare = mean(subset(df.test, df.test$Pclass == 3)$Fare, na.rm = TRUE)

#Analyze Data
##########################
formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare
tree <- rpart(formula, method='class', data = df.train)
plotcp(tree)

plot(tree, uniform=TRUE, main="Classification Tree")
text(tree, use.n=TRUE,all=TRUE,cex=.8)

treeReg <- rpart(formula, method='anova',data=df.train)
plot(treeReg, uniform=TRUE, main="Regression Tree")
text(treeReg, use.n=TRUE, all=TRUE, cex=.8)
#fit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = df.train, family=binomial)
#survived.glm <- predict(fit, df.test, type='response')

fitControl <- trainControl(method="repeatedcv",
                           number=10,
                           repeats=1,verbose=TRUE)
set.seed(117)
fit1<-train(formula, 
            data=df.train, 
            method='gbm',
            trControl = fitControl,
            verbose=FALSE
            )
print("FIT1 COMPLETE.  SETTING GRID")
fitGrid<-expand.grid(interaction.depth = c(1:4),
                     n.trees=seq(200,10000,200),
                     shrinkage=c(.1, .05, .01,.005,.001))

print("STARTING FIT2")
fit2 <- train(formula, data=df.train,
              method='gbm',
              trControl=fitControl,
              verbose=FALSE,
              tuneGrid=fitGrid)

trellis.par.set(caretTheme())
plot(model)

predictions <- predict(fit2, df.test, type='raw')

write.csv(predicitons, file="GBMForestModel.csv", row.names= FALSE)