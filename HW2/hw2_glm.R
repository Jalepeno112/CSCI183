#Predict who survives using glm()


#fill NA with mean of column
fillMean <- function(col) {
  col[is.na(col)] = mean(col, na.rm=TRUE)
  return(col)
}

#find the optimal threshold for rounding
findRoundingThreshold <- function(survived.glm) {
  #do some checks on which rounding threshold is best
  roundingTest = data.frame(Rounded = roundThreshold(survived.glm,0.5), Category=.5)
  roundingTest = rbind(roundingTest, 
                       data.frame(Rounded=roundThreshold(survived.glm,.6), Category=.6),
                       data.frame(Rounded=roundThreshold(survived.glm,.7), Category=.7),
                       data.frame(Rounded=roundThreshold(survived.glm,.8),Category=.8))
  roundingTest.summary = summaryBy(Rounded ~ Category, data=roundingTest)
  
  m = (roundingTest.summary[1,1] - roundingTest.summary[2,1])/ (roundingTest.summary[1,2] - roundingTest.summary[2,2])
  b = roundingTest.summary[1,2] - (m *roundingTest.summary[1,1])
  x = (.31 - b)/m
  
  return(x)
}

#round all values that are above threshold up, and below threshold down
roundThreshold <- function(col, threshold) {
  for (i in 1:length(col)) {
    if (col[i] >= threshold) {
      col[i] = ceiling(col[i])
    }
    else {
      col[i] = floor(col[i])
    }
  }
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

#Format data
#########################
#get rid of NA values in the testing set
df.test$Age = fillMean(df.test$Age)

#passenger 1044 (row 153) has the only NA fare value
#he belongs to Pclass == 3, so fill it with the average of all other Pclass == 3 passengers
df.test[is.na(df.test$Fare), ]$Fare = mean(subset(df.test, df.test$Pclass == 3)$Fare, na.rm = TRUE)


#Analyze Data
##########################
fit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = df.train, family=binomial)
survived.glm <- predict(fit, df.test, type='response')


#survived.glm cotains the prediciton of whether or not someone survives.
#format it so that we can turn it in
survival.rounded = data.frame(Chance = survived.glm)

#the rounding threshold value was found just using the command line
#findRoundingThreshold() contains the code used to find it.
survival.rounded$Rounded = roundThreshold(survival.rounded$Chance, 0.5766054)


#Plot
#########################3
plot <- ggplot(data=survival.rounded, aes(x=Rounded, fill=factor(Rounded))) +
   geom_histogram(binwidth=1, aes(y=..count../sum(..count..))) +
   scale_fill_discrete("Survived", labels=c("No", "Yes")) + ylab('Frequency') +
   xlab('Data') + ggtitle("Survival Predicitons with Rounding")
print(plot)


survival.submission = data.frame(PassengerId = df.test$PassengerId, Survived=survival.rounded$Rounded)



write.csv(survival.submission, file="RoundedModel.csv", row.names= FALSE)
