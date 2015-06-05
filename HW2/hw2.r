library("doBy")
library("ggplot2")

#read data from test files.  Test files are stored locally
#The general process for loading the data was taken from:
#   https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
#
#The structure for the rest of the code was influenced by a MATLAB project from a previous class
#The project was to try and beat Netflix's recommnedation algorithm
#
#This solution uses the idea that similar items are rated similarly.
#If person x lives and their vector is similar to person y, then it is likely that person y also lived
#We use cosine similarity to determine how similar persons are.
#
#NOTE: with movies, the rankings are generally between 0 and 5, but with life and death its between 0 and 1


#create function to readData in
readData <- function(file.name, column.types, missing.types) {
  read.csv(file.name, colClasses= column.types, na.strings=missing.types)
}

#Cosine Similarity function
cosineSimilarity <- function(passenger.vector, neighbor.vector, columns) {
  #calculate the cosine similarity between the passenger and the neighbor vectors
  # :param passenger.vector:  a vector describing a passenger from the test set
  # :param neighbor.vector:   a vector describing a passenger from the training set
  # :param columns:           the columns we are using to calculate cosine similarity.
  
  
  #only use the columns in columns
  neighbor.vector = neighbor.vector[,c(columns)]
  passenger.vector = passenger.vector[,c(columns)]
  
  #filter out NA values.  They do nothing for us.
  #filter the two vectors down so that each column as a value that is NOT NA
  passenger.filtered = passenger.vector[,!apply(is.na(passenger.vector),2,all)]
  neighbor.filtered = neighbor.vector[,!apply(is.na(neighbor.vector),2,all)]
  
  col_intersect = intersect(colnames(neighbor.filtered), colnames(passenger.filtered))
  
  passenger.filtered = passenger.filtered[,c(col_intersect)]
  neighbor.filtered = neighbor.filtered[,c(col_intersect)]
  
  
  #print(neighbor.filtered)
  #print(ncol(passenger.filtered))
  #print(1/(1+dist(rbind(neighbor.filtered,passenger.filtered))))
  similarity = 0
  
  #calculate cosine similarity
  if (ncol(passenger.filtered) > 1) {
    #print("COSINE")
    similarity = sum(neighbor.filtered*passenger.filtered)/sqrt(sum(neighbor.filtered^2)*sum(passenger.filtered^2))
  }
  else if(ncol(passenger.filtered == 1)){
    #if there is only 1 co-marked item, then cosine similarity will always return 1
    #use Euclidean Distance instead
    #print("EUCLIDEAN")
    similarity = 1/(1+dist(rbind(neighbor.filtered,passenger.filtered)))
      
  }
  else {
    similarity = 0
  }
  return(similarity)
  
}

#create Pearson Corellation Function


#Load data 
##############################################################
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

#force some columns from factor to numeric
df.train$Sex = as.numeric(df.train$Sex)   #1 = female, 2 = male
df.train$Embarked = as.numeric(df.train$Embarked) #1 = C, 2 = Q, 3 = S 

df.test$Sex = as.numeric(df.test$Sex)   #1 = female, 2 = male
df.test$Embarked = as.numeric(df.test$Embarked) #1 = C, 2 = Q, 3 = S 

#pick out which columns we want to use for cosine similarity
#the columns we chose are all ones that can be mapped to a value
#AND seemd to have a good deal of significance on survival
columns = c("Pclass","Sex" ,"Age" , "SibSp" ,"Parch" ,"Fare","Embarked")

##RUN ANALYSIS
###########################################################
test_rows = nrow(df.test)
train_rows = nrow(df.train)

predictions = data.frame(PassengerId = integer(test_rows), Survived=integer(test_rows))

for (p in 1:nrow(df.test)) {
  print(paste("Passenger: ", p))
  similarities = data.frame(Similar=numeric(train_rows), Survived = integer(train_rows))
  
  for (n in 1:nrow(df.train)) {
    similarities$Similar[n] = cosineSimilarity(df.test[p,], df.train[n,], columns)
    similarities$Survived[n] = df.train$Survived[n]
    
  }
  most_similar = subset(similarities, similarities$Similar > .9)
  predictions$Survived[p] = round(mean(most_similar$Similar * most_similar$Survived))
  predictions$PassengerId[p] = df.test$PassengerId[p]
}

#predctions$Survived has values of 1 and 2, with 2 being survived, and 1 being did not survive
#subtract 1 from this column to get the values we want

predictions$Survived = predictions$Survived - 1

write.csv(predictions, file="CosineSimilarityModel.csv", row.names= FALSE)