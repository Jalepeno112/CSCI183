require("ggplot2")
require("reshape")

#NOTE: all of the pixel values in the training and test set were normalized outside of this script

WHITE_THRESHOLD = .1

#loading data
print("Loading training data")
data.train <- read.csv("train.csv")

#for each image, look at a box around the center of the photo.
#get a ratio of the number of white pixels contained in the box
#the higer the ratio, the smoother the galaxy
top <- seq(868,885)
white_square <- top
for (i in seq(1,16)) {
  next_row <- top + (i * 50)
  white_square <- c(white_square, next_row)
}

#get the ratio
getRatio <- function(image) {
  #get the columns that represent the pixels of the image  
  pixels <- image[2:2501]
  
  #get the pixels in our square region
  white <- image[white_square]

  #divide the total number of white pixels by the number of white pixels in the square
  ratio <- length(white[white>= WHITE_THRESHOLD]) / length(pixels[pixels >= WHITE_THRESHOLD])
  
  return(ratio)
}

getVariance <- function(image) {
  white <- image[white_square]
  
  return(var(unlist(white)))
}

print("Creating features for training data")
data.train$WhiteRatio <- apply(X = data.train,1,function(x){getRatio(x)})
data.train$Variance <- apply(X=data.train,1,function(x){getVariance(x)})
data.train$StandardDev <- apply(X=data.train[,white_square],1,sd)

#build the glm model
print("Building model")
formula_vars = c(c("X10th", "X25th", "X75th", "X90th", "Mean", "Variance", "StandardDev"),
                    grep("V",colnames(data.train),value=TRUE))

#formula <- Prob_Smooth ~ X10th + X25th + X75th + X90th + Mean + Variance +StandardDev 
formula <- reformulate(termlabels=formula_vars, response='Prob_Smooth')
fit <- glm(formula, family = "gaussian", data = data.train)

#clear the training data from memory
print("Clearing train data from memory")
rm(data.train)

#read in data.test
print("Loading test data")
data.test <- read.csv("test.csv")

print("Creating features on test data")
data.test$WhiteRatio <- apply(X= data.test, 1, function(x){getRatio(x)})
data.test$Variance <- apply(X=data.test,1,function(x){getVariance(x)})
data.test$StandardDev <- apply(X=data.test[,white_square],1,sd)


predictions <- predict(fit,data.test,type='response')

data.predictions <- data.frame(Prob_Smooth = predictions, GalaxyID = data.test$Id)

rmse <- function(sim, obj) {
  error <- sim - obj
  return(sqrt(mean(error^2)))
}
#rootMeanError <- rmse(data.predictions$Prob_Smooth, data.test$Prob_Smooth)
#print(rootMeanError)


write.csv(data.predictions, "predictions.csv", row.names=FALSE)

