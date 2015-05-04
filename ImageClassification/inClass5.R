require("jpeg")
require("EBImage")
require("ripa")


#get the ids of all the images and place them in a column of a dataframe
greyImages <- list.files("images/greyscaled",
                         pattern="*.jpg",full.names=TRUE)
imageIds <- unlist(lapply(greyImages, 
                   function(x) as.numeric(strsplit(
                     strsplit(x,"/")[[1]][3],".jpg")[[1]][1]
                   )))
data.pre <- data.frame(Id = imageIds)

#get the 10,25,75,90th percentiles
getPercentiles <- function(img) {
  i <- readImage(img)
  return (quantile(imageData(i),c(.10,.25,.75,.90)))
}

print("Calculating percentiles")
percentiles <- lapply(greyImages,function(x) {getPercentiles(x)})
percentiles.frame <- do.call(rbind.data.frame, percentiles)
colnames(percentiles.frame) <- c('10th','25th','75th','90th')
data.pre <- cbind(data.pre,percentiles.frame)

print("Calculating mean")
means <- lapply(greyImages, function(x){
  i <- readImage(x)
  return (mean(imageData(i)))
})
data.pre$Mean <- unlist(means)

#variances <- sapply(greyImages, function(x) {
#  i <- readImage(x)
#  return (var(unlist(imageData(i))))
#})


#get the known probablity for those images
#there are 60,000 images in the images folder
#converting all of them to greyscale is taking fiveever
#instead, check the galaxy_train data, if we greyscaled an image whose ID is the training set, great
#if that image is not in the training set, put it in the test set
#Hopefully, we will have enough in each set
print('Breaking images into test and training sets')
galaxy_train <- read.csv("galaxy_train.csv")
matches <- data.pre$Id %in% galaxy_train$GalaxyID
data.train <- subset(data.pre,matches)

#get the prob_smooth values from the training data
data.train$Prob_Smooth <- unlist(lapply(data.train$Id, 
                                        function(x){galaxy_train$Prob_Smooth[galaxy_train$GalaxyID == x]})
                      )

#get test set.  These are all of the Image Ids that do not exist in the galaxy_train dataframe
data.test <- subset(data.pre, !matches)
  
#do a quick glm fit
print("Creating GLM fit")
fit <- glm(Prob_Smooth ~ ., family = "gaussian", data = data.train)
predictions <- predict(fit,data.test,type='response')

data.predictions <- data.frame(Prob_Smooth = predictions, GalaxyID = data.test$Id)

#calculate root mean square error
print("Calculating root mean squre error")
rmse <- function(sim, obj) {
  error <- sim - obj
  return(sqrt(mean(error^2)))
}
rootMeanError <- rmse(data.predictions$Prob_Smooth, data.train$Prob_Smooth)
print(rootMeanError)


print("Writing data to files")
#write data out to csv file for later use
write.csv(data.predictions, "predictions.csv")
write.csv(data.train, "train.csv")
write.csv(data.test, "test.csv")
