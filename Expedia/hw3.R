require("caret")
require("doBy")

#Load Data
#############################
#readData <- function(file.name, column.types, missing.types) {
#  read.csv(file.name, colClasses= column.types, na.strings=missing.types)
#}
reviewsImpute <- function(reviews,stars, starRatingsMean) {
  #x:           row from a dataframe
  #review_mean: the mean of all non-zero review entries
  #star_mean:   the mean of all non-zero starrating entries
  if(stars == 0 & reviews ==0) {
    return (starRatingsMean)
  }
  else if(reviews==0) {
    return (stars)
  }
  else{
    return (reviews)
  }
}

ratingsImpute <-function(reviews, stars, starRatingsMean) {
  #x:           row from a dataframe
  #review_mean: the mean of all non-zero review entries
  #star_mean:   the mean of all non-zero starrating entries
  if(reviews == 0 & stars ==0) {
    return (starRatingsMean)
  }
  else if(stars ==0) {
    return (reviews)
  }
  else{
    return (stars)
  }
}


#set the seed so that these results can be recreated
set.seed(117)

train.raw = read.csv("train.csv")
test.raw = read.csv("test.csv")

df.train <- train.raw
df.test <- test.raw

#break the prices down into sub groups of 100 to make handeling it easier
#NOTE:  100 is arbitrarily chosen
#       It looked like a reasonable number based on the data
df.train$price_category <- cut(df.train$price, 
                               c(0,100,200,300,400,500,600,700,800,900,
                                 1000,1100,1500,1600,max(df.train$price)))

#set the formula that we will use to determine booking_bool
formula <- booking_bool ~ 
  site_id +  #the site used to make the purchased
  prop_country_id + #the country the hotel is in
  (prop_review_score * #the review score (scale of 1-5 in increments of .5)
  prop_starrating) + #the star rating (scale 1-5 increments of 1)
  price_usd + #the listed price (not including taxes and such)
  srch_adults_count + #the number of adults
  promotion_flag+
  srch_children_count

fitGrid<-expand.grid(interaction.depth = c(1:4),
                     n.trees=150,
                     shrinkage=c(.1, .05, .01))

fitControl <- trainControl(method='repeatedcv', 
                           number=10, 
                           repeats=1, 
                           verbose=TRUE)

fit1<-train(formula,
            data=df.train,
            method='gbm',
            trControl = fitControl,
            tuneGrid = fitGrid,
            verbose=FALSE
            )

predicitons <- predict(fit1, df.test, type="raw")

#format the output
#there are 2 columns:
#   srch_prop-id: the srch id concatanted with the prop_id and separated with a "-"
#   booking_bool: the results from predictions

survival.submission <- data.frame(srch_id = paste(df.test$srch_id,df.test$prop_id,sep="-"),
                    booking_bool = predicitons)
colnames(survival.submission) <- c("srch-prop_id", "booking_bool")

#write to csv
write.csv(survival.submission,file="Predictions.csv",row.names=FALSE)

