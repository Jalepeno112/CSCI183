require("caret")
require("doBy")
require("pROC")
#This file splits the training data into test and training subsets
#That way, we can test our model without having to repeadetly upload to Kaggle

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

train.raw <- read.csv("train.csv")
#break the prices down into sub groups of 100 to make handeling it easier
#NOTE:  100 is arbitrarily chosen
#       It looked like a reasonable number based on the data
train.raw$price_category <- cut(train.raw$price_usd, 
                               c(0,100,200,300,400,500,600,700,800,900,
                                 1000,1100,1500,1600,max(train.raw$price_usd)))

sample_size <- floor(0.75 * nrow(train.raw))

train_ind <- sample(seq_len(nrow(train.raw)), size=sample_size)

df.train <- train.raw[train_ind, ]
df.test<- train.raw[-train_ind, ]

#there are 0s in the prop_review_score and starrating columns
#this means that a review or rating has not been given.
#fill these zeros in
########################################################
#star_mean <- mean(df.train$prop_starrating[df.train$prop_starrating != 0])
#reviews_mean <- mean(df.train$prop_review_score[df.train$prop_review_score != 0])
#starReviewMean <- mean(c(star_mean,reviews_mean))

#df.train$prop_review_score <- apply(df.train[,c('prop_review_score','prop_starrating')],
#                                    MARGIN = 1, 
#                                    function(x) reviewsImpute(x['prop_review_score'],
#                                                              x['prop_starrating'],
#                                                              starReviewMean)
#)

#df.train$prop_starrating <- apply(df.train[, c('prop_review_score', 'prop_starrating')],
#                                  MARGIN = 1, 
#                                  function(x) ratingsImpute(x['prop_review_score'], 
#                                                            x['prop_starrating'], 
#                                                            starRatingsMean)
#)

#do the same for the test data
########################################
#star_mean <- mean(df.test$prop_starrating[df.test$prop_starrating != 0])
#reviews_mean <- mean(df.test$prop_review_score[df.test$prop_review_score != 0])
#starReviewMean <- mean(c(star_mean,reviews_mean))

#df.test$prop_review_score <- apply(df.test[,c('prop_review_score','prop_starrating')],
#                                    MARGIN = 1, 
#                                    function(x) reviewsImpute(x['prop_review_score'],
#                                                              x['prop_starrating'],
#                                                              starReviewMean)
#)

#df.test$prop_starrating <- apply(df.test[, c('prop_review_score', 'prop_starrating')],
#                                  MARGIN = 1, 
#                                  function(x) ratingsImpute(x['prop_review_score'], 
#                                                            x['prop_starrating'], 
#                                                            starRatingsMean)
#)



#set the formula that we will use to determine booking_bool

##################################################################
formula <- booking_bool ~ 
  site_id +  #the site used to make the purchased
  prop_country_id + #the country the hotel is in
  (prop_review_score * #the review score (scale of 1-5 in increments of .5)
  prop_starrating) + #the star rating (scale 1-5 increments of 1)
  #price_usd + #the listed price (not including taxes and such)
  #srch_adults_count + #the number of adults
  promotion_flag +
  price_category

#10 fold CV 
fitControl <- trainControl(method='repeatedcv', 
                           number=10, 
                           repeats=1, 
                           verbose=TRUE)

fit1<-train(formula,
            data=df.train,
            method='gbm',
            trControl = fitControl,
            verbose=FALSE
)

predicitons <- predict(fit1, df.test, type="raw")

#format the output
#there are 2 columns:
#   srch_prop-id: the srch id concatanted with the prop_id and separated with a "-"
#   booking_bool: the results from predictions
df.test$predictions <- predicitons

#create a ROC and calaculate AUc for it
ROC1 <- roc(df.test$booking_bool, df.test$predictions)
print(plot(ROC1), col='blue')
AUC <- auc(ROC1)

survival.submission = data.frame(srch_id = paste(df.test$srch_id,df.test$prop_id,sep="-"),
                                 booking_bool = predicitons)

colnames(survival.submission) <- c("srch-prop_id", "booking_bool")

#write to csv
#write.csv(survival.submission,file="Predictions_test.csv",row.names=FALSE)
