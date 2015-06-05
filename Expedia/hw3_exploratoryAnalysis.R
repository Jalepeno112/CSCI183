require("doBy")
require("ggplot2")
require("caret")

percentBooked <- function(sub, set, x, y) {
  for (i in 1:nrow(sub)) {
    sub$percentBooked[i] <- sub[i,x]/set[set[,y] == sub[i,y],x]
  }
  return(sub)
}

stdErrorMean <- function(x) {
  return(sd(x)/sqrt(length(x)))
}

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


#Load Data
#############################
#readData <- function(file.name, column.types, missing.types) {
#  read.csv(file.name, colClasses= column.types, na.strings=missing.types)
#}

train.raw = read.csv("train.csv")
df.train <- train.raw

#break the prices down into sub groups of 100 to make handeling it easier
#NOTE:  100 is arbitrarily chosen
#       It looked like a reasonable number based on the data
df.train$price_category <- cut(df.train$price, 
                               c(0,100,200,300,400,500,600,700,800,900,
                                 1000,1100,1500,1600,max(df.train$price)))



booked <- subset(df.train, df.train$booking_bool == 1)
grouped.bySite <- aggregate(srch_id ~ site_id, data=df.train, FUN=length)
grouped.bySite$normalized <- grouped.bySite$srch_id/sum(grouped.bySite$srch_id)


#see if the site affects booking rates
##########################################################################
print(ggplot(data=df.train,aes(x=site_id, fill=factor(c(site_id)))) +
        geom_histogram(binwidth=1)+
        ggtitle("Number of searches per Site"))

booked.grouped.bySite <- aggregate(srch_id ~site_id, data= booked, FUN = length)
booked.grouped.bySite <- percentBooked(booked.grouped.bySite, 
                                       grouped.bySite, "srch_id", "site_id")
print(ggplot(data=booked.grouped.bySite, aes(x=site_id, y = percentBooked, 
                                fill= factor(c(site_id)))) + 
  geom_bar(stat='identity') +
  ggtitle("Booking rate per Site"))

error_mean <- stdErrorMean(booked.grouped.bySite$pecentBooked)

#check country
##########################
grouped.byPropCountry <- aggregate(srch_id ~ prop_country_id, data=df.train, FUN=length)
booked.grouped.byPropCountry <- aggregate(srch_id ~ prop_country_id, 
                                          data=booked, FUN=length)
booked.grouped.byPropCountry <- percentBooked(booked.grouped.byPropCountry, 
                                              grouped.byPropCountry, 
                                              "srch_id", "prop_country_id")

#look at how rating affects booking rates
#############################################################
grouped.byRating <- aggregate(srch_id~prop_review_score, data=df.train, FUN=length)
print(ggplot(data=grouped.byRating, 
             aes(x=factor(prop_review_score),
                 fill=factor(prop_review_score),
                 y=srch_id))+
    geom_bar(stat='identity') + 
    ggtitle("Number of Searches per Review Score"))

booked.grouped.byRating <- aggregate(srch_id~prop_review_score, data=booked, FUN=length)
booked.grouped.byRating <- percentBooked(booked.grouped.byRating, grouped.byRating, 
                                         "srch_id","prop_review_score")
print(ggplot(data=booked.grouped.byRating, 
             aes(x=prop_review_score, 
                 y=percentBooked, 
                 fill=factor(prop_review_score))) + 
        geom_bar(stat='identity') +
        ggtitle("Percent Booked based on Review Score"))
error_mean <- stdErrorMean(booked.grouped.byRating$pecentBooked)
print(sprintf("RATING STD ERROR MEAN: %f\n", error_mean))

#look at how star rating affects booking
#####################################################
grouped.byStarRating <- aggregate(srch_id~prop_starrating, data = df.train, FUN=length)
print(ggplot(data=df.train, 
             aes(x=prop_starrating, fill=factor(prop_starrating))) +
        geom_histogram(binwidth=1) + 
        ggtitle("Number of Searches per Star Rating")
        )


booked.grouped.byStarRating <- aggregate(srch_id~prop_starrating,data=booked,FUN=length)
booked.grouped.byStarRating$percentBooked <- booked.grouped.byStarRating$srch_id/grouped.byStarRating$srch_id
print(ggplot(data=booked.grouped.byStarRating,aes(x=prop_starrating, 
                                                  fill=factor(prop_starrating),
                                                  y=percentBooked)) + 
        geom_bar(stat='identity') +
        ggtitle("Booking Rate per Star Rating"))
error_mean <- stdErrorMean(booked.grouped.byStarRating$percentBooked)
print(sprintf("STAR RATING STD ERROR MEAN: %f\n", error_mean))


#look at how the price affects booking
#############################################################
print(ggplot(data=df.train,aes(x=price_category, fill=price_category)) + 
  geom_bar() +
  ggtitle("Number of Searches per Price Group"))

grouped.byPrice <- aggregate(srch_id ~price_category, data=df.train,FUN=length)
booked.grouped.byPrice <- aggregate(srch_id ~ price_category,data=booked,FUN = length)
booked.grouped.byPrice <- percentBooked(booked.grouped.byPrice, grouped.byPrice, 
                                        "srch_id", "price_category")
booked.grouped.byPrice <- aggregate(srch_id~ price_category, data =booked, FUN=length)
booked.grouped.byPrice <- percentBooked(booked.grouped.byPrice,grouped.byPrice,
                                        "srch_id","price_category")
print(ggplot(data=booked.grouped.byPrice,
             aes(x=price_category, fill=price_category, y = percentBooked)) + 
        geom_bar(stat='identity') +
        ggtitle("Booking Rate per Price Group"))
error_mean = stdErrorMean(booked.grouped.byPrice$percentBooked)
print(sprintf("PRICE STD ERROR MEAN: %f\n", error_mean))

#look at how length of stay affects booking rate
########
grouped.byLengthOfStay <- aggregate(srch_id ~ srch_length_of_stay, data=df.train,FUN=length)
print(ggplot(data=df.train, aes(x=srch_length_of_stay, fill=factor(srch_length_of_stay))) +
        geom_histogram(binwidth=1) +
        ggtitle("Number of Searches by Length of Stay"))

booked.grouped.byLengthOfStay <- aggregate(srch_id ~srch_length_of_stay, data=booked, FUN = length)
booked.grouped.byLengthOfStay <- percentBooked(booked.grouped.byLengthOfStay, grouped.byLengthOfStay, "srch_id", "srch_length_of_stay")
print(ggplot(data=booked.grouped.byLengthOfStay,
       aes(x=srch_length_of_stay, fill=factor(srch_length_of_stay), y=percentBooked)) + 
  geom_bar(stat='identity') + ggtitle("Booking Rate per Length Of Stay"))


#promotion_flag
#############################
grouped.byPromotionFlag <- aggregate(srch_id ~ promotion_flag, data=df.train, FUN=length)
print(ggplot(data=df.train, 
             aes(x=promotion_flag, fill=factor(promotion_flag))) +
        geom_histogram(binwidth = 1) +
        ggtitle("Number of Searches by Promotion Flag")
      )

booked.grouped.byPromotionFlag <- aggregate(srch_id ~ promotion_flag, data=booked, FUN=length)
booked.grouped.byPromotionFlag$percentBooked = booked.grouped.byPromotionFlag$srch_id/grouped.byPromotionFlag$srch_id

print(ggplot(data=booked.grouped.byPromotionFlag, 
             aes(x=promotion_flag, fill=factor(promotion_flag), y = percentBooked)) +
        geom_bar(stat='identity')+
        ggtitle("Booking Rate by Promotion Flag"))

#number of adults
##############################
grouped.byAdultCount <- aggregate(srch_id ~ srch_adults_count, data=df.train, FUN = length)
print(ggplot(data=df.train, 
             aes(x=srch_adults_count, fill=factor(srch_adults_count))) +
        geom_histogram(binwidth=1) +
        ggtitle("Number of Searches by Adults Count")
)
booked.grouped.byAdultCount <- aggregate(srch_id~srch_adults_count, data=booked, FUN=length)
booked.grouped.byAdultCount$percentBooked <- booked.grouped.byAdultCount$srch_id/
  grouped.byAdultCount$srch_id

print(ggplot(data=booked.grouped.byAdultCount,
             aes(x=srch_adults_count, 
                 fill=factor(srch_adults_count),
                 y=percentBooked))+
        geom_bar(stat='identity')+
        ggtitle("Booking Rate by Adult Count"))


#by Children
#####################################
grouped.byChildrenCount <- aggregate(srch_id ~ srch_children_count, 
                                     data=df.train,FUN=length)
booked.grouped.byChildrenCount <- aggregate(srch_id ~srch_children_count, 
                                            data=booked,FUN = length)
booked.grouped.byChildrenCount <- percentBooked(booked.grouped.byChildrenCount, 
                                                grouped.byChildrenCount, 
                                                "srch_id", 
                                                "srch_children_count")

print(ggplot(data=booked.grouped.byChildrenCount,
             aes(x=srch_children_count, 
                 fill=factor(srch_children_count),
                 y=percentBooked))+
        geom_bar(stat='identity')+
        ggtitle("Booking Rate by Children Count"))

