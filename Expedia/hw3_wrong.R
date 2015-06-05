require("doBy")
require("ggplot2")


percentBooked <- function(sub, set, x, y) {
  for (i in 1:nrow(sub)) {
    sub$percentBooked[i] <- sub[i,x]/set[set[,y] == sub[i,y],x]
  }
  return(sub)
}

stdErrorMean <- function(x) {
  return(sd(x)/sqrt(length(x)))
}

#Load Data
#############################
#readData <- function(file.name, column.types, missing.types) {
#  read.csv(file.name, colClasses= column.types, na.strings=missing.types)
#}

#train = read.csv("train.csv")
df.train <- train

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
print(ggplot(data=df.train,aes(x=site_id, fill=factor(site_id))) +
        geom_histogram(binwidth=1)+
        ggtitle("Number of searches per Site"))

booked.grouped.bySite <- percentBooked(booked.grouped.bySite, grouped.bySite, "srch_id", "site_id")
print(ggplot(data=booked.grouped.bySite, aes(x=site_id, y = percentBooked, 
                                fill= factor((site_id)))) + 
  geom_bar(stat='identity') +
  ggtitle("Booking rate per Site"))




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
booked.grouped.byRating <- percentBooked(booked.grouped.byRating, grouped.byRating, "srch_id","prop_review_score")
print(ggplot(data=booked.grouped.byRating, 
             aes(x=prop_review_score, 
                 y=percentBooked, 
                 fill=factor(prop_review_score))) + 
        geom_bar(stat='identity') +
        ggtitle("Percent Booked based on Review Score"))


#look at how star rating affects booking
#####################################################
grouped.byStarRating <- aggregate(srch_id~prop_starrating, data = df.train, FUN=length)
print(ggplot(data=df.train, 
             aes(x=prop_starrating, fill=factor(prop_starrating))) +
        geom_histogram(binwidth=1) + 
        ggtitle("Number of Searches per Star Rating")
        )


booked.grouped.byStarRating <- aggregate(srch_id~prop_starrating,data=booked,FUN=length)
booked.grouped.byStarRating <-percentBooked(booked.grouped.byStarRating, grouped.byStarRating,
                                            "srch_id","prop_starrating")
print(ggplot(data=booked.grouped.byStarRating,aes(x=prop_starrating, 
                                                  fill=factor(prop_starrating),
                                                  y=percentBooked)) + 
        geom_bar(stat='identity') +
        ggtitle("Booking Rate per Star Rating"))



#look at how the price affects booking
#############################################################
print(ggplot(data=df.train,aes(x=price_category, fill=price_category)) + 
  geom_bar() +
  ggtitle("Number of Searches per Price Group"))

grouped.byPrice <- aggregate(srch_id ~price_category, data=df.train,FUN=length)
booked.grouped.byPrice <- aggregate(srch_id ~ price_category,data=booked,FUN = length)
booked.grouped.byPrice <- percentBooked(booked.grouped.byPrice, grouped.byPrice, 
                                        "srch_id", "price_category")
print(ggplot(data=booked,aes(x=price_category, fill=price_category)) + 
        geom_bar() +
        ggtitle("Number of Bookings per Price Group"))

