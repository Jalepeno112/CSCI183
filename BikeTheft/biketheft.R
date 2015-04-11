data =read.csv(file="Bike Theft Log (From Sept07).csv")

getBuildings <- function(data, buildings) {
  data$LOCATION_DORM = "Other"
  for (i in 1:length(buildings)) {
    data$LOCATION_DORM[grepl(buildings[i], data$LOCATION)] <- buildings[i]
  }
  return(data)
}

#list of buildings we want to look at
buildings = c("Swig", "Benson", "Learning Common", 
              "Casa", "Dunne", "Graham","McLaughlin", 
              "Nobili", "Sanfilipo", "Villas", "Walsh",
              "Sobrato", "O'Connor", "Eng", "Daly")

#apply map the location to buildings
data <- getBuildings(data,buildings)

#get the number of each thefts at each location
grouped.byDorm = aggregate(CASE ~ LOCATION_DORM, data=data, length)

#there are a lot of other buildings in the OTHER category
#plot evetything except it
print(ggplot(data=subset(grouped.byDorm,grouped.byDorm$LOCATION_DORM != "Other"), 
             aes(x=LOCATION_DORM, y =CASE, fill=LOCATION_DORM)) + 
        geom_bar(stat='identity') + 
        ylab("Thefts") + 
        ggtitle('Thefts by Building'))


#format the dates
data$DATE_FORM <- as.character(data$DATE)
data$DATE_FORM[2] <- "12/07/2007"
data$DATE_FORM[25] <- "03/14/2008"
data$DATE_FORM[67] <- "9/23/2009"
data$DATE_FORM[165] <- "11/15/2011"

#convert them to factor
data$DATE_FACTOR <- as.Date(data$DATE_FORM, format="%m/%d/%Y")

#group them
grouped.byDate <- aggregate(CASE~DATE_FACTOR, data=data, length)

#group by weekday
data$WEEKDAY <-  weekdays(as.Date(data$DATE_FORM,format="%m/%d/%Y"))
grouped.byWeekday <- aggregate(CASE ~ WEEKDAY, data=data, length)

#plot
print(ggplot(data=grouped.byWeekday, aes(x=WEEKDAY, y = CASE, fill=WEEKDAY)) + 
        geom_bar(stat="identity") + 
        ggtitle("Thefts by Weekday") + 
        ylab("Thefts") +
        scale_fill_brewer(palette="Set1"))


#group by month
data$MONTH <- months(as.Date(data$DATE_FORM,format="%m/%d/%Y"))
grouped.byMonth <- aggregate(CASE ~ MONTH, data=data, length)

#plot thefts by month
print(ggplot(data=grouped.byMonth, aes(x=MONTH, y = CASE, fill=MONTH)) + 
        geom_bar(stat="identity") + ylab("Thefts") + ggtitle("Thefts by Month"))

