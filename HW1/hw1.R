#NAME:        Giovanni Briggs
#DATE:        4/6/15
#CLASS:       CSCI183
#ASSIGNMENT:  HW1   

#import libraries
require("ggplot2")
require("doBy")
require("reshape")

#timeaxis.  Series of dates form April 1st, 2012 to April 31st, 2012
x_axis = seq(as.Date("2012/5/1"), as.Date("2012/5/31"), by='day')

merged_ctr = data.frame()
merged_cat = data.frame()

for (i in 1:length(x_axis)) {
  #build url to fetch data and read in data
  fetch <- paste("http://stat.columbia.edu/~rachel/datasets/nyt",toString(i),".csv",sep="")
  data <- read.csv(url(fetch))
  
  #add an age_group to all users
  data$age_group <- cut(data$Age, c(-Inf, 0, 18,24,34,54,64,Inf)) 
  
  #only get the set of users that are signed in
  data_signedIn <- subset(data, data$Signed_In==1)
  
  #calculate click through rate of signed in users
  data_signedIn$CTR <- data_signedIn$Clicks/data_signedIn$Impressions
  
  #describe the user behavior.  They either Clicked, saw an add and DidNotClick, or NoAds were displayed
  data_signedIn$category[data_signedIn$Impressions == 0] <- "NoAdd"
  data_signedIn$category[data_signedIn$Impressions > 0] <- "DidNotClick"
  data_signedIn$category[data_signedIn$Clicks > 0] <- "Clicked"

  #get the number of people in each category grouped by age_group and Gender
  len = function(x){length(x)}
  cat_summary <- summaryBy(category ~ age_group + Gender + category, 
                           data= data_signedIn, FUN = len)

  #get the average CTR per age group per gender
  #have to take the subset of the data where Impressions>0, or else NAN is returned
  ctr_mean <- summaryBy(CTR~Gender+age_group,data=subset(data_signedIn, Impressions>0))
  
  #attach the corresponding timestamp to this data
  ctr_mean$timestamp <-  factor(x_axis[i])
  
  #factor the Gender column so it is treated as either 1 or 0 instead of a continous set
  ctr_mean$Gender <- factor(ctr_mean$Gender)
  
  #melt the data down
  molten <- melt(ctr_mean)
  
  #add this days data to the global dataframe
  merged_ctr <- rbind(merged_ctr,molten)
  merged_cat <- rbind(merged_cat, cat_summary)
  
  print(fetch)
}

#merged_ctr will now have all data for signed in users
#create one plot for all male data for each age group, and one for all female data
male_ctr_plot = ggplot(data=subset(merged_ctr,merged_ctr$Gender ==0),
             aes(x=as.Date(timestamp), y= value, fill=age_group)) + geom_bar(position = "dodge", stat="identity") + ggtitle("Average Click Through Rate of Males across all Age Groups")

female_ctr_plot = ggplot(data=subset(merged_ctr,merged_ctr$Gender ==1),
                         aes(x=as.Date(timestamp), y= value, fill=age_group)) + geom_bar(position = "dodge", stat="identity") + ggtitle("Average Click Through Rate of Females across all Age Groups")

#get the mean ctr of each group by age_group and gender
ag_mean = aggregate(value ~ Gender + age_group,  merged_ctr, function(x) c(mean = mean(x)))
ag_mean_plot = ggplot(data=ag_mean, 
                      aes(x = age_group, y = value, fill=factor(Gender, labels=c("Male","Female")))) + geom_bar(stat="identity", position="dodge") +  guides(fill=guide_legend(title="Gender"))+ ggtitle("Average CTR per Age Group")

#save plots
ggsave("~/CSCI 183/HW1/male_ctr.png",male_ctr_plot)
ggsave("~/CSCI 183/HW1/female_ctr.png",female_ctr_plot)
ggsave("~/CSCI 183/HW1/ag__signed_mean.png",ag_mean_plot)

#examine the breakdown of user categories over time based on gender and age_group
#display the breakdown of users in each category by age
sumCat = summaryBy(category.len ~ age_group + category, data = merged_cat, FUN = sum)
catAgePlot <- ggplot(data=sumCat, aes(x=category, y=category.len.sum/sum(category.len.sum), fill = age_group)) + geom_histogram(position="dodge", stat="identity") + ylab("Frequency") + xlab("Category") +ggtitle("Frequency By Category and Age Group")

#display the breakdown of users in each category by age and gender
catAgeGenderSum <- summaryBy(category.len ~ Gender + age_group + category, data = merged_cat, FUN = sum)
catAgeGenderPlot <- ggplot(data=catAgeGenderSum, aes(x=category, y=category.len.sum/sum(category.len.sum), fill = age_group, linetype=factor(Gender), color=factor(Gender))) + geom_histogram(position="dodge", stat="identity") + ylab("Frequency") + xlab("Category") +ggtitle("Frequency By Category, Age Group and Gender")

#display the breakdown of users in each category by gender alone
catGenderSum <- summaryBy(category.len ~ Gender + category, data = merged_cat, FUN = sum)
catGenderPlot <- ggplot(data=catGenderSum, aes(x=category, y=category.len.sum/sum(category.len.sum), fill = factor(Gender))) + geom_histogram(position="dodge", stat="identity") + ylab("Frequency") + xlab("Category") +ggtitle("Frequency By Category and Gender")

#save plots
ggsave("~/CSCI 183/HW1/catAge.png",catAgePlot)
ggsave("~/CSCI 183/HW1/catAgeGender.png",catAgeGenderPlot)
ggsave("~/CSCI 183/HW1/catGender.png",catGenderPlot)

#do analysis on users who clicked only
clickedSum <- summaryBy(category.len ~ age_group + Gender, data = clicked_users, FUN = sum)
clickedUserPlot <- ggplot(data=clickedSum, aes(x=age_group, y = category.len.sum/sum(category.len.sum), fill=factor(Gender))) + geom_histogram(position="dodge", stat="identity") + ggtitle("Frequency of Users who Click by Age Group and Gender") + ylab("Frequency")

#save plot
ggsave("~/CSCI 183/HW1/clickedUsers.png",clickedUserPlot)