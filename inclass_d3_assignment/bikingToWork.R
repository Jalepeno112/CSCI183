require("choroplethr")
require("acs")

bike_data <- get_acs_data("B08101", "county", column_idx = 41)
bike_data.df <- bike_data$df

totalTransportData <- get_acs_data("B08101","county", column_idx = 1)
totalTransport.df <- totalTransportData$df

#our D3 file expects the columns to be title id and rate
#they are currently called region and value
colnames(bike_data.df) <- c("id","rate")

#divide the total number of people using bikes by the total number of people who uses transportation
bike_data.df$rate <- bike_data.df$rate/totalTransport.df$value

#write to tsv file
write.table(data.df, file="bikeToWork.tsv", sep="\t",row.names=FALSE)

