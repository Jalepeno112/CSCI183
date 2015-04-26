require("ggplot2")

#load data
music_all <- read.csv("music-all.csv")

#impute nas with 0
music_all[is.na(music_all)] <- 0

#use only numeric columns so that we can use prcomp
music_numeric <- music_all[sapply(music_all,is.numeric)]

#perform prcomp with retx=TRUE and scale. = TRUE
#prcomp Principal Component Analysis (PCA) is a multivariate technique 
#that allows us to summarize the systematic patterns of variations in the data.
music_prcomp <- prcomp(music_numeric,retx=TRUE, scale. =TRUE)

#plot the standard deviation
#i.e., the square roots of the eigenvalues
plot(music_prcomp$sdev)

#music_prcomp$x is the scores matrix
#there are as many columns in the scores matrix 
#as there are rows in the original dataframe
rotated_data <- data.frame(music_prcomp$x)

#create a new column with the artist names
rotated_data$artist <- music_all$artist

#plot PC2 vs PC1 and look at the groups.
#the Principle Components are used as coefficients to plot in 2d space
#each row of the PC column is the coffecient to a variable in music_all
#Use PC1 for the x-axis, and PC2 for the y-axis
#Use artist names for labels
print(ggplot(data=rotated_data, 
             aes(x=PC1, y = PC2)) +
        geom_text(label=rotated_data$artist) +
        ggtitle("PC2 vs PC1"))