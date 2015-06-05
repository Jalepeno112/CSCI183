require("ggplot2")
require("glmnet")
#load data
soil <- read.csv("soil.csv")

#use a matrix instead of a dataframe
soil_matrix <- data.matrix(soil)

#glmnet: fit a generalized liner model via penalized maximum likelihood
#fit glment model with pH as the target variable
soil_fit = glmnet(x=soil_matrix[,1:3578], #everything except the pH column
                  y=soil_matrix[,3579], #pH column
                  family="gaussian")

#plot the coffecticents of soil_fit
plot(soil_fit)

#find best lambda paramter and plot coefficients
soil_fit_cv = cv.glmnet(x=soil_matrix[,1:3578],
                        y = soil_matrix[,3579],
                        family="gaussian")

#the smaller the lambda the better
lambda_min = soil_fit_cv$lambda.min
print(lambda_min)

plot(coef(soil_fit_cv),s="lambda.min")

#get the ten coefficents with the smallest lambda value
best_coef <- coef(soil_fit_cv, s="lambda.min")
best_coef <- abs(best_coef)

best_names <-rownames(best_coef)[order(best_coef, decreasing=TRUE)][1:10]

print(best_names)
