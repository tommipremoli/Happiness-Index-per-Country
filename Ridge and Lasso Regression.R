library(glmnet)
library(dplyr)
library(tidyverse)
library(readr)
library(ggcorrplot)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(formattable)
library(cluster)
library(countrycode)
library(rnaturalearth)
library(rworldmap)

library(ggpubr)
library(readxl)
library(openxlsx)



# Lasso regression
set.seed(123)
happiness_data <- read.xlsx('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Group Project/happiness_data.xlsx')

y <- happiness_data$happiness
x <- data.matrix(happiness_data[, c('gdp', 'social_supp', 'life_exp', 'freedom', 'generosity', 'corruption', 'cluster')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)
summary(cv_model)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_modell <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# Plot dei coefficienti della Ridge Regression
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)


# Ridge Regression
set.seed(123)
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)
summary(cv_model)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_modelr <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)

pred_lasso <- predict(best_modell, newx = x, s = best_lambda)
mse_lasso <- mean((pred_lasso - y)^2)
mse_lasso

pred_ridge <- predict(best_modelr, newx = x, s = best_lambda)
mse_ridge <- mean((pred_ridge - y)^2)
mse_ridge
