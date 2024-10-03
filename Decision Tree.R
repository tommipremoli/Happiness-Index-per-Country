# Decisional Tree

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
library("rpart")
library("rpart.plot")
library(party)
library("partykit")

set.seed(101)
train.idx <- sample(nrow(happiness_data), 2/3 * nrow(happiness_data)) 
happiness_data.train <- happiness_data[train.idx, ]
happiness_data.test <- happiness_data[-train.idx, ]

ptree<-ctree(happiness ~ social_supp + freedom + generosity + corruption, data=happiness_data.train, control = ctree_control(mincriterion=0.01, minsplit=0, minbucket=0))
plot(ptree)

pptree <- ctree(happiness ~ social_supp + freedom + generosity + corruption, data=happiness_data.train, control = ctree_control(mincriterion=0.9, minsplit=0, minbucket=0))
plot(pptree)

rtree<-rpart(happiness ~ social_supp + freedom + generosity + corruption, data=happiness_data.train, cp=0.0001)
printcp(rtree)

rpart.plot(rtree, digits = 2, fallen.leaves = TRUE, cex = 0.9)
