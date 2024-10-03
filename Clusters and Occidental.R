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
library(ggplot2)
library(ggpubr)
library(readxl)
library(openxlsx)


#----------
raw_data <- read_csv('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Group Project/WHR2023.csv')

happiness_data <- raw_data[apply(raw_data, MARGIN = 1, FUN = function(rows) all(rows!=0)),] %>% 
  drop_na() %>% select(`Country name`, `Logged GDP per capita` : `Perceptions of corruption`) %>%
  rename(country = `Country name`, gdp = `Logged GDP per capita`, social_supp = `Social support`,
         life_exp = `Healthy life expectancy`, freedom = `Freedom to make life choices`, generosity = Generosity,
         corruption = `Perceptions of corruption`)

rownames(happiness_data) <- happiness_data$country

ds <- dist(scale(happiness_data[, -1]))

# Hierarchical Clustering

agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) }

h_average <- hclust(ds, method = "average")
h_average$labels <- happiness_data$country

fviz_dend(h_average, k = 4, cex = 0.5, k_colors = c("#E7B800", "#2E9FDF", "#FC4E07", "#008080"),
          color_labels_by_k = T, rect = T, show_labels = T, main = "Hierarchical Cluster with Average Linkage")

h_complete <- hclust(ds, method="complete")
h_complete$labels <- happiness_data$country

fviz_dend(h_complete, k = 4, cex = 0.5, k_colors = c("#E7B800", "#2E9FDF", "#FC4E07", "#008080"),
          color_labels_by_k = T, rect = T, show_labels = T, main = "Hierarchical Cluster with Complete Linkage")

h_ward <- hclust(ds, method="ward.D2")
h_ward$labels <- happiness_data$country

fviz_dend(h_ward, k = 4, cex = 0.5, k_colors = c("#E7B800", "#2E9FDF", "#FC4E07", "#008080"),
          color_labels_by_k = T, rect = T, show_labels = T, main = "Hierarchical  Cluster with Ward Linkage")

average <- cutree(h_average, k = 4)
complete <- cutree(h_complete, k = 4)
ward <- cutree(h_ward, k = 4)

table(average)
table(complete)
table(ward)

agglo(h_ward)

hwardcluster <- cutree(h_ward, k = 4)
hwardcluster
plot(happiness_data, col = hwardcluster, main = "Ward Linkage")


happiness_data.stand <- scale(happiness_data[,-1]) 
groups <- cutree(h_ward, k=4)

heatmap(happiness_data.stand)


happiness_data.touse <- happiness_data[,-1]

means <- aggregate(happiness_data.touse, list(hwardcluster), mean)
means 

meansz <- aggregate(scale(happiness_data.touse), list(hwardcluster), mean)
meansz


happiness_data$ISO3 <- countrycode(happiness_data$country, 
                                   origin = "country.name", 
                                   destination = "iso3c")

happiness_data$cluster <- hwardcluster

country_map <- joinCountryData2Map(happiness_data,
                                   joinCode = "ISO3",
                                   nameJoinColumn = "ISO3")

map1 <- mapCountryData(country_map, nameColumnToPlot = "cluster",
                       catMethod = "categorical",
                       mapRegion = "world",
                       missingCountryCol = gray(.9),
                       colourPalette = c("#2E9FDF", "#008080", "#E7B800", "#FC4E07"),
                       mapTitle = "World Map with Hierarchical Cluster",
                       borderCol = "white")

subset_cluster_1 <- happiness_data[happiness_data$cluster == 1, ]
subset_cluster_1
subset_cluster_2 <- happiness_data[happiness_data$cluster == 2, ]
subset_cluster_2
subset_cluster_3 <- happiness_data[happiness_data$cluster == 3, ]
subset_cluster_3
#-----------


# Dataset
raw_data <- read_csv('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Group Project/WHR2023.csv')

happiness_data <- raw_data[apply(raw_data, MARGIN = 1, FUN = function(rows) all(rows!=0)),] %>% 
  drop_na() %>% select(`Country name`, `Ladder score`, `Logged GDP per capita` : `Perceptions of corruption`) %>%
  rename(country = `Country name`, happiness = `Ladder score`, gdp = `Logged GDP per capita`, social_supp = `Social support`,
         life_exp = `Healthy life expectancy`, freedom = `Freedom to make life choices`, generosity = Generosity,
         corruption = `Perceptions of corruption`)

rownames(happiness_data) <- happiness_data$country

ds <- dist(scale(happiness_data[, -1]))

rownames(happiness_data) <- happiness_data$country

happiness_data$cluster <- hwardcluster

write.xlsx(happiness_data, "happiness_data.xlsx")

happiness_data <- read.xlsx('/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Group Project/happiness_data.xlsx')

cluster_data <- read.xlsx("/Users/tommipremoli8/Desktop/Data Science for Economics/Materie/2nd Semester/Machine Learning e Statistical Learning/Statistical Learning/Group Project/cluster.xlsx", sheet = 1)
happiness_data <- cbind(happiness_data, cluster_data)


# Linear regression with clusters
happiness_data$cluster <- as.factor(happiness_data$cluster)
happiness_data$occidental <- as.factor(happiness_data$occidental)

# clusters
summary(lm(happiness ~ gdp + social_supp + freedom + cluster, data = happiness_data))

# stepwise regression
summary(lm(happiness ~ gdp + social_supp + life_exp + freedom + generosity + corruption + cluster, data = happiness_data))
summary(lm(happiness ~ gdp + social_supp + life_exp + freedom + corruption + cluster, data = happiness_data))
summary(lm(happiness ~ gdp + social_supp + life_exp + freedom + cluster, data = happiness_data))




summary(lm(happiness ~ social_supp + freedom + generosity + cluster, data = happiness_data))



