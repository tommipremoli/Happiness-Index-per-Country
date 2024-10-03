# Silhouette
calculate_silhouette_values <- function(data, num_clusters) {
  set.seed(123)
  kmeans_fit <- kmeans(data, num_clusters)

  silhouette_values <- silhouette(kmeans_fit$cluster, dist(data))
  
  return(silhouette_values)
}

silhouette_values_3 <- calculate_silhouette_values(happiness_data.stand, 3)
silhouette_values_4 <- calculate_silhouette_values(happiness_data.stand, 4)
silhouette_values_5 <- calculate_silhouette_values(happiness_data.stand, 5)

print(silhouette_values_3)
print(silhouette_values_4)
print(silhouette_values_5)

plot(silhouette_values_3)
plot(silhouette_values_4)
plot(silhouette_values_5)