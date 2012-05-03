# regression with simple lm
# 5-fold cross validation with folds calculated globally

cluster <- function(data) {
  # constants
  lat_max <- 40.3530;
  lat_min <- 40.3390;
  lat_range <- lat_max - lat_min
  lng_min <- -74.6660;
  lng_max <- -74.6440;
  lng_range <- lng_max - lng_min

  # parameters
  num_clusters <- 200

  count <- 0
  count_cluster <- 0
  predictive_dist <- 0

  clustering_data <- data[, c('lat', 'lng', 'bearing')]
  clusters <- kmeans(clustering_data, num_clusters)
  
  print(clusters$centers)
}

require(MASS)
input_file <- "prepared_data.txt";
data <- read.table(input_file)
cluster(data)