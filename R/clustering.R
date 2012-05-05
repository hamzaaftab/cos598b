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
  num_clusters <- 400
  # Bearing will be reduced by this much for kmeans
  bearing_multiplier = 1/18000;

  count <- 0
  count_cluster <- 0
  predictive_dist <- 0

  clustering_data <- data[, c('lat', 'lng', 'bearing')]
  clustering_data$bearing = bearing_multiplier * clustering_data$bearing;
  clusters <- kmeans(clustering_data, num_clusters, 10000, 100)
  
  mean_time_to_wifi <- matrix(NA, num_clusters, 1)

  for (i in 1:num_clusters) {
    # among indices in that cluster
    indices_in_cluster <- which(clusters$cluster == i)
    # find mean time to wifi in that cluster
    if (length(indices_in_cluster) > 0) {
      data_in_cluster <- data[indices_in_cluster,]
      mean_time_to_wifi[i] <- mean(data_in_cluster$time_to_wifi)
    }
  }

  output<-data.frame(clusters$centers, mean_time_to_wifi)
  
  #Unnormalize bearing
  output$bearing <- output$bearing/bearing_multiplier
  
  output
}

require(MASS)
input_file <- "prepared_data.txt";
data <- read.table(input_file)
model <- cluster(data)
write.table(model,"kmeans_model.txt", row.names=FALSE,col.names=FALSE)