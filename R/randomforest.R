# clustering

cluster <- function(data, num_clusters) {
  # parameters
  num_folds <- 5

  count <- 0
  count_cluster <- 0
  predictive_dist <- c()
  
  folds <- sample(1:num_folds, nrow(data), replace=TRUE)
  for (k in 1:num_folds) {
    # fold indices
    in_fold_index <- which(folds == k)
    out_fold_index <- which(folds != k)

    # fold data
    data_in_fold <- data[in_fold_index,]
    data_out_fold <- data[out_fold_index,]

    # clusters on out of fold data
    clustering_data <- data_out_fold[, c('lat', 'lng', 'bearing')]
    clustering_data$bearing = clustering_data$bearing*bearing_multiplier;
    clusters <- kmeans(clustering_data, num_clusters, 1000, 3)

    for (i in 1:length(in_fold_index)) {
      # find cluster that this in fold point would have been assigned to
      ssd <- (clusters$centers[,1] - data$lat[in_fold_index[i]])^2 + (clusters$centers[,2] - data$lng[in_fold_index[i]])^2 + (clusters$centers[,3] - data$bearing[in_fold_index[i]]*bearing_multiplier)^2

      cluster_assigned = which.min(ssd)

      # among indices in that cluster
      indices_in_cluster <- which(clusters$cluster == cluster_assigned)
      # find mean time to wifi in that cluster
      if (length(indices_in_cluster) > 0) {
    	data_in_cluster <- data_out_fold[indices_in_cluster,]
    	mean_time_to_wifi <- mean(data_in_cluster$time_to_wifi)
    	
    	# difference between the mean value and the actual value
    	count <- count +  1
    	predictive_dist[count] <- abs(data$time_to_wifi[in_fold_index[i]] - mean_time_to_wifi)
      }
    }
  }
  #remove outliers
  predictive_dist <- predictive_dist[which(predictive_dist <= median(predictive_dist) + 1.5*IQR(predictive_dist))]
  
  # return mean L1
  mean(predictive_dist)
}

require(MASS)
input_file <- "prepared_data.txt";
data <- read.table(input_file)

# Bearing will be reduced by this much for kmeans
bearing_multiplier = 1/18000;

num_clusters_iter <- seq(from=10,to=(2*nrow(data)/3),by=10)

min_dist <- 100000000
min_num_clusters <- 0
for (num_clusters in num_clusters_iter) {
  pred_dist <- cluster(data, num_clusters)
  print(num_clusters)
  print(pred_dist)
  if (pred_dist < min_dist) {
    min_dist <- pred_dist
    min_num_clusters <- num_clusters
  }
}
print('Best');
print(min_num_clusters);
print(min_dist);
