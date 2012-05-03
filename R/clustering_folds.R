# regression with simple lm
# 5-fold cross validation with folds calculated globally

cluster <- function(data) {
  # parameters
  num_clusters <- 100
  num_folds <- 10

  count <- 0
  count_cluster <- 0
  predictive_dist <- 0
  
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
    clusters <- kmeans(clustering_data, num_clusters)

    for (i in 1:length(in_fold_index)) {
      # find cluster that this in fold point would have been assigned to
      ssd <- (clusters$centers[,1] - data$lat[in_fold_index[i]])^2 + (clusters$centers[,2] - data$lng[in_fold_index[i]])^2 + (clusters$centers[,3] - data$bearing[in_fold_index[i]])^2

      cluster_assigned = which.min(ssd)

      # among indices in that cluster
      indices_in_cluster <- which(clusters$cluster == cluster_assigned)
      # find mean time to wifi in that cluster
      if (length(indices_in_cluster) > 0) {
	data_in_cluster <- data_out_fold[indices_in_cluster,]
	mean_time_to_wifi <- mean(data_in_cluster$time_to_wifi)
	
	# difference between the mean value and the actual value
	predictive_dist <- predictive_dist + abs(data$time_to_wifi[in_fold_index[i]] - mean_time_to_wifi)
	count <- count +  1
      }
    }
  }
  print(predictive_dist/count)
  print(count)
}

require(MASS)
input_file <- "prepared_data.txt";
data <- read.table(input_file)
cluster(data)