# boosting

boost <- function(data) {
  # parameters
  num_folds <- 5

  count <- 0
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
    model <- gbm(time_to_wifi ~ lat+lng+bearing+time+wday, data=data_out_fold, distribution="gaussian", cv.folds=5);

    best.iter <- gbm.perf(model, method="cv");

    pred_val <- predict(model, data_in_fold, best.iter)
    predictive_dist <- c(predictive_dist, abs(pred_val - data_in_fold$time_to_wifi))
  }
  #remove outliers
  #predictive_dist <- predictive_dist[which(predictive_dist <= median(predictive_dist) + 1.5*IQR(predictive_dist))]
  
  plot(ecdf(predictive_dist), xlab="Error", ylab="CDF", main="")

  # return mean L1
  return(mean(predictive_dist))
}

require(gbm)
require(MASS)
require(randomForest)
input_file <- "prepared_data.txt";
data <- read.table(input_file)

L1 <- boost(data)
print(L1)

return()

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
