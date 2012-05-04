# regression with simple lm
# 5-fold cross validation with folds calculated globally

regress_lm <- function(data, lat_div, lng_div, bearing_div) {
  # constants
    lat_max <- 40.3530;
    lat_min <- 40.3390;
    lng_min <- -74.6660;
    lng_max <- -74.6440;
  lat_range <- lat_max - lat_min
  lng_range <- lng_max - lng_min

  # parameters
  num_folds <- 5

  # prediction distance
  pred_dist <- 0

  count<-0

  folds <- sample(1:num_folds, nrow(data), replace=TRUE)
  for (k in 1:num_folds) {
    # fold indices
    in_fold_index <- which(folds == k)
    out_fold_index <- which(folds != k)

    # fold data
    data_in_fold <- data[in_fold_index,]
    data_out_fold <- data[out_fold_index,]

    # loop over all bearing divisions
    for (x in 1: bearing_div) {
      # bearing range in this division
      bearing_min_div <- (360/bearing_div) * (x-1)
      bearing_max_div <- (360/bearing_div) * x
      # loop over all latitude divisions
      for (y in 1: lat_div) {
	# latitude range in this division
	lat_min_div <- lat_min + ((lat_range/lat_div) * (y-1))
	lat_max_div <- lat_min + ((lat_range/lat_div) * y)
	# loop over all longitude divisions
	for (z in 1: lng_div) {
	  # longitude range in this division
	  lng_min_div <- lng_min + ((lng_range/lng_div) * (z-1))
	  lng_max_div <- lng_min + ((lng_range/lng_div) * z)

	  # out of fold data in this lat/lng/bearing range
	  indices_div_out_fold <- rownames(data_out_fold)[which(data_out_fold$bearing >= bearing_min_div &
						   data_out_fold$bearing <  bearing_max_div &
						   data_out_fold$lat     >= lat_min_div     &
						   data_out_fold$lat     <  lat_max_div     &
						   data_out_fold$lng     >= lng_min_div     &
						   data_out_fold$lng     <  lng_max_div, arr.ind=TRUE)]
	  
	  # in fold data in this lat/lng/bearing range
	  indices_div_in_fold <- rownames(data_in_fold)[which(data_in_fold$bearing >= bearing_min_div &
						       data_in_fold$bearing <  bearing_max_div &
						       data_in_fold$lat     >= lat_min_div     &
						       data_in_fold$lat     <  lat_max_div     &
						       data_in_fold$lng     >= lng_min_div     &
						       data_in_fold$lng     <  lng_max_div, arr.ind=TRUE)]

	  if (length(indices_div_out_fold) > 0) {
	    # train model on out of fold data in this lat/lng/bearing division
	    model <- lm(time_to_wifi ~ speed+time+wday, data=data_out_fold[indices_div_out_fold,])

	    # coefficients of ridge model
	    coeff <- coef(model)

	    # compute cumulative L1 distance
	    pred_val <- predict(model, data_in_fold[indices_div_in_fold,])
	    pred_dist <- pred_dist + sum(abs(pred_val - data_in_fold[indices_div_in_fold, 'time_to_wifi']))
	    count <- count + length(indices_div_in_fold)
	  }
	}
      }
    }
  }
  pred_dist = pred_dist/count
  return(pred_dist)
}

require(MASS)
input_file <- "prepared_data.txt";
data <- read.table(input_file)

bearing_div_iter <- seq(1,10, 5);
lat_div_iter <- seq(1, 20, 5);
lng_div_iter <- seq(1, 20, 5);

min_dist <- 100000000
min_bearing_div <- 0
min_lat_div <- 0
min_lng_div <- 0

for (bearing_div in bearing_div_iter) {
  for (lat_div in lat_div_iter) {
    for (lng_div in lng_div_iter) {
      pred_dist <- regress_lm(data, lat_div, lng_div, bearing_div) 
      print(bearing_div)
      print(lat_div)
      print(lng_div)
      print(pred_dist)
      if (pred_dist < min_dist) {
	min_dist <- pred_dist
	min_bearing_div <- bearing_div
	min_lat_div <- lat_div
	min_lng_div <- lng_div
      }
    }
  }
}
print('Best');
print(min_lat_div);
print(min_lng_div);
print(min_bearing_div);
print(min_dist)