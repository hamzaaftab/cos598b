# regression with simple lm
# 5-fold cross validation with folds calculated within each division

regress_lm2 <- function(data) {
  # constants

    lat_max <- 40.3510
    lat_min <- 40.3490
    lng_min <- -74.6540
    lng_max <- -74.6520
  lat_range <- lat_max - lat_min
  lng_range <- lng_max - lng_min

  # parameters
  bearing_div <- 4;
  lat_div <- 4;
  lng_div <- 4;
  num_folds <- 5;

  # prediction distance
  pred_dist <- 0

  count<-0

  # loop over all bearing divisions
  for (x in 1: bearing_div) {
    # bearing range in this division
    bearing_min_div <- (360/bearing_div) * (x-1)
    bearing_max_div <- (360/bearing_div) * x
    # loop over all latitude divisions
    for (y in 1: lat_div) {
      #latitude range in this division
      lat_min_div <- lat_min + ((lat_range/lat_div) * (y-1))
      lat_max_div <- lat_min + ((lat_range/lat_div) * y)
      # loop over all longitude divisions
      for (z in 1: lng_div) {
	# longitude range in this division
	lng_min_div <- lng_min + ((lng_range/lng_div) * (z-1))
	lng_max_div <- lng_min + ((lng_range/lng_div) * z)

	indices_div <- rownames(data)[which(data$bearing >= bearing_min_div &
				 data$bearing <  bearing_max_div &
				 data$lat     >= lat_min_div     &
				 data$lat     <  lat_max_div     &
				 data$lng     >= lng_min_div     &
				 data$lng     <  lng_max_div, arr.ind=TRUE)]

	data_div <- data[indices_div,]

	if (nrow(data_div) > 1) {

	  folds <- sample(1:num_folds, nrow(data_div), replace=TRUE)
	  for (k in 1:num_folds) {
	    # fold indices
	    indices_div_in_fold <- which(folds == k)
	    indices_div_out_fold <- which(folds != k)

	    # fold data
	    data_div_in_fold <- data_div[indices_div_in_fold,]
	    data_div_out_fold <- data_div[indices_div_out_fold,]

	    if (length(indices_div_out_fold) > 0) {
	      # train model on out of fold data in this lat/lng/bearing division
	      model <- lm(time_to_wifi ~ speed+time+wday, data=data_div_out_fold[,])

	      # compute cumulative L1 distance
	      if (length(indices_div_in_fold) > 0) {
		pred_val <- predict(model, data_div_in_fold[,])
		pred_dist <- pred_dist + sum(abs(pred_val - data_div_in_fold[, 'time_to_wifi']))
		count <- count + length(indices_div_in_fold)
	      }
	    }
	  }
	}
      }
    }
  }
  pred_dist = pred_dist/count
  print(pred_dist)
  print(count)
}

require(MASS)
input_file <- "prepared_data.txt";
data <- read.table(input_file)
regress_lm2(data)