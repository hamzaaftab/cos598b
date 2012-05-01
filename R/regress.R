
#[1] "lat"          "lng"          "bearing"      "speed"        "user_id"     
#[6] "time"         "wday"         "time_to_wifi"

regress <- function(data) {
  # constants
  lat_max <- 40.3530;
  lat_min <- 40.3390;
  lat_range <- lat_max - lat_min
  lng_min <- -74.6660;
  lng_max <- -74.6440;
  lng_range <- lng_max - lng_min

  # parameters
  bearing_div <- 1;
  lat_div <- 4;
  lng_div <- 4;
  lambda <- seq(1,4,0.4)
  num_folds <- 5


  folds <- sample(1:num_folds, nrow(data), replace=TRUE)
  for (k in 1:num_folds) {
    # fold indices
    in_fold_index <- which(folds == k)
    out_fold_index <- which(folds != k)

    data_in_fold <- data[in_fold_index,]
    data_out_fold <- data[out_fold_index,]

    for (x in 1: bearing_div) {
      bearing_min_div <- (360/bearing_div) * (x-1)
      bearing_max_div <- (360/bearing_div) * x
      for (y in 1: lat_div) {
	lat_min_div <- lat_min + ((lat_range/lat_div) * (y-1))
	lat_max_div <- lat_min + ((lat_range/lat_div) * y)
	for (z in 1: lng_div) {
	  lng_min_div <- lng_min + ((lng_range/lng_div) * (z-1))
	  lng_max_div <- lng_min + ((lng_range/lng_div) * z)

	  indices <- which(data_out_fold$bearing >= bearing_min_div &
			   data_out_fold$bearing <  bearing_max_div &
			   data_out_fold$lat     >= lat_min_div     &
			   data_out_fold$lat     <  lat_max_div     &
			   data_out_fold$lng     >= lng_min_div     &
			   data_out_fold$lng     <  lng_max_div)

	  if (length(indices) > 0) {
	    print('LOL')
	    print(indices)
	    print(data_out_fold[indices,])
	    model <- lm.ridge(time_to_wifi ~ ., data=data_out_fold[indices,], lambda=lambda)
	    print(summary(model))
	  } else {print('B')}
	}
      }
    }
  }
}

require(MASS)
input_file <- "prepared_data.txt";
data <- read.table(input_file)
regress(data)