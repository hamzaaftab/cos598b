# Takes the dataframe that comes out of prepare_data and transforms it into 
# appropriate covariates
transform <- function(data, lat_div_size, lng_div_size, bearing_div_size) {
    # Find the number of divisions
    lat_div_total <- ceiling((lat_max - lat_min) / lat_div_size);
    lng_div_total <- ceiling((lng_max - lng_min) / lng_div_size);
    bearing_div_total <- ceiling(360 / bearing_div_size);

    # Convert into matrix for quick operations
    output <- as.matrix(data)
    
    # Reshape the output array
    max_index <- (lat_div_total-1) * lng_div_total * bearing_div_total + (lng_div_total-1) * bearing_div_total + (bearing_div_total-1);
    for (i in 0:max_index) {
        output = cbind(output,0,0,0)
    }
    
    # Loop over rows
    for (i in 1:nrow(data)) {
        # Find the division and the residual values
        lat_div <- floor((data$lat[i] - lat_min) / lat_div_size) + 1;
        lat_value <- data$lat[i] - lat_min - (lat_div - 1) * lat_div_size;
        lng_div <- floor((data$lng[i] - lng_min) / lng_div_size) + 1;
        lng_value <- data$lng[i] - lng_min - (lng_div - 1) * lng_div_size;
        bearing_div <- floor(data$bearing[i] / bearing_div_size) + 1;
        bearing_value <- data$bearing[i] - (bearing_div - 1) * bearing_div_size;
        
        # Find the division index (a number amongst all the divisions)
        index <- (lat_div-1) * lng_div_total * bearing_div_total + (lng_div-1) * bearing_div_total + (bearing_div-1);
        
        # Store values in the correct column
        output[i,9 + 3*index] <- lat_value;
        output[i,9 + 3*index + 1] <- lng_value;
        output[i,9 + 3*index + 2] <- bearing_value;
    }
    
    # Return data after striping unneeded columns
    data = data.frame(output);
    data$lat <- NULL;
    data$lng <- NULL;
    data$bearing <- NULL;
    data
}

# 5-fold cross validated L1 score for a given division size
L1 <- function(data, lat_div_size, lng_div_size, bearing_div_size) {
    # Transform data
    data = transform(data, lat_div_size, lng_div_size, bearing_div_size);
    
    # Make fold assignments
    folds <- sample(rep(1:5, length=nrow(data)));
    
    # Iterate over folds
    predicted <- c();
    for (fold in 1:5) {
        # Divide into in fold and out of fold data
        out_of_fold <- data[folds != fold , ];
        in_fold <- data[folds == fold , ];
        
        # Use linear regression
        model <- lm('time_to_wifi ~ .', data=out_of_fold);
        
        # Predict in_fold data
        predicted[folds == fold] <- predict(model, in_fold);
    }
    
    # Convert the negative predictions to zeros
    predicted[which(predicted < 0)] <- 0;
    
    # Actual values
    actual <- data$time_to_wifi;
    
    # Find L1 error
    error <- abs(predicted - actual);
    
    # Remove outliers
    error <- error[which(error < median(error) + 1.5*IQR(error))];
    
    # Return error
    mean(error)
}

# Libraries
library('MASS')

# Constants (make sure they stay in sync across all files)
lat_max <- 40.3530;
lat_min <- 40.3390;
lng_min <- -74.6660;
lng_max <- -74.6440;

# Read data
data <- read.table("prepared_data.txt");

# Values to try
lat_div_size_iter <- seq(0.005, 0.05, 0.005);
lng_div_size_iter <- seq(0.005, 0.05, 0.005);
bearing_div_size_iter <- seq(20, 90, 10);

# Iterate
best_lat_div_size <- 0;
best_lng_div_size <- 0;
best_bearing_div_size <- 0;
best_score <- 1000000000000000000000;
for (lat_div_size in lat_div_size_iter) {
    for (lng_div_size in lng_div_size_iter) {
        for (bearing_div_size in bearing_div_size_iter) {            
            # Evaluate
            score <- L1(data,lat_div_size,lng_div_size,bearing_div_size);
            
            # Show progress
            print(lat_div_size);print(lng_div_size);print(bearing_div_size);print(score);
            
            # Keep track of best
            if (score < best_score) {
                best_score <- score;
                best_lat_div_size <- lat_div_size;
                best_lng_div_size <- lng_div_size;
                best_bearing_div_size <- bearing_div_size;
            }
        }
    }
}

# Print best
print("Best:");
print(best_lat_div_size);print(best_lng_div_size);print(best_bearing_div_size);print(best_score);