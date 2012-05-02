# Takes the dataframe that comes out of prepare_data and transforms it into 
# appropriate covariates
transform <- function(data) {
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

# Constants (make sure they stay in sync acroos all files)
lat_max <- 40.3530;
lat_min <- 40.3390;
lng_min <- -74.6660;
lng_max <- -74.6440;

# The dimensions of a division
lat_div_size <- 0.005;
lng_div_size <- 0.005;
bearing_div_size <- 30;

# Read data
data <- read.table("prepared_data.txt");

# Transform data
data = transform(data)