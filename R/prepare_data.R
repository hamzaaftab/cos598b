# Prepares Raw Data downloaded from app engine to something we can use for
# machine learning.

prepare_data <- function() {
    # Constants
    input_file <- "data.txt";
    output_file <- "prepared_data.txt";
    power_threshold <- -70;
    accuracy_threshold <- 25;
    lat_max <- 40.3530;
    lat_min <- 40.3390;
    lng_min <- -74.6660;
    lng_max <- -74.6440;
    markov_length = 10*60;

    # Read data
    data <- read.table(input_file);
    
    # Remove inaccurate readings
    data <- data[which(data$accuracy <= accuracy_threshold),];
    
    # Filter out of bound locations
    data <- data[which(data$lat <= lat_max),];
    data <- data[which(data$lat >= lat_min),];
    data <- data[which(data$lng <= lng_max),];
    data <- data[which(data$lng >= lng_min),];
    
    # convert timestamp to time of day (in seconds)
    temp <- as.POSIXlt(data$timestamp/1000, 'EST', origin="1970-01-01");
    data$time <- temp$sec + temp$min * 60 + temp$hour * 60 * 60;
    
    # Get day of the week
    data$wday <- temp$wday;
    
    # Convert user_id to user number (1 to NUM_USERS)
    temp <- unique(data$user_id);
    temp2 <- c();
    for (point in 1:length(data$user_id)) {
        for (i in 1:length(temp)) {
            if (data$user_id[point] == temp[i]) {
                temp2[point] <- i;
            }
        }
    }
    data$user_id <- temp2;
    
    # Find how long it took to get wifi
    for (point in 1:length(data$wifi_power_levels)) {
        levels <- as.numeric(unlist(strsplit(toString(data$wifi_power_levels[point]), "\\.")));
        l <- length(levels);
        temp = which(levels >= power_threshold);
        if (length(temp) == 0) {
            data$got_wifi[point] <- FALSE;
            data$time_to_wifi[point] <- -1;
        } else {
            data$got_wifi[point] <- TRUE;
            data$time_to_wifi[point] <- (temp[1] - 1) / (l - 1) * markov_length;
        }
    }
    
    # Strip column names that are not needed
    data$accuracy <- NULL;
    data$timestamp <- NULL;
    data$wifi_power_levels <- NULL
    
    row.names(data) <- 1:nrow(data)
    # Write data
    write.table(data, "prepared_data.txt");
}

prepare_data();