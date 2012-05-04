package com.cos598b;


public class Consts {

    // ------------------------ Markov Constants ------------------------------- //

    // time granularity for location updates (in seconds)

    public static final int TIME_GRANULARITY = 10;

    // maximum wait for a gps location / wifi scan to return (in seconds)
    public static final int MAX_WAIT = 8;

    // total length of markov model in seconds
    public static final int MARKOV_TOTAL_SECONDS = 10*60;

    // how many steps of location data to store
    public static final int NUM_MARKOV_STEPS = MARKOV_TOTAL_SECONDS / TIME_GRANULARITY;

    // supported wireless SSID's
    public static final String[] SSID_WHITELIST = {"puwireless", "csvapornet"};

    // refresh rate for number of datapoints on main activity (in seconds)
    public static final int REFRESH_RATE = 10;

    // ------------------------ Prediction Constants --------------------------- //

    // We will scan for wifi this often (seconds)
    public static final int WIFI_SCAN_FREQUENCY = 10;

    // Bearing will be reduced in cluster prediction by this much
    public static final double BEARING_MULTIPLIER = 1/18000;

    // Minimum good wifi RSSI to be considered connected
    public static final int MIN_WIFI_RSSI = -70;

    // ------------------------ HTTP Constants --------------------------------- //

    // Number of data points to send in one http request
    public static final int HTTP_BATCH_LIMIT = 10;

    // URL for sending data to backend
    public static final String SEND_POINTS_URL = "http://cos598b.appspot.com/add_data";

    // Prediction Model
    public static final String PREDICTION_MODEL_URL = "https://raw.github.com/hamzaaftab/cos598b/master/R/kmeans_model.txt";

    // number of tries to make for an http request before giving up
    public static final int HTTP_MAX_ATTEMPTS = 3;

    // ------------------------ Test Constants --------------------------------- //

    // testing messages etc will only appear on these phones
    public static final String[] TEST_DEVICE_WHITELIST = {
        "e8bce1e69b89be6f",      // Hamza's personal Phone
        "892ff7ea98149a55",      // Galaxy Nexus we got for the project
        "72890e4ed7e94cae"		 // Shreshth's personal phone
    };
}
