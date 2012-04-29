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

    // ------------------------ HTTP Constants --------------------------------- //

    // Number of data points to send in one http request
    public static final int HTTP_BATCH_LIMIT = 10;

    // URL for sending data to backend
    public static final String SEND_POINTS_URL = "http://cos598b.appspot.com/add_data";

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
