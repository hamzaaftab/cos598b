package com.cos598b;

/*
 * class for data points
 */
public class DataPoint {
    private double lat;
    private double lng;
    private float bearing;               // 0 - 360 degrees
    private double timestamp;            // ms
    private String wifi_power_levels;    // the wifi power level for the next 10 data points (dot_seperated), the first value is the points own power level
    private boolean valid;               // whether the data point is valid or not (because we did not have gps or something)
    private float speed;                 // speed
    private float accuracy;              // accuracy of gps reading

    public DataPoint(double lat, double lng, float bearing, double timestamp, float speed, float accuracy) {
        this.lat = lat;
        this.lng = lng;
        this.bearing = bearing;
        this.timestamp = timestamp;
        this.speed = speed;
        this.accuracy = accuracy;
        this.valid = true;
        this.wifi_power_levels = "";
    }

    // return an invalid datapoint
    public static DataPoint getInvalid() {
        DataPoint dp = new DataPoint(0,0,0,0,0,0);
        dp.valid = false;
        return dp;
    }

    public double getLat() { return this.lat; }
    public double getLng() { return this.lng; }
    public double getBearing() { return this.bearing; }
    public double getTimestamp() { return this.timestamp; }
    public String getWifiPowerLevels() { return this.wifi_power_levels; }
    public float getSpeed() { return this.speed; }
    public float getAccuracy() { return this.accuracy; }
    public boolean isValid() { return this.valid; }
    public void addWifiPowerLevel(int n) {
        if (this.wifi_power_levels.equals("")) {
            this.wifi_power_levels = Integer.toString(n);
        } else {
            this.wifi_power_levels = this.wifi_power_levels.concat(".").concat(Integer.toString(n));
        }
    }

}