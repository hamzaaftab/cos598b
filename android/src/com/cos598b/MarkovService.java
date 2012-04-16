package com.cos598b;

import java.util.List;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.net.wifi.ScanResult;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;

public class MarkovService extends Service {

    // value to return if no wifi found
    private static final int WIFI_MIN_POWER_LEVEL = -1000;

    // alarm codes
    private static int WAIT_ALARM_CODE = 101;
    private static int SCHEDULED_ALARM_CODE = 102;

    /* location model - Markov chain of 10 steps */
    private static DataPoint[] loc_steps = new DataPoint[Consts.NUM_MARKOV_STEPS];

    /* location tracking */
    private static LocationListener locationListener;

    private static Location mLocation = null;
    private static Integer mWifiPowerLevel = null;
    private static boolean mCollectingData;
    private static boolean mServiceRunning;

    /* handler for printing to Toast */
    Handler toastHandler;

    @Override
    public int onStartCommand (Intent intent, int flags, int startId) {
        if (mServiceRunning == false) {
            onStart();
            mServiceRunning = true;
        }
        return super.onStartCommand(intent, flags, startId);
    }

    /*
     * called when the service is started
     */
    private void onStart() {
        // setup listener for location updates
        locationListener = new LocationListener() {
            @Override
            public void onLocationChanged(Location arg0) {
                onLocation(arg0, MarkovService.this);
            }
            @Override
            public void onProviderDisabled(String arg0) {
            }
            @Override
            public void onProviderEnabled(String provider) {
            }
            @Override
            public void onStatusChanged(String provider, int status, Bundle extras) {
            }
        };
        // set up an alarm for every data point
        AlarmManager am = (AlarmManager) getSystemService(Context.ALARM_SERVICE);
        Intent newintent = new Intent(this, ScheduledAlarmReceiver.class);
        PendingIntent operation = PendingIntent.getBroadcast(this, SCHEDULED_ALARM_CODE, newintent, PendingIntent.FLAG_UPDATE_CURRENT);
        am.setRepeating(AlarmManager.RTC_WAKEUP, System.currentTimeMillis(), Consts.TIME_GRANULARITY*1000, operation);
    }

    /*
     * called when the alarm for each data point goes off
     */
    public synchronized static void onAlarm(Context context) {
        mLocation = null;
        mWifiPowerLevel = null;
        mCollectingData = true;
        // start wifi scan
        WifiManager wm = (WifiManager) context.getSystemService (Context.WIFI_SERVICE);
        if (wm.isWifiEnabled()) {
            wm.startScan();
        } else {
            // uh oh. wifi is probably off
            Utils.toast(context, "DroiDTN: Cannot scan for wifi availability. Please check if wifi on.");

            mCollectingData = false;
        }
        // start gps scan
        LocationManager lm = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        Criteria criteria = new Criteria();
        criteria.setAccuracy(Criteria.ACCURACY_FINE);
        criteria.setBearingRequired(true);
        criteria.setBearingAccuracy(Criteria.ACCURACY_HIGH);
        criteria.setSpeedRequired(true);
        criteria.setSpeedAccuracy(Criteria.ACCURACY_HIGH);
        try {
            lm.requestSingleUpdate(criteria, locationListener, null);
        } catch (Exception e) {
            // uh oh. GPS is probably off
            Utils.toast(context, "DroiDTN: Cannot request location. Please check if the GPS Setting is on.");

            mCollectingData = false;
        }
        // alarm for when we dont get location/scan in time
        AlarmManager am = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        Intent newintent = new Intent(context, WaitAlarmReceiver.class);
        PendingIntent operation = PendingIntent.getBroadcast(context, WAIT_ALARM_CODE, newintent, PendingIntent.FLAG_UPDATE_CURRENT);
        am.set(AlarmManager.RTC_WAKEUP, System.currentTimeMillis() + Consts.MAX_WAIT*1000, operation);
    }

    /*
     * When the timer expires and we still dont have location/scan updates
     */
    public synchronized static void onNoResult(Context context) {
        if (locationListener != null) {
            LocationManager lm = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
            lm.removeUpdates(locationListener);
        }
        if (mLocation == null || mWifiPowerLevel == null) {
            newPoint(mLocation, mWifiPowerLevel, false, context);
        }
        mLocation = null;
        mWifiPowerLevel = null;
        mCollectingData = false;
    }

    /*
     * called when scan results are available
     */
    public synchronized static void onScanResults(Context context) {
        WifiManager w = (WifiManager) context.getSystemService (Context.WIFI_SERVICE);
        mWifiPowerLevel = gotWifi(w.getScanResults(), context);
        if (mLocation != null && mCollectingData) {
            newPoint(mLocation, mWifiPowerLevel, true, context);
            mCollectingData = false;
        }
    }

    /*
     * Helper function for determining if wifi is available, and returning max power level of available wifi
     */
    private static Integer gotWifi(List<ScanResult> list, Context context) {
        Integer wifi_power_level = null;
        if (list != null) {
            wifi_power_level = WIFI_MIN_POWER_LEVEL;
            WifiManager wm = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
            List<WifiConfiguration> remembered = wm.getConfiguredNetworks();
            for (ScanResult result : list) {
                for (String ssid : Consts.SSID_WHITELIST) {
                    if (result.SSID.equals(ssid)) {
                        if (wifi_power_level == null || wifi_power_level < result.level) {
                            wifi_power_level = result.level;
                        }
                    }
                }
                for (WifiConfiguration config : remembered) { // check in remembered SSIDs
                    if (config.SSID.charAt(0) == '\"' && config.SSID.charAt(config.SSID.length()-1) == '\"') { // SSIDs are usually in "", need to strip those out
                        if (result.SSID.equals(config.SSID.substring(1, config.SSID.length()-1))) {
                            if (wifi_power_level == null || wifi_power_level < result.level) {
                                wifi_power_level = result.level;
                            }
                        }
                    }
                    else  if (result.SSID.equals(config.SSID)) {
                        if (wifi_power_level == null || wifi_power_level < result.level) {
                            wifi_power_level = result.level;
                        }
                    }
                }
            }
        }
        return wifi_power_level;
    }

    /*
     * called when gps results are available
     */
    private synchronized static void onLocation(Location location, Context context) {
        mLocation = location;
        if (mWifiPowerLevel != null && mCollectingData) {
            newPoint(mLocation, mWifiPowerLevel, true, context);
            mCollectingData = false;
        }
    }

    /*
     * New data point is available
     * Called every 60 seconds
     * valid: whether the data point is valid or not (could be invalid if it is missing
     *  location, scan etc info which could happen if we are inside a building, etc). if
     *  it is invalid then location and wifiFound are null
     * location: location returned by gps location
     * wifiFound: whether we had access to wifi at this point (not eventually)
     * 
     */
    private static void newPoint(Location location, Integer wifi_power_level, boolean valid, Context context) {
        // if wifi was found
        if (wifi_power_level == null) {
            wifi_power_level = WIFI_MIN_POWER_LEVEL; // same large negative number
        }

        // add wifi power level to earlier points
        for (int i = 0; i < Consts.NUM_MARKOV_STEPS; i++) {
            if (loc_steps[i] != null) {
                // mark as having found wifi
                loc_steps[i].addWifiPowerLevel(wifi_power_level);
            }
        }

        // store earliest point
        DataPoint point_last = loc_steps[Consts.NUM_MARKOV_STEPS-1];

        // move stuff up
        for (int i = Consts.NUM_MARKOV_STEPS-1; i > 0; i--) {
            loc_steps[i] = loc_steps[i-1];
        }
        loc_steps[0] = null;

        // add new point to markov model
        DataPoint point_add;
        if (valid) {
            point_add = new DataPoint(location.getLatitude(), location.getLongitude(), location.getBearing(), System.currentTimeMillis(), location.getSpeed(), location.getAccuracy());
            point_add.addWifiPowerLevel(wifi_power_level);
            Utils.toast_test(context, "location found. power level is ".concat(Integer.toString(wifi_power_level)));
        } else {
            point_add = DataPoint.getInvalid();
            point_add.addWifiPowerLevel(wifi_power_level);
            Utils.toast_test(context, "location not found. power level is ".concat(Integer.toString(wifi_power_level)));
        }
        loc_steps[0] = point_add;

        // add last data point to database
        if (point_last != null && point_last.isValid()) {
            DatabaseHelper.addPoint(context, point_last);
            Utils.toast_test(context, "store point");
        }
    }

    /*
     * on creating the service
     */
    @Override
    public void onCreate()
    {
        super.onCreate();
        // initialize the toast handler
        toastHandler = new Handler();
    }
    /*
     * on destroying the service
     */
    @Override
    public void onDestroy() {
        super.onDestroy();
        mServiceRunning = false;
    }

    // helper for showing the toast notification
    private void showToast(String string) {
        toastHandler.post(new DisplayToast(string));
    }

    /*
     * display a toast from within non-UI thread
     */
    private class DisplayToast implements Runnable {
        String text;

        public DisplayToast(String text){
            this.text = text;
        }

        @Override
        public void run(){
            Utils.toast_test(getApplicationContext(), text);
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

}