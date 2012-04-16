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

    // alarm codes
    private static int WAIT_ALARM_CODE = 101;
    private static int SCHEDULED_ALARM_CODE = 102;

    /* location model - Markov chain of 10 steps */
    private static DataPoint[] loc_steps = new DataPoint[Consts.NUM_MARKOV_STEPS];

    /* location tracking */
    private static LocationListener locationListener;

    private static Location mLocation = null;
    private static Boolean mWifiFound = null;
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
        mWifiFound = null;
        mCollectingData = true;
        // start wifi scan
        WifiManager wm = (WifiManager) context.getSystemService (Context.WIFI_SERVICE);
        if (wm.isWifiEnabled()) {
            wm.startScan();
        } else {
            // uh oh. wifi is probably off
            Utils.toast(context, "DroiDTN: Cannot scan for wifi availability. Please check if wifi on.");
        }
        // start gps scan
        LocationManager lm = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        Criteria criteria = new Criteria();
        criteria.setAccuracy(Criteria.ACCURACY_FINE);
        try {
            lm.requestSingleUpdate(criteria, locationListener, null);
        } catch (Exception e) {
            // uh oh. GPS is probably off
            Utils.toast(context, "DroiDTN: Cannot request location. Please check if the GPS Setting is on.");
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
        if (mLocation == null || mWifiFound == null) {
            newPoint(mLocation, mWifiFound, false, context);
        }
        mLocation = null;
        mWifiFound = null;
        mCollectingData = false;
    }

    /*
     * called when scan results are available
     */
    public synchronized static void onScanResults(Context context) {
        WifiManager w = (WifiManager) context.getSystemService (Context.WIFI_SERVICE);
        mWifiFound = gotWifi(w.getScanResults(), context);
        if (mLocation != null && mCollectingData) {
            newPoint(mLocation, mWifiFound, true, context);
            mCollectingData = false;
        }
    }

    /*
     * Helper function for determining if wifi is available
     */
    private static boolean gotWifi(List<ScanResult> list, Context context) {
        if (list != null) {
            WifiManager wm = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
            List<WifiConfiguration> remembered = wm.getConfiguredNetworks();
            for (ScanResult result : list) {
                for (String ssid : Consts.SSID_WHITELIST) {
                    if (result.SSID.equals(ssid) && result.level >= Consts.MIN_WIFI_POWER) {
                        return true;
                    }
                }
                for (WifiConfiguration config : remembered) { // check in remembered SSIDs
                    if (config.SSID.charAt(0) == '\"' && config.SSID.charAt(config.SSID.length()-1) == '\"') { // SSIDs are usually in "", need to strip those out
                        if (result.SSID.equals(config.SSID.substring(1, config.SSID.length()-1)) && result.level >= Consts.MIN_WIFI_POWER) {
                            return true;
                        }
                    }
                    else  if (result.SSID.equals(config.SSID) && result.level >= Consts.MIN_WIFI_POWER) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /*
     * called when gps results are available
     */
    private synchronized static void onLocation(Location location, Context context) {
        mLocation = location;
        if (mWifiFound != null && mCollectingData) {
            newPoint(mLocation, mWifiFound, true, context);
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
    private static void newPoint(Location location, Boolean wifiFound, boolean valid, Context context) {
        // if wifi was found
        if (wifiFound != null && wifiFound) {
            // mark earlier points as having eventually found wifi
            for (int i = 0; i < Consts.NUM_MARKOV_STEPS; i++) {
                if (loc_steps[i] != null && loc_steps[i].getTimeTillWifi() == -1) {
                    // mark as having found wifi
                    loc_steps[i].setTimeTillWifi(Consts.TIME_GRANULARITY * (i+1));

                    // add to database
                    if (loc_steps[i].isValid()) {
                        DatabaseHelper.addPoint(context, loc_steps[i]);
                        Utils.toast_test(context, "store point");
                    }

                    // remove from markov model
                    loc_steps[i] = null;
                }
            }

            // move stuff up
            for (int i = Consts.NUM_MARKOV_STEPS-1; i > 0; i--) {
                loc_steps[i] = loc_steps[i-1];
            }
            loc_steps[0] = null;

            // add new point
            if (valid) {
                DataPoint point_add;
                point_add = new DataPoint(location.getLatitude(), location.getLongitude(), location.getBearing(), wifiFound, System.currentTimeMillis(), 0, location.getSpeed(), location.getAccuracy());
                Utils.toast_test(context, "valid point: location found, wifi available");
                DatabaseHelper.addPoint(context, point_add);
                Utils.toast_test(context, "store point");
            } else {
                Utils.toast_test(context, "invalid point: location not found, wifi available");
            }
        }
        // if wifi was not found
        else {
            // store the data point to be removed
            DataPoint point_last = loc_steps[Consts.NUM_MARKOV_STEPS-1];

            // move stuff up
            for (int i = Consts.NUM_MARKOV_STEPS-1; i > 0; i--) {
                loc_steps[i] = loc_steps[i-1];
            }

            // add new point to markov model
            DataPoint point_add;
            if (valid) {
                point_add = new DataPoint(location.getLatitude(), location.getLongitude(), location.getBearing(), wifiFound, System.currentTimeMillis(), -1, location.getSpeed(), location.getAccuracy());
                Utils.toast_test(context, "valid point: location found, wifi unavailable");
            } else {
                point_add = DataPoint.getInvalid();
                Utils.toast_test(context, "invalid point: location not found, wifi unavailable");
            }
            loc_steps[0] = point_add;

            // add last data point to database
            if (point_last != null && point_last.isValid()) {
                DatabaseHelper.addPoint(context, point_last);
                Utils.toast_test(context, "store point");
            }
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