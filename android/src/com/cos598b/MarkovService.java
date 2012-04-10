package com.cos598b;

import java.util.List;

import android.app.AlarmManager;
import android.app.IntentService;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.net.wifi.ScanResult;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.os.Handler;

public class MarkovService extends IntentService {

    // alarm codes
    private static int WAIT_ALARM_CODE = 101;
    private static int SCHEDULED_ALARM_CODE = 102;

    /* location model - Markov chain of 10 steps */
    private static DataPoint[] loc_steps = new DataPoint[Consts.num_loc_steps];

    /* location tracking */
    private static LocationListener locationListener;

    private static Location mLocation = null;
    private static Boolean mWifiFound = null;
    private static boolean mCollectingData = false;

    /* handler for printing to Toast */
    Handler toastHandler;

    /*
     * constructor
     */
    public MarkovService() {
        super("MarkovService");
    }

    @Override
    public int onStartCommand (Intent intent, int flags, int startId) {
        onStart();
        return super.onStartCommand(intent, flags, startId);
    }

    /*
     * how to handle intents
     */
    @Override
    protected void onHandleIntent(Intent intent) {
        if (!intent.hasExtra("METHOD")) {
            return;
        }
        String method = intent.getExtras().getString("METHOD");
        if (method == null) {
            return;
        } else if (method.equals("start")) {
            onStart();
        }
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
        am.setRepeating(AlarmManager.RTC_WAKEUP, System.currentTimeMillis(), Consts.time_granularity*1000, operation);
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
        wm.startScan();
        // start gps scan
        LocationManager lm = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        Criteria criteria = new Criteria();
        criteria.setAccuracy(Criteria.ACCURACY_FINE);
        lm.requestSingleUpdate(criteria, locationListener, null);
        // alarm for when we dont get location/scan in time
        AlarmManager am = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        Intent newintent = new Intent(context, WaitAlarmReceiver.class);
        PendingIntent operation = PendingIntent.getBroadcast(context, WAIT_ALARM_CODE, newintent, PendingIntent.FLAG_UPDATE_CURRENT);
        am.set(AlarmManager.RTC_WAKEUP, System.currentTimeMillis() + Consts.max_wait*1000, operation);
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
        mWifiFound = gotWifi(w.getScanResults());
        if (mLocation != null && mCollectingData) {
            newPoint(mLocation, mWifiFound, true, context);
        }
    }

    /*
     * Helper function for determining if wifi is available
     */
    private static boolean gotWifi(List<ScanResult> list) {
        for (ScanResult result : list) {
            for (String ssid : Consts.ssids) {
                if (result.SSID.equals(ssid)) {
                    return true;
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
        // if wifi was found, mark earlier points as having eventually found wifi
        if (wifiFound != null && wifiFound) {
            for (int i = 0; i < Consts.num_loc_steps; i++) {
                if (loc_steps[i] != null && loc_steps[i].getTimeTillWifi() == -1) {
                    loc_steps[i].setTimeTillWifi(Consts.time_granularity * (i+1));
                }
            }
        }

        // store the data point to be removed
        DataPoint point_last = loc_steps[Consts.num_loc_steps-1];

        // move stuff up
        for (int i = Consts.num_loc_steps-1; i > 0; i--) {
            loc_steps[i] = loc_steps[i-1];
        }

        // add new point to markov model
        DataPoint point_add;
        if (valid) {
            int time_till_wifi = wifiFound ? 0 : -1;
            point_add = new DataPoint(location.getLatitude(), location.getLongitude(), location.getBearing(), wifiFound, System.currentTimeMillis(), time_till_wifi);
        } else {
            point_add = DataPoint.getInvalid();
        }
        loc_steps[0] = point_add;

        // add last data point to database
        if (point_last != null && point_last.isValid()) {
            DatabaseHelper.addPoint(context, point_last);
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
            Utils.toast(getApplicationContext(), text);
        }
    }

}