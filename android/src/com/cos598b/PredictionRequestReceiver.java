package com.cos598b;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.location.Criteria;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.os.Handler;

public class PredictionRequestReceiver extends BroadcastReceiver {

    private Context mContext;
    private int tolerance;
    private int timer;
    private boolean callbackPending;

    // handler and runnable to scan for wifi
    private Handler scanHandler = new Handler();
    private Runnable scanRunnable = new Runnable() {
        @Override
        public void run() {
            doWifiScan();
        }
    };

    @Override
    public void onReceive(Context context, Intent intent) {
        mContext = context;
        tolerance = intent.getIntExtra("tolerance", 0);
        timer = 0;
        callbackPending = true;

        // callback immediately if wifi is turned off
        if (!isWifiEnabled()) {
            callback("wifi not enabled");
            return;
        }
        
        Utils.toast(mContext, "tolerance is " + tolerance);

        // scan for wifi
        scanHandler.post(scanRunnable);

        // scan for location
        if (callbackPending) {
            doLocationScan();
        }
    }

    private void callback(String reason) {
        if (callbackPending) {
            Intent callback_intent = new Intent("com.cos598b.callback");
            callback_intent.putExtra("reason", reason);
            mContext.sendBroadcast(callback_intent);
            callbackPending = false;
        }
    }

    private void doLocationScan() {
        LocationManager lm = (LocationManager) mContext.getSystemService(Context.LOCATION_SERVICE);
        Criteria criteria = new Criteria();
        criteria.setAccuracy(Criteria.ACCURACY_FINE);
        criteria.setBearingRequired(true);
        criteria.setBearingAccuracy(Criteria.ACCURACY_HIGH);
        criteria.setSpeedRequired(true);
        criteria.setSpeedAccuracy(Criteria.ACCURACY_HIGH);
        LocationListener locationListener = new LocationListener() {
            @Override
            public void onLocationChanged(Location arg0) {
                onLocation(arg0);
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
        try {
            lm.requestSingleUpdate(criteria, locationListener, null);
        } catch (Exception e) {
            callback("location not available");
        }
    }

    private void onLocation(Location location) {
        // get prediction
        int time_to_wifi = DatabaseHelper.predict(mContext, location.getLatitude(), location.getLongitude(), location.getBearing(), location.getSpeed(), location.getAccuracy(), System.currentTimeMillis());

        // if prediction is too long, then callback
        if (time_to_wifi > tolerance - timer) {
            callback("wifi will not be available soon");
        } else {
            // display prediction
            if (callbackPending) {
                Utils.toast(mContext, "Wifi unavailable, but expected in " + Integer.toString(time_to_wifi-timer) + " seconds");
            }
        }
    }

    private void doWifiScan() {
        // scan
        WifiManager wm = (WifiManager) mContext.getSystemService (Context.WIFI_SERVICE);
        wm.startScan();

        // if wifi is connected, then callback
        if (isConnectedWiFi() && wm.getConnectionInfo().getRssi() > Consts.MIN_WIFI_RSSI) {
            callback("wifi found");
        } else {
            // increment timer
            timer = timer + Consts.WIFI_SCAN_FREQUENCY;

            // callback if timer > tolerance
            if (timer > tolerance) {
                callback("timer expired");
            } else {
                // prepare for next scan
                scanHandler.postDelayed(scanRunnable, Consts.WIFI_SCAN_FREQUENCY*1000);
            }
        }
    }

    private boolean isConnectedWiFi() {
        ConnectivityManager cm = (ConnectivityManager) mContext.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo[] ni = cm.getAllNetworkInfo();
        for (NetworkInfo n : ni) {
            if (n.getTypeName().equalsIgnoreCase("wifi") && n.isConnected()) {
                return true;
            }
        }
        return false;
    }

    private boolean isWifiEnabled() {
        WifiManager wm = (WifiManager) mContext.getSystemService(Context.WIFI_SERVICE);
        return wm.isWifiEnabled();
    }

}