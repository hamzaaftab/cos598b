/************************************
 * TO DOS:
 * 1. Popup window for turning GPS on
 * 2. If user doesn't turn GPS on, then quit?
 * 3. If GPS not returning a location (e.g. indoors), then? In this case, it just ignores all this data.
 * 4. So far, only checking if Wifi is *connected*. Should check if any open Wifi is available?
 * 5. If there is movement in 60 seconds, but no movement in 5-second span, we can't detect direction. Currently, it uses the lat/long
 *    from the previous 60 second span (less accurate since more motion in 60 seconds than 5). Is this okay,
 *    or should we just discard that data?
 * 6. What if we go out of range of GPS and then get WiFi (e.g. enter a building and then get WiFi). In this
 * 	  case, we wouldn't get any location updates, and will not be capture the fact that WiFi was found.
 */

package com.cos598b;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.location.LocationManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.os.Handler;
import android.provider.Settings;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.TextView;


public class Home extends Activity {

    /* is WiFi connected */
    private boolean isConnectedWiFi() {
        ConnectivityManager cm = (ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo[] ni = cm.getAllNetworkInfo();
        for (NetworkInfo n : ni) {
            if (n.getTypeName().equalsIgnoreCase("wifi") && n.isConnected()) {
                return true;
            }
        }
        return false;
    }

    /* is mobile data connected? */
    private boolean isConnectedMobile() {
        ConnectivityManager cm = (ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo[] ni = cm.getAllNetworkInfo();
        for (NetworkInfo n : ni) {
            if (n.getTypeName().equalsIgnoreCase("mobile") && n.isConnected()) {
                return true;
            }
        }
        return false;
    }

    /* stop WiFi, if enabled */
    private void disableWiFi() {
        WifiManager wm = (WifiManager) getSystemService(Context.WIFI_SERVICE);
        if (wm.isWifiEnabled()) { wm.setWifiEnabled(false); }
    }

    /* start WiFi, if disabled */
    private void enableWiFi() {
        WifiManager wm = (WifiManager) getSystemService(Context.WIFI_SERVICE);
        if (!wm.isWifiEnabled()) { wm.setWifiEnabled(true); }
    }

    private void stop3G() {
        // impossibru
    }

    // send data points to back-end
    private void sendPoints() {
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run(){
                while (DatabaseHelper.getNumRows(Home.this) > 0) {
                    Map<String, String> data = DatabaseHelper.popFew(Home.this);
                    // Create a new HttpClient and Post Header
                    HttpClient httpclient = new DefaultHttpClient();
                    HttpPost httppost = new HttpPost(Consts.SEND_POINTS_URL);
                    try {
                        // Add data
                        List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(2);
                        nameValuePairs.add(new BasicNameValuePair("lat", data.get(DatabaseHelper.KEY_LAT)));
                        nameValuePairs.add(new BasicNameValuePair("lng", data.get(DatabaseHelper.KEY_LNG)));
                        nameValuePairs.add(new BasicNameValuePair("bearing", data.get(DatabaseHelper.KEY_BEARING)));
                        nameValuePairs.add(new BasicNameValuePair("timestamp", data.get(DatabaseHelper.KEY_TIMESTAMP)));
                        nameValuePairs.add(new BasicNameValuePair("time", data.get(DatabaseHelper.KEY_TIME_TILL_WIFI)));
                        nameValuePairs.add(new BasicNameValuePair("speed", data.get(DatabaseHelper.KEY_SPEED)));
                        nameValuePairs.add(new BasicNameValuePair("accuracy", data.get(DatabaseHelper.KEY_ACCURACY)));
                        nameValuePairs.add(new BasicNameValuePair("user_id", Utils.getDeviceID(Home.this)));
                        httppost.setEntity(new UrlEncodedFormEntity(nameValuePairs));

                        // make attempts
                        int attempt = 0;
                        while (attempt < Consts.HTTP_MAX_ATTEMPTS) {
                            HttpResponse response = httpclient.execute(httppost);
                            if (response.getStatusLine().getStatusCode() == 200) {
                                break;
                            } else {
                                attempt = attempt + 1;
                            }
                        }
                    } catch (ClientProtocolException e) {
                        Log.d("Network error", e.toString());
                    } catch (IOException e) {
                        Log.d("Network error", e.toString());
                    }
                }
            }
        });
        thread.start();
    }

    // fetch predictions from back-end
    private void getPredictions() {

    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        // start markov service for collecting data points
        startService(new Intent(this, MarkovService.class));

        setContentView(R.layout.main);
        findViewById(R.id.send_button).setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View arg0) {
                if (isConnectedWiFi() || isConnectedMobile()) {
                    sendPoints();
                } else {
                    Utils.toast(Home.this, "DroiDTN: Internet Connection is unavailable. Please try again later.");
                }
            }
        });
        findViewById(R.id.get_button).setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View arg0) {
                getPredictions();
            }
        });
        
        // if GPS is disabled, ask user to turn it on 
    	// Runs only once, when activity is created
    	// XXX: Alternately could put it in onResume() to constantly remind user
    	LocationManager lm = (LocationManager) getSystemService(Context.LOCATION_SERVICE);
    	if (!lm.isProviderEnabled(LocationManager.GPS_PROVIDER)) {
    		showDialog(0);
    	}
    }

    // handler and runnable to update number of points
    private Handler updateHandler = new Handler();
    private Runnable updateRunnable = new Runnable() {
    	@Override
    	public void run() {
    		refreshNumPoints();
    	}
    };
    
    /**
     * Called every time activity gets focus
     */
    @Override
    protected void onResume() {
        super.onResume();
        
        refreshNumPoints();
    }
    
    /**
     * Called whenever activity loses focus
     * Stops the refreshing of number of data points
     */
    @Override
    protected void onPause() {
    	super.onPause();
    	updateHandler.removeCallbacks(updateRunnable);
    	Log.d("Refresh", "Stop refreshing");
    }
    
    /**
     * refresh the number of points collected
     * sets timer to refresh points again according to REFRESH_RATE
     */
    private void refreshNumPoints() {
        int num_points = DatabaseHelper.getNumRows(this);
        TextView tv = (TextView) findViewById(R.id.num_rows);
        tv.setText(Integer.toString(num_points));
        updateHandler.postDelayed(updateRunnable, Consts.REFRESH_RATE*1000); // update at twice the rate of points being found
        Log.d("Refresh", "Refreshed number of datapoints");
    }  
    
    /**
     * Dialog to ask user to turn on GPS
     */
    @Override
    protected Dialog onCreateDialog(int id) {
    	AlertDialog alert;
    	if (id == 0) {
    		AlertDialog.Builder builder = new AlertDialog.Builder(this);
    		builder.setMessage("GPS is turned off. Would you like to turn it on?")
    			   .setCancelable(false)
    			   .setPositiveButton("Yes", new DialogInterface.OnClickListener() {
    				   public void onClick(DialogInterface dialog, int id) {
    					   Intent gpsIntent = new Intent(Settings.ACTION_LOCATION_SOURCE_SETTINGS);
    					   gpsIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
    					   startActivity(gpsIntent);
    				   }
    			   })
    			   .setNegativeButton("No", new DialogInterface.OnClickListener() {
    				   public void onClick(DialogInterface dialog, int id) {
    					   dialog.cancel();
    				   }
    			   });
    		alert = builder.create();       
    	}
    	else alert = null;
    	return alert;
    }
}