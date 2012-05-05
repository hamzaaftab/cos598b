package com.cos598b;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.database.sqlite.SQLiteStatement;

public class DatabaseHelper extends SQLiteOpenHelper {

    // Database Version
    private static final int DATABASE_VERSION = 3;       // after adding PREDICTION MODEL

    // Database Name
    private static final String DATABASE_NAME = "droidtn";

    // Points table name
    private static final String TABLE_POINTS = "points";

    // Points Table Columns names
    public static final String KEY_ID = "id";
    public static final String KEY_LAT = "lat";
    public static final String KEY_LNG = "lng";
    public static final String KEY_BEARING = "bearing";
    public static final String KEY_TIMESTAMP = "timestamp";
    public static final String KEY_WIFI_POWER_LEVELS = "wifi_power_levels";
    public static final String KEY_SPEED = "speed";
    public static final String KEY_ACCURACY = "accuracy";

    // Prediction Model table name
    private static final String TABLE_PREDICTION = "prediction_model";

    // Extra columns for Prediction Model
    public static final String KEY_TIME_TO_WIFI = "time_to_wifi";

    public DatabaseHelper(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        String CREATE_POINTS_TABLE = "CREATE TABLE " + TABLE_POINTS + "("
                        + KEY_ID + " INTEGER PRIMARY KEY," + KEY_LAT + " REAL,"
                        + KEY_LNG + " REAL," + KEY_BEARING + " REAL,"
                        + KEY_TIMESTAMP + " INTEGER,"
                        + KEY_WIFI_POWER_LEVELS + " STRING,"
                        + KEY_SPEED + " REAL," + KEY_ACCURACY + " REAL" + ")";
        db.execSQL(CREATE_POINTS_TABLE);
        String CREATE_PREDICTION_TABLE = "CREATE TABLE " + TABLE_PREDICTION + "("
                        + KEY_ID + " INTEGER PRIMARY KEY," + KEY_LAT + " REAL,"
                        + KEY_LNG + " REAL," + KEY_BEARING + " REAL,"
                        + KEY_TIME_TO_WIFI + " INTEGER" + ")";
        db.execSQL(CREATE_PREDICTION_TABLE);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        // Drop older table if existed
        db.execSQL("DROP TABLE IF EXISTS " + TABLE_POINTS);
        db.execSQL("DROP TABLE IF EXISTS " + TABLE_PREDICTION);

        // Create tables again
        onCreate(db);
    }

    // delete all predictions
    private void deletePredictions() {
        SQLiteDatabase db = this.getWritableDatabase();
        db.execSQL("DELETE FROM " + TABLE_PREDICTION);
        db.close(); // Closing database connection
    }

    // Adding new data point
    private void addPoint(DataPoint point) {
        if (point.isValid()) {
            SQLiteDatabase db = this.getWritableDatabase();

            ContentValues values = new ContentValues();
            values.put(KEY_LAT, point.getLat());
            values.put(KEY_LNG, point.getLng());
            values.put(KEY_BEARING, point.getBearing());
            values.put(KEY_TIMESTAMP, point.getTimestamp());
            values.put(KEY_WIFI_POWER_LEVELS, point.getWifiPowerLevels());
            values.put(KEY_SPEED, point.getSpeed());
            values.put(KEY_ACCURACY, point.getAccuracy());

            // Inserting Row
            db.insert(TABLE_POINTS, null, values);
            db.close(); // Closing database connection
        }
    }

    // Get the number of rows in the database
    private int getNumRows() {
        String sql = "SELECT COUNT(*) FROM " + TABLE_POINTS;
        SQLiteDatabase db = this.getWritableDatabase();
        SQLiteStatement statement = db.compileStatement(sql);
        int count = (int) statement.simpleQueryForLong();
        db.close(); // Closing database connection
        return count;
    }

    // Add a prediction model cluster
    private void addPredictions(List<Double> lat, List<Double> lng, List<Double> bearing, List<Integer> time_to_wifi) {
        SQLiteDatabase db = this.getWritableDatabase();

        db.beginTransaction();
        for (int i = 0; i < time_to_wifi.size(); i++) {
            ContentValues values = new ContentValues();
            values.put(KEY_LAT, lat.get(i));
            values.put(KEY_LNG, lng.get(i));
            values.put(KEY_BEARING, bearing.get(i));
            values.put(KEY_TIME_TO_WIFI, time_to_wifi.get(i));

            // Inserting Row
            db.insert(TABLE_PREDICTION, null, values);
        }
        db.setTransactionSuccessful();
        db.endTransaction();
        db.close(); // Closing database connection
    }

    // Predict time_to_wifi (in seconds)
    private int predict(double lat, double lng, double bearing, double speed, double accuracy, double timestamp) {
        // fetch all clusters
        List<Double> lat_list = new ArrayList<Double>();
        List<Double> lng_list = new ArrayList<Double>();
        List<Double> bearing_list = new ArrayList<Double>();
        List<Integer> time_list = new ArrayList<Integer>();
        String selectQuery = "SELECT * FROM " + TABLE_PREDICTION;
        SQLiteDatabase db = this.getWritableDatabase();
        Cursor cursor = db.rawQuery(selectQuery, null);
        if (cursor.moveToFirst()) {
            do {
                lat_list.add(Double.parseDouble(cursor.getString(1)));
                lng_list.add(Double.parseDouble(cursor.getString(2)));
                bearing_list.add(Double.parseDouble(cursor.getString(3)));
                time_list.add(Integer.parseInt(cursor.getString(4)));
            } while (cursor.moveToNext());
        }
        cursor.close();
        db.close(); // Closing database connection

        // find closest cluster
        double min_dist = Integer.MAX_VALUE;
        int best_time = Integer.MAX_VALUE;
        for (int i = 0; i < time_list.size(); i++) {
            double dist = Math.pow(lat - lat_list.get(i),2)+Math.pow(lng - lng_list.get(i),2)+Math.pow((bearing - bearing_list.get(i))*Consts.BEARING_MULTIPLIER,2);
            if (dist < min_dist) {
                min_dist = dist;
                best_time = time_list.get(i);
            }
        }

        return best_time;
    }

    // Retrieve a few data points and remove them from the database
    // Returns a comma separated string of fields
    private Map<String, String> popFew() {
        Map<String, String> data = new HashMap<String, String>();
        List<String> latList = new ArrayList<String>();
        List<String> lngList = new ArrayList<String>();
        List<String> bearingList = new ArrayList<String>();
        List<String> timestampList = new ArrayList<String>();
        List<String> wifipowerlevelList = new ArrayList<String>();
        List<String> speedList = new ArrayList<String>();
        List<String> accuracyList = new ArrayList<String>();

        // Select All Query
        String selectQuery = "SELECT * FROM " + TABLE_POINTS + " ORDER BY " + KEY_TIMESTAMP + " ASC LIMIT " + Consts.HTTP_BATCH_LIMIT;

        SQLiteDatabase db = this.getWritableDatabase();
        Cursor cursor = db.rawQuery(selectQuery, null);

        long greatestTimeStamp = 0;
        // looping through all rows and adding to list
        if (cursor.moveToFirst()) {
            do {
                long timestamp = Long.parseLong(cursor.getString(4));
                if (timestamp > greatestTimeStamp) {
                    greatestTimeStamp = timestamp;
                }
                latList.add(cursor.getString(1));
                lngList.add(cursor.getString(2));
                bearingList.add(cursor.getString(3));
                timestampList.add(cursor.getString(4));
                wifipowerlevelList.add(cursor.getString(5));
                speedList.add(cursor.getString(6));
                accuracyList.add(cursor.getString(7));
            } while (cursor.moveToNext());
        }
        cursor.close();

        // Delete retrieved points
        db.delete(TABLE_POINTS, KEY_TIMESTAMP + " <= ?", new String[] {Long.toString(greatestTimeStamp)});
        db.close(); // Closing database connection

        data.put(KEY_LAT, Utils.implode(latList.toArray(new String[0]), ","));
        data.put(KEY_LNG, Utils.implode(lngList.toArray(new String[0]), ","));
        data.put(KEY_BEARING, Utils.implode(bearingList.toArray(new String[0]), ","));
        data.put(KEY_TIMESTAMP, Utils.implode(timestampList.toArray(new String[0]), ","));
        data.put(KEY_WIFI_POWER_LEVELS, Utils.implode(wifipowerlevelList.toArray(new String[0]), ","));
        data.put(KEY_SPEED, Utils.implode(speedList.toArray(new String[0]), ","));
        data.put(KEY_ACCURACY, Utils.implode(accuracyList.toArray(new String[0]), ","));

        return data;
    }

    // --------------- Synchronized access to whole class ----------------------------
    public synchronized static void addPoint(Context context, DataPoint point) {
        DatabaseHelper db = new DatabaseHelper(context);
        db.addPoint(point);
    }

    public synchronized static void addPredictions(Context context, List<Double> lat, List<Double> lng, List<Double> bearing, List<Integer> time_to_wifi) {
        DatabaseHelper db = new DatabaseHelper(context);
        db.deletePredictions();
        db.addPredictions(lat,lng,bearing,time_to_wifi);
    }

    public synchronized static Map<String, String> popFew(Context context) {
        DatabaseHelper db = new DatabaseHelper(context);
        return db.popFew();
    }

    public synchronized static int getNumRows(Context context) {
        DatabaseHelper db = new DatabaseHelper(context);
        return db.getNumRows();
    }

    public synchronized static int predict(Context context, double lat, double lng, double bearing, double speed, double accuracy, double timestamp) {
        DatabaseHelper db = new DatabaseHelper(context);
        return db.predict(lat, lng, bearing, speed, accuracy, timestamp);
    }
}
