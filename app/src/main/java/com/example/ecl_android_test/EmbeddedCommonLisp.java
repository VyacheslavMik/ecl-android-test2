package com.example.ecl_android_test;

import android.util.Log;

public class EmbeddedCommonLisp {
    private static String TAG = "EmbeddedCommonLisp";

    public void start() {
        start(System.getenv("user.dir"));
    }
    public native void start(String path);
    public native void exec(int method);

    static {
        System.loadLibrary("ecl");
        System.loadLibrary("app");
        Log.w(TAG,"Done loading library");
    }
}
