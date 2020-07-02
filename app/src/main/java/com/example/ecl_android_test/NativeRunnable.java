package com.example.ecl_android_test;

public class NativeRunnable implements Runnable {

    private int m_method;

    public NativeRunnable(int method)
    {
        m_method = method;
    }

    @Override
    public void run() {
        HelloEclActivity.getInstance().ECL.exec(m_method);
    }
}
