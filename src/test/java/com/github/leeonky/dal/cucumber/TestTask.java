package com.github.leeonky.dal.cucumber;

public class TestTask {
    public static int threadsCount(String env, int defaultValue) {
        String value = System.getenv(env);
        if (value == null)
            return defaultValue;
        return Integer.parseInt(value);
    }
}
