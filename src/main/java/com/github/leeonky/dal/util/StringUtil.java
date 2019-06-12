package com.github.leeonky.dal.util;

public class StringUtil {
    public static String capitalize(String str) {
        return str.isEmpty() ? str : str.toUpperCase().substring(0, 1) + str.substring(1);
    }

    public static String unCapitalize(String str) {
        return str.isEmpty() ? str : str.toLowerCase().substring(0, 1) + str.substring(1);
    }
}
