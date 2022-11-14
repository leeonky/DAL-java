package com.github.leeonky.dal.util;

import java.util.List;

import static java.util.Arrays.asList;

public class TextUtil {
    public static List<String> lines(String content) {
        return asList(content.split("\n\r|\r\n|\r|\n"));
    }

    public static String join(List<Character> characters) {
        StringBuilder builder = new StringBuilder();
        characters.forEach(builder::append);
        return builder.toString();
    }

    public static int differentPosition(String expected, String actual) {
        int minCount = Math.min(expected.length(), actual.length());
        int i = 0;
        while (i < minCount && expected.charAt(i) == actual.charAt(i))
            i++;
        return i;
    }
}
