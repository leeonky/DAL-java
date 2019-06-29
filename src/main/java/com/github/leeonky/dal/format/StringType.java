package com.github.leeonky.dal.format;

public class StringType implements Formatter<String> {
    @Override
    public Object toValue(String input) {
        return input;
    }
}
