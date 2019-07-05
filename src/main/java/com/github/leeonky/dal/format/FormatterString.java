package com.github.leeonky.dal.format;

public class FormatterString implements Formatter<String> {
    @Override
    public Object toValue(String input) {
        return input;
    }
}
