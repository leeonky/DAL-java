package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.net.MalformedURLException;

public class URL implements Formatter<String> {
    @Override
    public Object toValue(String input) {
        try {
            return new java.net.URL(input);
        } catch (MalformedURLException e) {
            throw new IllegalTypeException();
        }
    }
}
