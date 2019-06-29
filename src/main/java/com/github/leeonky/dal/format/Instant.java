package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

public class Instant implements Formatter<String> {
    @Override
    public Object toValue(String input) {
        try {
            return java.time.Instant.parse(input);
        } catch (Exception e) {
            throw new IllegalTypeException();
        }
    }
}
