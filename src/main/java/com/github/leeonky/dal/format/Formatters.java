package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.math.BigInteger;

public class Formatters {
    public static class FormatterString implements Formatter<String> {
        @Override
        public Object toValue(String input) {
            return input;
        }
    }

    public static class Instant implements Formatter<String> {
        @Override
        public Object toValue(String input) {
            return Formatter.toValueOrThrowIllegalTypeException(input, java.time.Instant::parse);
        }
    }

    public static class PositiveInteger implements Formatter<Number> {

        @Override
        public Object toValue(Number input) {
            String val = input.toString();
            if (val.chars().anyMatch(c -> !Character.isDigit(c)))
                throw new IllegalTypeException();
            BigInteger value = new BigInteger(val);
            if (value.compareTo(BigInteger.ZERO) <= 0)
                throw new IllegalTypeException();
            return value;
        }
    }

    public static class URL implements Formatter<String> {
        @Override
        public Object toValue(String input) {
            return Formatter.toValueOrThrowIllegalTypeException(input, java.net.URL::new);
        }
    }
}
