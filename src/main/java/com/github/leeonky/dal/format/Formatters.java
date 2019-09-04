package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.math.BigDecimal;
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

    public static class PositiveInteger extends Integer {

        @Override
        public Object toValue(Number input) {
            BigInteger value = (BigInteger) super.toValue(input);
            if (value.compareTo(BigInteger.ZERO) <= 0)
                throw new IllegalTypeException();
            return value;
        }
    }

    public static class Integer implements Formatter<Number> {

        @Override
        public boolean isValidType(Object input) {
            return input instanceof Number;
        }

        @Override
        public Object toValue(Number input) {
            if (input instanceof Double
                    || input instanceof Float
                    || (input instanceof BigDecimal && ((BigDecimal) input).scale() != 0)) {
                throw new IllegalTypeException();
            }
            return new BigInteger(input.toString());
        }
    }

    public static class URL implements Formatter<String> {
        @Override
        public Object toValue(String input) {
            return Formatter.toValueOrThrowIllegalTypeException(input, java.net.URL::new);
        }
    }
}
