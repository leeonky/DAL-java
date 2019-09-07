package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.format.DateTimeParseException;

import static java.lang.Enum.valueOf;

public class Formatters {
    public static class String implements Formatter<java.lang.String> {
        @Override
        public Object toValue(java.lang.String input) {
            return input;
        }
    }

    public static class Instant implements Formatter<java.lang.String> {
        @Override
        public Object toValue(java.lang.String input) {
            return Formatter.toValueOrThrowIllegalTypeException(input, java.time.Instant::parse);
        }
    }

    public static class PositiveInteger extends Integer {

        @Override
        public Object toValue(java.lang.Number input) {
            BigInteger value = (BigInteger) super.toValue(input);
            if (value.compareTo(BigInteger.ZERO) <= 0)
                throw new IllegalTypeException();
            return value;
        }
    }

    public static class Integer implements Formatter<java.lang.Number> {

        @Override
        public boolean isValidType(Object input) {
            return input instanceof java.lang.Number;
        }

        @Override
        public Object toValue(java.lang.Number input) {
            if (input instanceof Double
                    || input instanceof Float
                    || (input instanceof BigDecimal && ((BigDecimal) input).scale() != 0)) {
                throw new IllegalTypeException();
            }
            return new BigInteger(input.toString());
        }
    }

    public static class URL implements Formatter<java.lang.String> {
        @Override
        public Object toValue(java.lang.String input) {
            return Formatter.toValueOrThrowIllegalTypeException(input, java.net.URL::new);
        }
    }

    public static class Enum<T extends java.lang.Enum<T>> implements Formatter<java.lang.String> {
        private final Class<T> enumType;

        public Enum() {
            this(null);
        }

        public Enum(Class<T> enumType) {
            this.enumType = enumType;
        }

        @Override
        public Object toValue(java.lang.String input) {
            return enumType == null ? defaultVerification(input) : verifyViaEnumType(input);
        }

        private Object verifyViaEnumType(java.lang.String input) {
            try {
                return valueOf(enumType, input);
            } catch (Exception e) {
                throw new IllegalTypeException();
            }
        }

        private Object defaultVerification(java.lang.String input) {
            if (input.chars().filter(Character::isLetter)
                    .anyMatch(Character::isLowerCase))
                throw new IllegalTypeException();
            return input;
        }
    }

    public static class Number implements Formatter<java.lang.Number> {

        @Override
        public Object toValue(java.lang.Number input) {
            return input;
        }
    }

    public static class PositiveNumber implements Formatter<java.lang.Number> {

        @Override
        public Object toValue(java.lang.Number input) {
            if (new BigDecimal(input.toString()).compareTo(BigDecimal.ZERO) <= 0)
                throw new IllegalTypeException();
            return input;
        }
    }

    public static class ZeroNumber implements Formatter<java.lang.Number> {

        @Override
        public Object toValue(java.lang.Number input) {
            if (new BigDecimal(input.toString()).compareTo(BigDecimal.ZERO) != 0)
                throw new IllegalTypeException();
            return input;
        }
    }

    public static class LocalDate implements Formatter<java.lang.String> {

        @Override
        public Object toValue(java.lang.String input) {
            try {
                return java.time.LocalDate.parse(input);
            } catch (DateTimeParseException ignore) {
                throw new IllegalTypeException();
            }
        }
    }

    public static class LocalDateTime implements Formatter<java.lang.String> {
        @Override
        public Object toValue(java.lang.String input) {
            try {
                return java.time.LocalDateTime.parse(input);
            } catch (DateTimeParseException ignore) {
                throw new IllegalTypeException();
            }
        }
    }

    public static class Boolean implements Formatter<java.lang.Boolean> {
        @Override
        public Object toValue(java.lang.Boolean input) {
            return input;
        }
    }
}
