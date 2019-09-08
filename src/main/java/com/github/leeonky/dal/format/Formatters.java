package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.format.DateTimeParseException;

import static java.lang.Enum.valueOf;

public class Formatters {
    public static class String extends BaseFormatter<java.lang.String, java.lang.String> {
        @Override
        public java.lang.String toValue(java.lang.String input) {
            return input;
        }
    }

    public static class Instant extends BaseFormatter<java.lang.String, java.time.Instant> {
        public static Instant equalTo(java.time.Instant expect) {
            return new Instant() {
                @Override
                public boolean verify(java.time.Instant actual) {
                    return expect.equals(actual);
                }

                @Override
                public java.lang.String getFormatterName() {
                    return java.lang.String.format("Instant equalTo [%s]", expect);
                }
            };
        }

        @Override
        public java.time.Instant toValue(java.lang.String input) {
            return BaseFormatter.toValueOrThrowIllegalTypeException(input, java.time.Instant::parse);
        }
    }

    public static class PositiveInteger extends Integer {
        public static PositiveInteger equalTo(long expect) {
            return new PositiveInteger() {
                @Override
                public boolean verify(BigInteger value) {
                    return value.compareTo(BigInteger.valueOf(expect)) == 0;
                }
            };
        }

        @Override
        public BigInteger toValue(java.lang.Number input) {
            BigInteger value = super.toValue(input);
            if (value.compareTo(BigInteger.ZERO) <= 0)
                throw new IllegalTypeException();
            return value;
        }
    }

    public static class Integer extends BaseFormatter<java.lang.Number, BigInteger> {

        @Override
        public BigInteger toValue(java.lang.Number input) {
            if (input instanceof Double
                    || input instanceof Float
                    || (input instanceof BigDecimal && ((BigDecimal) input).scale() != 0)) {
                throw new IllegalTypeException();
            }
            return new BigInteger(input.toString());
        }
    }

    public static class URL extends BaseFormatter<java.lang.String, java.net.URL> {

        @Override
        public java.net.URL toValue(java.lang.String input) {
            return BaseFormatter.toValueOrThrowIllegalTypeException(input, java.net.URL::new);
        }
    }

    public static class Enum<T extends java.lang.Enum<T>> extends BaseFormatter<java.lang.String, T> {
        private final Class<T> enumType;

        public Enum() {
            this(null);
        }

        public Enum(Class<T> enumType) {
            this.enumType = enumType;
        }

        @Override
        public T toValue(java.lang.String input) {
            return enumType == null ? defaultVerification(input) : verifyViaEnumType(input);
        }

        private T verifyViaEnumType(java.lang.String input) {
            try {
                return valueOf(enumType, input);
            } catch (Exception e) {
                throw new IllegalTypeException();
            }
        }

        private T defaultVerification(java.lang.String input) {
            if (input.chars().filter(Character::isLetter)
                    .anyMatch(Character::isLowerCase))
                throw new IllegalTypeException();
            return null;
        }
    }

    public static class Number extends BaseFormatter<java.lang.Number, java.lang.Number> {

        @Override
        public java.lang.Number toValue(java.lang.Number input) {
            return input;
        }
    }

    public static class PositiveNumber extends BaseFormatter<java.lang.Number, BigDecimal> {

        @Override
        public BigDecimal toValue(java.lang.Number input) {
            BigDecimal decimal = new BigDecimal(input.toString());
            if (decimal.compareTo(BigDecimal.ZERO) <= 0)
                throw new IllegalTypeException();
            return decimal;
        }
    }

    public static class ZeroNumber extends BaseFormatter<java.lang.Number, java.lang.Integer> {

        @Override
        public java.lang.Integer toValue(java.lang.Number input) {
            if (new BigDecimal(input.toString()).compareTo(BigDecimal.ZERO) != 0)
                throw new IllegalTypeException();
            return 0;
        }
    }

    public static class LocalDate extends BaseFormatter<java.lang.String, java.time.LocalDate> {

        @Override
        public java.time.LocalDate toValue(java.lang.String input) {
            try {
                return java.time.LocalDate.parse(input);
            } catch (DateTimeParseException ignore) {
                throw new IllegalTypeException();
            }
        }
    }

    public static class LocalDateTime extends BaseFormatter<java.lang.String, java.time.LocalDateTime> {

        @Override
        public java.time.LocalDateTime toValue(java.lang.String input) {
            try {
                return java.time.LocalDateTime.parse(input);
            } catch (DateTimeParseException ignore) {
                throw new IllegalTypeException();
            }
        }
    }

    public static class Boolean extends BaseFormatter<java.lang.Boolean, java.lang.Boolean> {
        @Override
        public java.lang.Boolean toValue(java.lang.Boolean input) {
            return input;
        }
    }
}
