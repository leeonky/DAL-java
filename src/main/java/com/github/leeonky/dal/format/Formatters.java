package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.format.DateTimeParseException;
import java.util.Objects;

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
                    return java.lang.String.format("Instant equal to [%s]", expect);
                }
            };
        }

        public static Instant equalTo(java.lang.String expect) {
            return equalTo(java.time.Instant.parse(expect));
        }

        public static Instant now(int errorMs) {
            return new Instant() {
                private java.time.Instant now;

                @Override
                public boolean verify(java.time.Instant actual) {
                    now = java.time.Instant.now();
                    return actual.isAfter(now.plusMillis(-errorMs)) && actual.isBefore(now.plusMillis(errorMs));
                }

                @Override
                public java.lang.String getFormatterName() {
                    return java.lang.String.format("Instant now[%s] +/- %dms", now, errorMs);
                }
            };
        }

        public static Instant now() {
            return now(10000);
        }

        @Override
        public java.time.Instant toValue(java.lang.String input) {
            return BaseFormatter.toValueOrThrowIllegalTypeException(input, java.time.Instant::parse);
        }
    }

    @Deprecated
    public static class PositiveInteger extends Integer {
        @Override
        public BigInteger toValue(java.lang.Number input) {
            BigInteger value = super.toValue(input);
            if (value.compareTo(BigInteger.ZERO) <= 0)
                throw new IllegalTypeException();
            return value;
        }
    }

    public static class Integer extends BaseFormatter<java.lang.Number, BigInteger> {
        public static Integer equalTo(long expect) {
            return new Integer() {
                @Override
                public boolean verify(BigInteger value) {
                    return value.compareTo(BigInteger.valueOf(expect)) == 0;
                }

                @Override
                public java.lang.String getFormatterName() {
                    return java.lang.String.format("Integer equal to [%d]", expect);
                }
            };
        }

        public static Integer positive() {
            return greaterThan(0);
        }

        private static Integer greaterThan(long expect) {
            return new Integer() {
                @Override
                public boolean verify(BigInteger value) {
                    return value.compareTo(BigInteger.valueOf(expect)) > 0;
                }

                @Override
                public java.lang.String getFormatterName() {
                    return java.lang.String.format("Integer greater than %d", expect);
                }
            };
        }

        private static Integer lessThan(long expect) {
            return new Integer() {
                @Override
                public boolean verify(BigInteger value) {
                    return value.compareTo(BigInteger.valueOf(expect)) < 0;
                }

                @Override
                public java.lang.String getFormatterName() {
                    return java.lang.String.format("Integer less than %d", expect);
                }
            };
        }

        public static Integer negative() {
            return lessThan(0);
        }

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

        @SuppressWarnings("unchecked")
        public static <E extends java.lang.Enum<E>> Enum<E> equalTo(E expect) {
            return new Enum<E>((Class<E>) expect.getClass()) {
                @Override
                public boolean verify(E actual) {
                    return Objects.equals(expect, actual);
                }

                @Override
                public java.lang.String getFormatterName() {
                    return java.lang.String.format("Enum equal to %s", expect);
                }
            };
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

    @Deprecated
    public static class PositiveNumber extends BaseFormatter<java.lang.Number, BigDecimal> {

        @Override
        public BigDecimal toValue(java.lang.Number input) {
            BigDecimal decimal = new BigDecimal(input.toString());
            if (decimal.compareTo(BigDecimal.ZERO) <= 0)
                throw new IllegalTypeException();
            return decimal;
        }
    }

    @Deprecated
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
