package com.github.leeonky.dal.format;

import com.github.leeonky.dal.runtime.IllegalTypeException;
import com.github.leeonky.util.function.Comparator;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.format.DateTimeParseException;

import static java.lang.Enum.valueOf;
import static java.lang.String.format;

public class Formatters {
    /**
     * Use com.github.leeonky.dal.format.Type
     */
    @Deprecated
    public static class String extends BaseFormatter<java.lang.String, java.lang.String> {
        @Override
        public java.lang.String convert(java.lang.String input) {
            return input;
        }
    }

    public static class Instant extends BaseFormatter<java.lang.String, java.time.Instant> {

        public static Instant now(int errorMs) {
            return new Instant() {
                private java.time.Instant now;

                @Override
                public boolean isValidValue(java.time.Instant actual) {
                    now = java.time.Instant.now();
                    return actual.isAfter(now.plusMillis(-errorMs)) && actual.isBefore(now.plusMillis(errorMs));
                }

                @Override
                public java.lang.String getFormatterName() {
                    return format("Instant now[%s] +/- %dms", now, errorMs);
                }
            };
        }

        public static Instant now() {
            return now(10000);
        }

        @Override
        public java.time.Instant convert(java.lang.String input) {
            return BaseFormatter.toValueOrThrowIllegalTypeException(input, java.time.Instant::parse);
        }
    }

    public static class PositiveInteger extends Integer {
        @Override
        public BigInteger convert(java.lang.Number input) {
            BigInteger value = super.convert(input);
            if (value.compareTo(BigInteger.ZERO) <= 0)
                throw new IllegalTypeException();
            return value;
        }
    }

    //TODO  keep origin type, 1.0 => should integer
    public static class Integer extends BaseFormatter<java.lang.Number, BigInteger> {
        public static Integer equalTo(long expect) {
            return compare(expect, Comparator.equalTo(0), "equal to");
        }

        public static Integer positive() {
            return greaterThan(0);
        }

        public static Integer greaterThan(long expect) {
            return compare(expect, Comparator.greaterThan(0), "greater than");
        }

        public static Integer lessThan(long expect) {
            return compare(expect, Comparator.lessThan(0), "less than");
        }

        public static Integer negative() {
            return lessThan(0);
        }

        public static Integer greaterOrEqualTo(long expect) {
            return compare(expect, Comparator.greaterOrEqualTo(0), "greater or equal to");
        }

        public static Integer lessOrEqualTo(long expect) {
            return compare(expect, Comparator.lessOrEqualTo(0), "less or equal to");
        }

        public static Integer compare(long expect, Comparator<java.lang.Integer> comparator,
                                      java.lang.String formatterName) {
            return new Integer() {
                @Override
                public boolean isValidValue(BigInteger value) {
                    return comparator.compareTo(value.compareTo(BigInteger.valueOf(expect)));
                }

                @Override
                public java.lang.String getFormatterName() {
                    return format("Integer %s [%d]", formatterName, expect);
                }
            };
        }

        @Override
        public BigInteger convert(java.lang.Number input) {
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
        public java.net.URL convert(java.lang.String input) {
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
        public T convert(java.lang.String input) {
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

    public static class Number extends BaseFormatter<java.lang.Number, java.math.BigDecimal> {

        public static Number equalTo(java.lang.Number expect) {
            return compare(expect, Comparator.equalTo(0), "equal to");
        }

        public static Number compare(java.lang.Number expect, Comparator<java.lang.Integer> comparator,
                                     java.lang.String formatterName) {
            return new Number() {
                @Override
                public boolean isValidValue(BigDecimal value) {
                    return comparator.compareTo(value.compareTo(new BigDecimal(expect.toString())));
                }

                @Override
                public java.lang.String getFormatterName() {
                    return format("Number %s [%s]", formatterName, expect);
                }
            };
        }

        public static Number positive() {
            return greaterThan(0);
        }

        public static Number greaterThan(java.lang.Number expect) {
            return compare(expect, Comparator.greaterThan(0), "greater than");
        }

        public static Number negative() {
            return lessThan(0);
        }

        public static Number lessThan(java.lang.Number expect) {
            return compare(expect, Comparator.lessThan(0), "less than");
        }

        public static Number greaterOrEqualTo(java.lang.Number expect) {
            return compare(expect, Comparator.greaterOrEqualTo(0), "greater or equal to");
        }

        public static Number lessOrEqualTo(java.lang.Number expect) {
            return compare(expect, Comparator.lessOrEqualTo(0), "less or equal to");
        }

        @Override
        public java.math.BigDecimal convert(java.lang.Number input) {
            return new BigDecimal(input.toString());
        }
    }

    public static class PositiveNumber extends BaseFormatter<java.lang.Number, BigDecimal> {

        @Override
        public BigDecimal convert(java.lang.Number input) {
            BigDecimal decimal = new BigDecimal(input.toString());
            if (decimal.compareTo(BigDecimal.ZERO) <= 0)
                throw new IllegalTypeException();
            return decimal;
        }
    }

    public static class ZeroNumber extends BaseFormatter<java.lang.Number, java.lang.Integer> {

        @Override
        public java.lang.Integer convert(java.lang.Number input) {
            if (new BigDecimal(input.toString()).compareTo(BigDecimal.ZERO) != 0)
                throw new IllegalTypeException();
            return 0;
        }
    }

    public static class LocalDate extends BaseFormatter<java.lang.String, java.time.LocalDate> {

        @Override
        public java.time.LocalDate convert(java.lang.String input) {
            try {
                return java.time.LocalDate.parse(input);
            } catch (DateTimeParseException ignore) {
                throw new IllegalTypeException();
            }
        }
    }

    public static class LocalDateTime extends BaseFormatter<java.lang.String, java.time.LocalDateTime> {

        @Override
        public java.time.LocalDateTime convert(java.lang.String input) {
            try {
                return java.time.LocalDateTime.parse(input);
            } catch (DateTimeParseException ignore) {
                throw new IllegalTypeException();
            }
        }
    }

    public static class Boolean extends BaseFormatter<java.lang.Boolean, java.lang.Boolean> {
        @Override
        public java.lang.Boolean convert(java.lang.Boolean input) {
            return input;
        }
    }
}
