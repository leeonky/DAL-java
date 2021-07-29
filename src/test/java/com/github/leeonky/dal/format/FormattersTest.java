package com.github.leeonky.dal.format;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FormattersTest {
    public static abstract class BaseNumber<R> extends BaseFormatter<Number, R> {
    }

    public static class IntegerEq5 extends BaseNumber<BigInteger> {
        @Override
        public BigInteger convert(Number input) {
            return null;
        }

        @Override
        public boolean isValidValue(BigInteger value) {
            return value.equals(new BigInteger("5"));
        }
    }

    @Nested
    class IntegerHelper {
        Formatters.Integer integer = Formatters.Integer.positive();

        @Test
        void positive() {
            integer = Formatters.Integer.positive();

            assertTrue(integer.isValidValue(new BigInteger("1")));
            assertFalse(integer.isValidValue(new BigInteger("0")));
            assertFalse(integer.isValidValue(new BigInteger("-1")));

            assertThat(integer.getFormatterName()).isEqualTo("Integer greater than [0]");
        }

        @Test
        void equal() {
            integer = Formatters.Integer.equalTo(1);

            assertTrue(integer.isValidValue(new BigInteger("1")));
            assertFalse(integer.isValidValue(new BigInteger("0")));

            assertThat(integer.getFormatterName()).isEqualTo("Integer equal to [1]");
        }

        @Test
        void greater() {
            integer = Formatters.Integer.greaterThan(1);

            assertTrue(integer.isValidValue(new BigInteger("2")));
            assertFalse(integer.isValidValue(new BigInteger("1")));
            assertFalse(integer.isValidValue(new BigInteger("0")));

            assertThat(integer.getFormatterName()).isEqualTo("Integer greater than [1]");
        }

        @Test
        void less() {
            integer = Formatters.Integer.lessThan(1);

            assertTrue(integer.isValidValue(new BigInteger("0")));
            assertFalse(integer.isValidValue(new BigInteger("1")));
            assertFalse(integer.isValidValue(new BigInteger("2")));

            assertThat(integer.getFormatterName()).isEqualTo("Integer less than [1]");
        }

        @Test
        void greater_or_equal() {
            integer = Formatters.Integer.greaterOrEqualTo(1);

            assertTrue(integer.isValidValue(new BigInteger("2")));
            assertTrue(integer.isValidValue(new BigInteger("1")));
            assertFalse(integer.isValidValue(new BigInteger("0")));

            assertThat(integer.getFormatterName()).isEqualTo("Integer greater or equal to [1]");
        }

        @Test
        void less_or_equal() {
            integer = Formatters.Integer.lessOrEqualTo(1);

            assertTrue(integer.isValidValue(new BigInteger("0")));
            assertTrue(integer.isValidValue(new BigInteger("1")));
            assertFalse(integer.isValidValue(new BigInteger("2")));

            assertThat(integer.getFormatterName()).isEqualTo("Integer less or equal to [1]");
        }
    }

    @Nested
    class NumberHelper {
        Formatters.Number positive = Formatters.Number.positive();

        @Test
        void positive() {
            positive = Formatters.Number.positive();

            assertTrue(positive.isValidValue(new BigDecimal("1")));
            assertFalse(positive.isValidValue(new BigDecimal("0")));
            assertFalse(positive.isValidValue(new BigDecimal("-1")));

            assertThat(positive.getFormatterName()).isEqualTo("Number greater than [0]");
        }

        @Test
        void equal() {
            positive = Formatters.Number.equalTo(1);

            assertTrue(positive.isValidValue(new BigDecimal("1")));
            assertFalse(positive.isValidValue(new BigDecimal("0")));

            assertThat(positive.getFormatterName()).isEqualTo("Number equal to [1]");
        }

        @Test
        void greater() {
            positive = Formatters.Number.greaterThan(1);

            assertTrue(positive.isValidValue(new BigDecimal("2")));
            assertFalse(positive.isValidValue(new BigDecimal("1")));
            assertFalse(positive.isValidValue(new BigDecimal("0")));

            assertThat(positive.getFormatterName()).isEqualTo("Number greater than [1]");
        }

        @Test
        void less() {
            positive = Formatters.Number.lessThan(1);

            assertTrue(positive.isValidValue(new BigDecimal("0")));
            assertFalse(positive.isValidValue(new BigDecimal("1")));
            assertFalse(positive.isValidValue(new BigDecimal("2")));

            assertThat(positive.getFormatterName()).isEqualTo("Number less than [1]");
        }

        @Test
        void greater_or_equal() {
            positive = Formatters.Number.greaterOrEqualTo(1);

            assertTrue(positive.isValidValue(new BigDecimal("2")));
            assertTrue(positive.isValidValue(new BigDecimal("1")));
            assertFalse(positive.isValidValue(new BigDecimal("0")));

            assertThat(positive.getFormatterName()).isEqualTo("Number greater or equal to [1]");
        }

        @Test
        void less_or_equal() {
            positive = Formatters.Number.lessOrEqualTo(1);

            assertTrue(positive.isValidValue(new BigDecimal("0")));
            assertTrue(positive.isValidValue(new BigDecimal("1")));
            assertFalse(positive.isValidValue(new BigDecimal("2")));

            assertThat(positive.getFormatterName()).isEqualTo("Number less or equal to [1]");
        }
    }

    @Nested
    class Formatter {

        @Test
        void guess_type_from_nested_generic_type_args() {
            assertThat(BaseFormatter.guessInputType(IntegerEq5.class))
                    .isEqualTo(Number.class);
        }
    }
}