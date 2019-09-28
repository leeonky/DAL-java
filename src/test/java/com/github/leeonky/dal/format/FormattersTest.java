package com.github.leeonky.dal.format;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FormattersTest {

    @Nested
    class IntegerHelper {
        Formatters.Integer positive = Formatters.Integer.positive();

        @Test
        void positive() {
            positive = Formatters.Integer.positive();

            assertTrue(positive.isValidValue(new BigInteger("1")));
            assertFalse(positive.isValidValue(new BigInteger("0")));
            assertFalse(positive.isValidValue(new BigInteger("-1")));

            assertThat(positive.getFormatterName()).isEqualTo("Integer greater than [0]");
        }

        @Test
        void equal() {
            positive = Formatters.Integer.equalTo(1);

            assertTrue(positive.isValidValue(new BigInteger("1")));
            assertFalse(positive.isValidValue(new BigInteger("0")));

            assertThat(positive.getFormatterName()).isEqualTo("Integer equal to [1]");
        }

        @Test
        void greater() {
            positive = Formatters.Integer.greaterThan(1);

            assertTrue(positive.isValidValue(new BigInteger("2")));
            assertFalse(positive.isValidValue(new BigInteger("1")));
            assertFalse(positive.isValidValue(new BigInteger("0")));

            assertThat(positive.getFormatterName()).isEqualTo("Integer greater than [1]");
        }

        @Test
        void less() {
            positive = Formatters.Integer.lessThan(1);

            assertTrue(positive.isValidValue(new BigInteger("0")));
            assertFalse(positive.isValidValue(new BigInteger("1")));
            assertFalse(positive.isValidValue(new BigInteger("2")));

            assertThat(positive.getFormatterName()).isEqualTo("Integer less than [1]");
        }

        @Test
        void greater_or_equal() {
            positive = Formatters.Integer.greaterOrEqualTo(1);

            assertTrue(positive.isValidValue(new BigInteger("2")));
            assertTrue(positive.isValidValue(new BigInteger("1")));
            assertFalse(positive.isValidValue(new BigInteger("0")));

            assertThat(positive.getFormatterName()).isEqualTo("Integer greater or equal to [1]");
        }

        @Test
        void less_or_equal() {
            positive = Formatters.Integer.lessOrEqualTo(1);

            assertTrue(positive.isValidValue(new BigInteger("0")));
            assertTrue(positive.isValidValue(new BigInteger("1")));
            assertFalse(positive.isValidValue(new BigInteger("2")));

            assertThat(positive.getFormatterName()).isEqualTo("Integer less or equal to [1]");
        }
    }
}