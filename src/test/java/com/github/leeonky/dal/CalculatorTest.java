package com.github.leeonky.dal;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class CalculatorTest {
    private void assertCompare(Object v1, Object v2, int result) {
        assertThat(Calculator.compare(v1, v2)).isEqualTo(result);
    }

    @Nested
    class NumberCompare {

        @Test
        void all_params_should_not_be_null() {
            IllegalArgumentException illegalArgumentException = assertThrows(IllegalArgumentException.class, () -> Calculator.compare(1, null));
            assertThat(illegalArgumentException).hasMessage("Can not compare <1> and <null>");

            illegalArgumentException = assertThrows(IllegalArgumentException.class, () -> Calculator.compare(null, null));
            assertThat(illegalArgumentException).hasMessage("Can not compare <null> and <null>");

            illegalArgumentException = assertThrows(IllegalArgumentException.class, () -> Calculator.compare(null, 1));
            assertThat(illegalArgumentException).hasMessage("Can not compare <null> and <1>");
        }

        @Test
        void compare_number_in_different_type() {
            assertCompare(1, 1, 0);
            assertCompare(1, (byte) 1, 0);
            assertCompare(1, (short) (byte) 1, 0);
            assertCompare(1, (long) (byte) 1, 0);
            assertCompare(1, new BigDecimal(1), 0);
            assertCompare(1, new BigInteger("1"), 0);
            assertCompare(1, 0, 1);
        }

        @Test
        void should_check_type_before_compare() {
            IllegalArgumentException illegalArgumentException = assertThrows(IllegalArgumentException.class, () -> Calculator.compare(1, "1"));
            assertThat(illegalArgumentException).hasMessage("Can not compare <java.lang.Integer: 1> and <java.lang.String: 1>");
        }
    }

    @Nested
    class StringCompare {

        @Test
        void compare_string() {
            assertCompare("a", "a", 0);
            assertCompare("b", "a", 1);
        }
    }

    @Nested
    class Equal {
        @Test
        void both_null_is_equal() {
            assertTrue(Calculator.equals(null, null));
        }

        @Test
        void null_is_not_equal_to_not_null() {
            assertFalse(Calculator.equals(null, 1));
            assertFalse(Calculator.equals(1, null));
        }

        @Test
        void number_equal() {
            assertTrue(Calculator.equals(1, 1L));
        }

        @Test
        void other_type_equal() {
            assertTrue(Calculator.equals("a", "a"));
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> Calculator.equals("a", 1));
            assertThat(exception).hasMessage("Can not compare java.lang.String and java.lang.Integer");
        }
    }
}