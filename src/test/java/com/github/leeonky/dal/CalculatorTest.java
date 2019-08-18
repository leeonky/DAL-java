package com.github.leeonky.dal;

import com.github.leeonky.dal.util.Calculator;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class CalculatorTest {
    private void assertCompare(Object v1, Object v2, int result) {
        assertThat(Calculator.compare(v1, v2)).isEqualTo(result);
    }

    private void assertIllegalArgument(Executable executable, String message) {
        IllegalArgumentException illegalArgumentException = assertThrows(IllegalArgumentException.class, executable);
        assertThat(illegalArgumentException).hasMessage(message);
    }

    @Nested
    class NumberCompare {

        @Test
        void all_params_should_not_be_null() {
            assertIllegalArgument(() -> Calculator.compare(1, null), "Can not compare [1] and [null]");
            assertIllegalArgument(() -> Calculator.compare(null, null), "Can not compare [null] and [null]");
            assertIllegalArgument(() -> Calculator.compare(null, 1), "Can not compare [null] and [1]");
        }

        @Test
        void compare_number_with_three_result() {
            assertCompare(1, 1, 0);
            assertCompare(1, 0, 1);
            assertCompare(0, 1, -1);
        }

        @Test
        void compare_number_in_different_number_type() {
            assertCompare(1, 1.0, 0);
            assertCompare(1, (byte) 1, 0);
            assertCompare(1, (short) (byte) 1, 0);
            assertCompare(1, (long) (byte) 1, 0);
            assertCompare(1, new BigDecimal(1), 0);
            assertCompare(1, new BigInteger("1"), 0);
            assertCompare(1, 0, 1);
        }

        @Test
        void do_not_allow_compare_in_different_type() {
            assertIllegalArgument(() -> Calculator.compare(1, "1"), "Can not compare [java.lang.Integer: 1] and [java.lang.String: 1]");
        }
    }

    @Nested
    class StringCompare {

        @Test
        void compare_string() {
            assertCompare("a", "a", 0);
            assertCompare("b", "a", 1);
            assertCompare("a", "b", -1);
        }
    }

    @Nested
    class Plus {

        @Test
        void plus_number_in_different_number_type() {
            assertThat(Calculator.plus(1, 1L)).isEqualTo(new BigDecimal(2));
        }

        @Test
        void plus_object_and_string_should_call_to_string_of_object() {
            assertThat(Calculator.plus(1, "")).isEqualTo("1");
            assertThat(Calculator.plus("", 1)).isEqualTo("1");
        }

        @Test
        void should_raise_error_when_input_object_type_does_not_suit_for_plus() {
            assertIllegalArgument(() -> Calculator.plus(true, 1), "Can not plus 'java.lang.Boolean' and 'java.lang.Integer'");
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
        void number_equal_in_different_number_type() {
            assertTrue(Calculator.equals(1, 1L));
        }

        @Test
        void string_equals() {
            assertTrue(Calculator.equals("a", "a"));
            assertFalse(Calculator.equals("a", "b"));
        }

        @Test
        void number_equals() {
            assertTrue(Calculator.equals(1, 1L));
            assertTrue(Calculator.equals(1, 1.0));
            assertTrue(Calculator.equals(0, 0.0));
            assertFalse(Calculator.equals(1, 2L));
        }

        @Test
        void should_raise_error_when_type_not_matched() {
            assertIllegalArgument(() -> Calculator.equals("a", 1), "Can not compare 'java.lang.String' and 'java.lang.Integer'");
        }
    }

    @Nested
    class Multi {

        @Test
        void support_calculate_in_different_number_type() {
            assertThat(Calculator.multiply(1, 2L)).isEqualTo(new BigDecimal(2));
        }

        @Test
        void all_input_number_should_number_type() {
            assertIllegalArgument(() -> Calculator.multiply("2", "4"), "Operands should be number but 'java.lang.String' and 'java.lang.String'");
        }
    }

    @Nested
    class Div {

        @Test
        void support_calculate_in_different_number_type() {
            assertThat(Calculator.divide(8, 2)).isEqualTo(new BigDecimal(4));
        }

        @Test
        void all_input_number_should_number_type() {
            assertIllegalArgument(() -> Calculator.divide("2", "4"), "Operands should be number but 'java.lang.String' and 'java.lang.String'");
        }
    }

    @Nested
    class Sub {

        @Test
        void support_calculate_in_different_number_type() {
            assertThat(Calculator.subtract(4, 1)).isEqualTo(new BigDecimal(3));
        }

        @Test
        void all_input_number_should_number_type() {
            assertIllegalArgument(() -> Calculator.subtract("2", "4"), "Operands should be number but 'java.lang.String' and 'java.lang.String'");
        }
    }

    @Nested
    class Negate {

        @Test
        void support_all_number_type() {
            assertThat(Calculator.negate(1)).isEqualTo(new BigDecimal(-1));
            assertThat(Calculator.negate(1L)).isEqualTo(new BigDecimal(-1));
            assertThat(Calculator.negate(1.0)).isEqualTo(new BigDecimal("-1.0"));
        }

        @Test
        void should_raise_error_when_negate_non_number_types() {
            assertIllegalArgument(() -> Calculator.negate("2"), "Operands should be number but 'java.lang.String'");
        }
    }

    @Nested
    class Logical {

        @Test
        void logical_calculate() {
            assertThat(Calculator.and(() -> true, () -> false)).isEqualTo(false);
            assertThat(Calculator.and(() -> true, () -> true)).isEqualTo(true);
            assertThat(Calculator.and(() -> false, () -> true)).isEqualTo(false);
            assertThat(Calculator.and(() -> false, () -> false)).isEqualTo(false);

            assertThat(Calculator.or(() -> true, () -> false)).isEqualTo(true);
            assertThat(Calculator.or(() -> true, () -> true)).isEqualTo(true);
            assertThat(Calculator.or(() -> false, () -> true)).isEqualTo(true);
            assertThat(Calculator.or(() -> false, () -> false)).isEqualTo(false);

            assertThat(Calculator.not(true)).isEqualTo(false);
            assertThat(Calculator.not(false)).isEqualTo(true);
        }

        @Test
        void input_value_should_be_logical_type() {
            assertIllegalArgument(() -> Calculator.and(() -> "2", () -> true), "Operand 1 should be boolean but 'java.lang.String'");
            assertIllegalArgument(() -> Calculator.and(() -> true, () -> 1), "Operand 2 should be boolean but 'java.lang.Integer'");
            assertIllegalArgument(() -> Calculator.or(() -> "2", () -> true), "Operand 1 should be boolean but 'java.lang.String'");
            assertIllegalArgument(() -> Calculator.or(() -> false, () -> 1), "Operand 2 should be boolean but 'java.lang.Integer'");
            assertIllegalArgument(() -> Calculator.not(1), "Operand should be boolean but 'java.lang.Integer'");
        }
    }
}