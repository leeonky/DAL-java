package com.github.leeonky.dal;

import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.IllegalOperationException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class CalculatorTest {
    private RuntimeContextBuilder.DALRuntimeContext context = new RuntimeContextBuilder().build(null);

    private void assertCompare(Object v1, Object v2, int result) {
        assertThat(Calculator.compare(v1, v2, context)).isEqualTo(result);
    }

    private void assertIllegalArgument(Executable executable, String message) {
        IllegalOperationException exception = assertThrows(IllegalOperationException.class, executable);
        assertThat(exception).hasMessage(message);
    }

    @Nested
    class NumberCompare {

        @Test
        void compare_in_same_type() {
            assertCompare(1, 1, 0);
            assertCompare(1, 0, 1);
            assertCompare(0, 1, -1);
        }

        @Test
        void compare_in_different_number_type() {
            assertCompare(1, 1L, 0);
            assertCompare(1, BigInteger.ZERO, 1);
            assertCompare(BigDecimal.ZERO, 1, -1);
        }

        @Test
        void all_params_should_not_be_null() {
            assertIllegalArgument(() -> Calculator.compare(1, null, context), "Can not compare [1] and [null]");
            assertIllegalArgument(() -> Calculator.compare(null, null, context), "Can not compare [null] and [null]");
            assertIllegalArgument(() -> Calculator.compare(null, 1, context), "Can not compare [null] and [1]");
        }

        @Test
        void do_not_allow_compare_in_different_type() {
            assertIllegalArgument(() -> Calculator.compare(1, "1", context), "Can not compare [java.lang.Integer: 1] and [java.lang.String: 1]");
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

        @Nested
        class Number {

            @Test
            void plus_in_same_type() {
                assertThat(Calculator.plus(1, 1, context)).isEqualTo(2);
                assertThat(Calculator.plus(BigInteger.valueOf(1), BigInteger.valueOf(1), context)).isEqualTo(BigInteger.valueOf(2));
            }

            @Test
            void plus_number_in_different_type() {
                assertThat(Calculator.plus(1, 1L, context)).isEqualTo(2L);
            }
        }

        @Nested
        class _String {

            @Test
            void plus_object_and_string_should_call_to_string_of_object() {
                assertThat(Calculator.plus(1, "", context)).isEqualTo("1");
                assertThat(Calculator.plus("", 1, context)).isEqualTo("1");
            }
        }

        @Test
        void should_raise_error_when_input_object_type_does_not_suit_for_plus() {
            assertIllegalArgument(() -> Calculator.plus(true, 1, context),
                    "Can not plus 'java.lang.Boolean' and 'java.lang.Integer'");
        }
    }

    @Nested
    class Equal {
        private final RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new DAL().getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .build(null);

        @Test
        void both_null_is_equal() {
            assertEqual(null, null);
        }

        @Test
        void null_is_not_equal_to_not_null() {
            assertNotEqual(null, 1);
            assertNotEqual(1, null);
        }

        @Test
        void string_equals() {
            assertEqual("a", "a");
            assertNotEqual("a", "b");
        }

        @Test
        void number_equals() {
            assertEqual(1, 1);
        }

        @Test
        void number_equal_in_different_number_type() {
            assertNotEqual(1, 2);
            assertNotEqual(1, 1L);
            assertNotEqual(1, 1.0);
            assertNotEqual(0, 0.0);
        }

        @Test
        void customized_null() {
            assertEqual(null, JSONObject.NULL);
            assertEqual(JSONObject.NULL, null);
        }

        @Test
        void list() {
            assertEqual(new String[0], new String[0]);
        }

        private void assertNotEqual(Object v1, Object v2) {
            assertFalse(Calculator.equals(DALRuntimeContext.wrap(v1), DALRuntimeContext.wrap(v2)));
        }

        private void assertEqual(Object value1, Object value2) {
            assertTrue(Calculator.equals(DALRuntimeContext.wrap(value1), DALRuntimeContext.wrap(value2)));
        }
    }

    @Nested
    class Multi {

        @Test
        void support_calculate_in_same_type() {
            assertThat(Calculator.multiply(1, 2, context)).isEqualTo(2);
        }

        @Test
        void support_calculate_in_different_type() {
            assertThat(Calculator.multiply(1, 2L, context)).isEqualTo(2L);
        }

        @Test
        void all_input_number_should_number_type() {
            assertIllegalArgument(() -> Calculator.multiply("2", "4", context), "Operands should be number but 'java.lang.String' and 'java.lang.String'");
        }
    }

    @Nested
    class Div {
        @Test
        void support_calculate_in_same_number_type() {
            assertThat(Calculator.divide(8, 2, context)).isEqualTo(4);
        }

        @Test
        void support_calculate_in_different_number_type() {
            assertThat(Calculator.divide(8, 2L, context)).isEqualTo(4L);
        }

        @Test
        void all_input_number_should_number_type() {
            assertIllegalArgument(() -> Calculator.divide("2", "4", context), "Operands should be number but 'java.lang.String' and 'java.lang.String'");
        }
    }

    @Nested
    class Sub {

        @Test
        void support_calculate_in_same_number_type() {
            assertThat(Calculator.subtract(4, 1, context)).isEqualTo(3);
        }

        @Test
        void support_calculate_in_different_number_type() {
            assertThat(Calculator.subtract(4, 1L, context)).isEqualTo(3L);
        }

        @Test
        void all_input_number_should_number_type() {
            assertIllegalArgument(() -> Calculator.subtract("2", "4", context), "Operands should be number but 'java.lang.String' and 'java.lang.String'");
        }
    }

    @Nested
    class Negate {

        @Test
        void support_all_number_type() {
            assertThat(Calculator.negate(context.wrap(1), context).instance()).isEqualTo(-1);
            assertThat(Calculator.negate(context.wrap(1L), context).instance()).isEqualTo(-1L);
        }

        @Test
        void should_raise_error_when_negate_non_number_types() {
            assertIllegalArgument(() -> Calculator.negate(context.wrap("2"), context), "Operands should be number but 'java.lang.String'");
        }
    }
}