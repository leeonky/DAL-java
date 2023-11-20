package com.github.leeonky.dal;

import com.github.leeonky.dal.cucumber.JSONObjectAccessor;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.IllegalOperationException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class CalculatorTest {
    private RuntimeContextBuilder.DALRuntimeContext context = new RuntimeContextBuilder().build(null);

    private void assertIllegalArgument(Executable executable, String message) {
        IllegalOperationException exception = assertThrows(IllegalOperationException.class, executable);
        assertThat(exception).hasMessage(message);
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
            assertIllegalArgument(() -> Calculator.negate(context.wrap("2"), context), "Operand should be number but 'java.lang.String'");
        }
    }
}