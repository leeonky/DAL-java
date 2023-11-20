package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.IllegalOperationException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CalculatorTest {
    private RuntimeContextBuilder.DALRuntimeContext context = new RuntimeContextBuilder().build(null);

    private void assertIllegalArgument(Executable executable, String message) {
        IllegalOperationException exception = assertThrows(IllegalOperationException.class, executable);
        assertThat(exception).hasMessage(message);
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