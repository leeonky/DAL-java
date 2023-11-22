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