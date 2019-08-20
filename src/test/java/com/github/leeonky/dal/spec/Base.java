package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.DataAssert;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class Base {
    protected DataAssert dataAssert = new DataAssert();

    protected void assertPass(Object input, String expression) {
        assertTrue(dataAssert.assertData(input, expression).isPassed());
    }

    protected void assertFailed(Object input, String expression) {
        assertFalse(dataAssert.assertData(input, expression).isPassed());
    }

    protected void assertRuntimeException(Object input, String sourceCode, int position, String message) {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dataAssert.assertData(input, sourceCode));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }
}
