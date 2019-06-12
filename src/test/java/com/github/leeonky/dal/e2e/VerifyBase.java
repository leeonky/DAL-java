package com.github.leeonky.dal.e2e;

import com.github.leeonky.dal.DataAssert;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class VerifyBase {
    DataAssert dataAssert = new DataAssert();

    void assertPass(Object input, String expression) {
        assertTrue(dataAssert.assertData(input, expression).isPassed());
    }

    void assertFailed(Object input, String expression) {
        assertFalse(dataAssert.assertData(input, expression).isPassed());
    }

    void assertRuntimeException(Object input, String sourceCode, int position, String message) {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dataAssert.assertData(input, sourceCode));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }
}
