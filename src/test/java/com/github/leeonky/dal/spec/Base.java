package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.DataAssert;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class Base {
    protected DataAssert dataAssert = new DataAssert();

    protected void assertPass(Object input, String expression) {
        assertTrue(dataAssert.assertData(input, expression).isPassed());
    }

    protected void assertFailed(Object input, String expression) {
        AssertResult assertResult = null;
        AssertionFailure assertionFailure = null;
        try {
            assertResult = dataAssert.assertData(input, expression);
        } catch (AssertionFailure failure) {
            assertionFailure = failure;
        }
        if (assertResult != null)
            assertFalse(assertResult.isPassed());
        else
            assertThat(assertionFailure).isNotNull();
    }

    protected void assertRuntimeException(Object input, String sourceCode, int position, String message) {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dataAssert.assertData(input, sourceCode));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    protected void assertErrorContains(Runnable codeBlock, String errorMessage) {
        PrintStream systemErr = System.err;
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        PrintStream err = new PrintStream(os);
        System.setErr(err);
        try {
            codeBlock.run();
            assertThat(os.toString()).contains(errorMessage);
        } finally {
            System.setErr(systemErr);
        }
    }
}
