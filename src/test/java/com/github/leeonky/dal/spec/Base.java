package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.DalException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public class Base {
    protected DAL dal = new DAL();

    protected void assertPass(Object input, String expression) {
        assertTrue(dal.assertData(input, expression).isPassed());
    }

    protected void assertFailed(Object input, String expression) {
        AssertResult assertResult = null;
        AssertionFailure assertionFailure = null;
        try {
            assertResult = dal.assertData(input, expression);
        } catch (AssertionFailure failure) {
            assertionFailure = failure;
        }
        if (assertResult != null)
            assertFalse(assertResult.isPassed());
        else
            assertThat(assertionFailure).isNotNull();
    }

    protected void assertRuntimeException(Object input, String sourceCode, int position, String message) {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dal.assertData(input, sourceCode));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    protected void assertErrorContains(Object input, String expression, String errorMessage) {
        assertThat(assertThrows(DalException.class, () -> {
            dal.assertData(input, expression);
        })).hasMessage(errorMessage);
    }
}
