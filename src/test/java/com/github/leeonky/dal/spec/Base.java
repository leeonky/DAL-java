package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.AssertionFailure;
import com.github.leeonky.dal.runtime.DalException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class Base {
    protected DAL dal = new DAL();

    protected void assertTrue(Object input, String expression) {
        assertThat(dal.assertTrue(input, expression).isPassed()).isTrue();
    }

    protected void assertFalse(Object input, String expression) {
        assertThat(dal.assertTrue(input, expression).isPassed()).isFalse();
    }

    protected void assertPass(Object input, String expression) {
        dal.evaluate(input, expression);
//        assertThat((Object) dal.evaluate(input, expression)).isEqualTo(true);
    }

    protected void assertFailed(Object input, String expression) {
        AssertionFailure assertionFailure = null;
        try {
            dal.evaluate(input, expression);
        } catch (AssertionFailure failure) {
            assertionFailure = failure;
        }
        assertThat(assertionFailure).isNotNull();
    }

    protected void assertRuntimeException(Object input, String sourceCode, int position, String message) {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dal.evaluate(input, sourceCode));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    protected void assertErrorContains(Object input, String expression, String errorMessage) {
        assertThat(assertThrows(DalException.class, () -> {
            dal.evaluate(input, expression);
        })).hasMessage(errorMessage);
    }
}
