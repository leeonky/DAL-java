package com.github.leeonky.dal.e2e;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.DataAssertor;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ValueAssertionTest {
    DataAssertor dataAssertor = new DataAssertor();

    @Test
    void assert_int_equals_passed() {
        AssertResult assertResult = dataAssertor.assertData(1, "= 1");

        assertTrue(assertResult.isPassed());
    }

    @Test
    void assert_int_equals_failed() {
        AssertResult assertResult = dataAssertor.assertData(1, "= 2");

        assertFalse(assertResult.isPassed());
        Assertions.assertThat(assertResult.getMessage()).contains("Expected value to [= 2] but was <1>");
    }
}
