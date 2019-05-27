package com.github.leeonky.dal;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DataAssertorTest {

    DataAssertor dataAssertor = new DataAssertor();

    private void assertFailed(AssertResult assertResult, String s) {
        assertFalse(assertResult.isPassed());
        Assertions.assertThat(assertResult.getMessage()).contains(s);
    }

    @Test
    void assert_root_value() {
        assertTrue(dataAssertor.assertData(true, "").isPassed());
        assertFailed(dataAssertor.assertData(false, ""), "Expected root value to be [true] but was <false>");
    }
}
