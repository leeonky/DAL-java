package com.github.leeonky.dal.e2e;

import com.github.leeonky.dal.DataAssert;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TestBase {
    DataAssert dataAssert = new DataAssert();

    void assertPass(Object input, String expression) {
        assertTrue(dataAssert.assertData(input, expression).isPassed());
    }

    void assertFailed(Object input, String expression) {
        assertFalse(dataAssert.assertData(input, expression).isPassed());
    }
}
