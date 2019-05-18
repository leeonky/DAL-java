package com.github.leeonky.dal;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DataAssertorTest {

    DataAssertor dataAssertor = new DataAssertor();

    @Nested
    class BasicOperator {
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

        @Test
        void assert_int_not_equals_passed() {
            AssertResult assertResult = dataAssertor.assertData(1, "!= 2");

            assertTrue(assertResult.isPassed());
        }
    }
}
