package com.github.leeonky.dal;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class DataAssertorTest {

    DataAssertor dataAssertor = new DataAssertor();

    private void assertFailed(AssertResult assertResult, String s) {
        assertFalse(assertResult.isPassed());
        Assertions.assertThat(assertResult.getMessage()).contains(s);
    }

    @Nested
    class BasicOperator {
        @Test
        void assert_int_equals_passed() {
            assertTrue(dataAssertor.assertData(1, "= 1").isPassed());
        }

        @Test
        void assert_int_equals_failed() {
            assertFailed(dataAssertor.assertData(1, "= 2"), "Expected value to [= 2] but was <1>");
        }

        @Test
        void assert_int_not_equals_passed() {
            assertTrue(dataAssertor.assertData(1, "!= 2").isPassed());
        }
    }

    @Nested
    class KeywordOperator {
        @Test
        void assert_int_equals_passed() {
            assertTrue(dataAssertor.assertData(1, "is 1").isPassed());
        }

        @Test
        void assert_int_equals_failed() {
            assertFailed(dataAssertor.assertData(1, "is 2"), "Expected value to [is 2] but was <1>");
        }

        @Test
        void should_has_a_white_space_between_right_value() {
            assertThrows(RuntimeException.class, () -> dataAssertor.assertData(1, "is1"));

            assertTrue(dataAssertor.assertData(1, "is\t1").isPassed());
        }
    }
}
