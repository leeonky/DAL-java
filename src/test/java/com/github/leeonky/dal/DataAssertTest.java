package com.github.leeonky.dal;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class DataAssertTest {

    private DataAssert dataAssert = new DataAssert();

    private void assertFailed(AssertResult assertResult, String s) {
        assertFalse(assertResult.isPassed());
        assertThat(assertResult.getMessage()).contains(s);
    }

    private void assertPass(Object actual, String expression) {
        assertTrue(dataAssert.assertData(actual, expression).isPassed());
    }

    @Test
    void assert_root_value() {
        assertPass(true, "");
        assertFailed(dataAssert.assertData(false, ""), "Expected root value to be [true] but was <false>");
    }

    @Test
    void assert_property() throws JSONException {
        assertPass(new Bean().setField(true), ".field");

        assertPass("", ".empty");

        dataAssert.getCompilingContextBuilder().registerType(JSONObject.class, JSONObject::get);
        assertPass(new JSONObject("{\"field\": true}"), ".field");
    }

    @Test
    void assert_one_const_value() {
        assertPass(null, "true");
    }

    @Test
    void assert_simple_expression() {
        assertPass(2, "=2");
        assertPass(2, "1=1");
    }

    @Test
    void assert_simple_calculate() {
        assertPass(2, "+1=3");
    }

    @Test
    void assert_complex_calculate() {
        assertPass(17, "=2+3*4+4/2+1");
        assertPass(null, "2+3*4+4/2+1=2+3*4+4/2+1");
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public boolean field;

        public int value;
    }

    @Nested
    class RuntimeError {

        @Test
        void plus_with_un_matched_type() {
            RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dataAssert.assertData(null, " + 1"));

            assertThat(runtimeException)
                    .hasFieldOrPropertyWithValue("position", 1)
                    .hasMessage("Can not plus null and java.math.BigDecimal");
        }

        @Test
        void compare_with_un_matched_type() {
            RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dataAssert.assertData(null, " > 1"));

            assertThat(runtimeException)
                    .hasFieldOrPropertyWithValue("position", 1)
                    .hasMessage("Can not compare <null> and <1>");
        }

        @Test
        void access_invalid_property() {
            RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dataAssert.assertData("", " = .fun"));

            assertThat(runtimeException)
                    .hasFieldOrPropertyWithValue("position", 3)
                    .hasMessage("Get property failed, property can be public field, getter or customer type getter");
        }
    }
}
