package com.github.leeonky.dal;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.assertj.core.api.Assertions;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DataAssertsTest {

    DataAssertor dataAssertor = new DataAssertor();

    private void assertFailed(AssertResult assertResult, String s) {
        assertFalse(assertResult.isPassed());
        Assertions.assertThat(assertResult.getMessage()).contains(s);
    }

    private void assertPass(Object actual, String expression) {
        assertTrue(dataAssertor.assertData(actual, expression).isPassed());
    }

    @Test
    void assert_root_value() {
        assertPass(true, "");
        assertFailed(dataAssertor.assertData(false, ""), "Expected root value to be [true] but was <false>");
    }

    @Test
    void assert_property() throws JSONException {
        assertPass(new Bean().setField(true), ".field");

        assertPass("", ".empty");

        dataAssertor.getCompilingContextBuilder().registerType(JSONObject.class, JSONObject::get);
        assertPass(new JSONObject("{\"field\": true}"), ".field");
    }

    @Test
    void assert_one_const_value() {
        assertPass(null, "true");
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public boolean field;

        public int value;
    }
}
