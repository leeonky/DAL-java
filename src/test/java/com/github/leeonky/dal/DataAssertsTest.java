package com.github.leeonky.dal;

import lombok.Getter;
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

    @Test
    void assert_root_value() {
        assertTrue(dataAssertor.assertData(true, "").isPassed());
        assertFailed(dataAssertor.assertData(false, ""), "Expected root value to be [true] but was <false>");
    }

    @Test
    void assert_property_of_root_value() throws JSONException {
        assertTrue(dataAssertor.assertData(new Bean().setField(true), ".field").isPassed());
        assertTrue(dataAssertor.assertData(new Bean().setMethod(true), ".method").isPassed());

        dataAssertor.getCompilingContextBuilder().registerType(JSONObject.class, JSONObject::get);
        assertTrue(dataAssertor.assertData(new JSONObject("{\"field\": true}"), ".field").isPassed());
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public boolean field;
        @Getter
        public boolean method;
    }
}
