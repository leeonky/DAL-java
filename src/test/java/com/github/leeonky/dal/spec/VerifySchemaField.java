package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Formatters;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Instant;

class VerifySchemaField extends Base {
    @BeforeEach
    void registerJsonType() {
        dataAssert.getRuntimeContextBuilder().registerPropertyAccessor(JSONObject.class, new JsonPropertyAccessor());
    }

    @Test
    void support_verify_instant_value() throws JSONException {
        dataAssert.getRuntimeContextBuilder().registerSchema(InstantValue.class);
        assertPass(new JSONObject("{\"instant\": \"1999-10-10T11:12:13Z\"}"), "is InstantValue");
        assertFailed(new JSONObject("{\"instant\": \"1999-10-10T11:12:14Z\"}"), "is InstantValue");
    }

    @Test
    void support_verify_positive_integer() throws JSONException {
        dataAssert.getRuntimeContextBuilder().registerSchema(PositiveNumberValue.class);
        assertPass(new JSONObject("{" +
                "\"positiveInteger1\": 1," +
                " \"positiveInteger2\": 2" +
                "}"), "is PositiveNumberValue");
    }

    public static class InstantValue {
        public static final Formatters.Instant instant = Formatters.Instant.equalTo(Instant.parse("1999-10-10T11:12:13Z"));
    }

    public static class PositiveNumberValue {
        public static final Formatters.PositiveInteger positiveInteger1 = Formatters.PositiveInteger.equalTo(1);
        public Formatters.PositiveInteger positiveInteger2 = Formatters.PositiveInteger.equalTo(2);
    }
}
