package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Formatters;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Arrays.asList;

class VerifySchemaField extends Base {
    @BeforeEach
    void registerJsonType() {
        dataAssert.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JsonPropertyAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayListAccessor());
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

        assertFailed(new JSONObject("{" +
                "\"positiveInteger1\": 2," +
                " \"positiveInteger2\": 1" +
                "}"), "is PositiveNumberValue");
    }

    @Test
    void support_verify_positive_integer_list() throws JSONException {
        dataAssert.getRuntimeContextBuilder().registerSchema(PositiveNumberListValue.class);
        assertPass(new JSONObject("{" +
                "\"positiveIntegerList\": [1, 2]" +
                "}"), "is PositiveNumberListValue");

        assertFailed(new JSONObject("{" +
                "\"positiveIntegerList\": [1]" +
                "}"), "is PositiveNumberListValue");

        assertFailed(new JSONObject("{" +
                "\"positiveIntegerList\": [1, 3]" +
                "}"), "is PositiveNumberListValue");
    }

    @Test
    void support_verify_positive_integer_map() throws JSONException {
        dataAssert.getRuntimeContextBuilder().registerSchema(PositiveNumberMapValue.class);
        assertPass(new JSONObject("{" +
                "\"positiveIntegerMap\": {\"a\": 1, \"b\": 2}" +
                "}"), "is PositiveNumberMapValue");

        assertFailed(new JSONObject("{" +
                "\"positiveIntegerMap\": {\"a\": 1}" +
                "}"), "is PositiveNumberMapValue");

        assertFailed(new JSONObject("{" +
                "\"positiveIntegerMap\": {\"a\": 1, \"b\": 3}" +
                "}"), "is PositiveNumberMapValue");
    }

    public static class InstantValue {
        public static final Formatters.Instant instant = Formatters.Instant.equalTo(Instant.parse("1999-10-10T11:12:13Z"));
    }

    public static class PositiveNumberValue {
        public static final Formatters.PositiveInteger positiveInteger1 = Formatters.PositiveInteger.equalTo(1);
        public Formatters.PositiveInteger positiveInteger2 = Formatters.PositiveInteger.equalTo(2);
    }

    public static class PositiveNumberListValue {
        public List<Formatters.PositiveInteger> positiveIntegerList = asList(Formatters.PositiveInteger.equalTo(1), Formatters.PositiveInteger.equalTo(2));
    }

    public static class PositiveNumberMapValue {
        public Map<String, Formatters.PositiveInteger> positiveIntegerMap = new HashMap<String, Formatters.PositiveInteger>() {{
            put("a", Formatters.PositiveInteger.equalTo(1));
            put("b", Formatters.PositiveInteger.equalTo(2));
        }};
    }
}
