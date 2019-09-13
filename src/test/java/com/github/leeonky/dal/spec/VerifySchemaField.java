package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Formatters;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

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

    public enum E {
        A, B
    }

    public static class InstantNowValue {
        public static final Formatters.Instant instant = Formatters.Instant.now();
    }

    public static class IntegerValue {
        public static final Formatters.Integer integer1 = Formatters.Integer.equalTo(1);
        public Formatters.Integer integer2 = Formatters.Integer.equalTo(2);
    }

    public static class IntegerListValue {
        public List<Formatters.Integer> integerList = asList(Formatters.Integer.equalTo(1), Formatters.Integer.equalTo(2));
    }

    public static class IntegerMapValue {
        public Map<String, Formatters.Integer> integerMap = new HashMap<String, Formatters.Integer>() {{
            put("a", Formatters.Integer.equalTo(1));
            put("b", Formatters.Integer.equalTo(2));
        }};
    }

    public static class PositiveIntegerValue {
        public Formatters.Integer integer = Formatters.Integer.positive();
    }

    public static class NegativeIntegerValue {
        public Formatters.Integer integer = Formatters.Integer.negative();
    }

    public static class NumberValue {
        public Formatters.Number number = Formatters.Number.equalTo(1);
    }

    public static class PositiveNumberValue {
        public Formatters.Number number = Formatters.Number.positive();
    }

    public static class NegativeNumberValue {
        public Formatters.Number number = Formatters.Number.negative();
    }

    @Nested
    class Integer {

        @Test
        void support_verify_integer() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(IntegerValue.class);
            assertPass(new JSONObject("{" +
                    "\"integer1\": 1," +
                    " \"integer2\": 2" +
                    "}"), "is IntegerValue");

            assertFailed(new JSONObject("{" +
                    "\"integer1\": 2," +
                    " \"integer2\": 1" +
                    "}"), "is IntegerValue");
        }

        @Test
        void support_verify_integer_list() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(IntegerListValue.class);
            assertPass(new JSONObject("{" +
                    "\"integerList\": [1, 2]" +
                    "}"), "is IntegerListValue");

            assertFailed(new JSONObject("{" +
                    "\"integerList\": [1]" +
                    "}"), "is IntegerListValue");

            assertFailed(new JSONObject("{" +
                    "\"integerList\": [1, 3]" +
                    "}"), "is IntegerListValue");
        }

        @Test
        void support_verify_integer_map() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(IntegerMapValue.class);
            assertPass(new JSONObject("{" +
                    "\"integerMap\": {\"a\": 1, \"b\": 2}" +
                    "}"), "is IntegerMapValue");

            assertFailed(new JSONObject("{" +
                    "\"integerMap\": {\"a\": 1}" +
                    "}"), "is IntegerMapValue");

            assertFailed(new JSONObject("{" +
                    "\"integerMap\": {\"a\": 1, \"b\": 3}" +
                    "}"), "is IntegerMapValue");
        }

        @Test
        void support_positive_integer() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(PositiveIntegerValue.class);

            assertPass(new JSONObject("{" +
                    "\"integer\": 1" +
                    "}"), "is PositiveIntegerValue");

            assertFailed(new JSONObject("{" +
                    "\"integer\": 0" +
                    "}"), "is PositiveIntegerValue");

            assertFailed(new JSONObject("{" +
                    "\"integer\": -1" +
                    "}"), "is PositiveIntegerValue");
        }

        @Test
        void support_negative_integer() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(NegativeIntegerValue.class);

            assertFailed(new JSONObject("{" +
                    "\"integer\": 1" +
                    "}"), "is NegativeIntegerValue");

            assertFailed(new JSONObject("{" +
                    "\"integer\": 0" +
                    "}"), "is NegativeIntegerValue");

            assertPass(new JSONObject("{" +
                    "\"integer\": -1" +
                    "}"), "is NegativeIntegerValue");
        }
    }

    @Nested
    class Instant {

        @Test
        void support_verify_instant_now_value() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(InstantNowValue.class);
            assertPass(new JSONObject("{\"instant\": \"" + java.time.Instant.now().toString() + "\"}"), "is InstantNowValue");
            assertFailed(new JSONObject("{\"instant\": \"" + java.time.Instant.now().plusSeconds(100).toString() + "\"}"), "is InstantNowValue");
        }
    }

    @Nested
    class Number {

        @Test
        void support_equal_to() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(NumberValue.class);
            assertPass(new JSONObject("{\"number\": 1}"), "is NumberValue");
            assertPass(new JSONObject("{\"number\": 1.0}"), "is NumberValue");
            assertFailed(new JSONObject("{\"number\": 1.1}"), "is NumberValue");
            assertFailed(new JSONObject("{\"number\": \"1\"}"), "is NumberValue");
        }

        @Test
        void support_positive_number() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(PositiveNumberValue.class);
            assertPass(new JSONObject("{\"number\": 1}"), "is PositiveNumberValue");
            assertPass(new JSONObject("{\"number\": 1.0}"), "is PositiveNumberValue");
            assertFailed(new JSONObject("{\"number\": 0.0}"), "is PositiveNumberValue");
            assertFailed(new JSONObject("{\"number\": 0}"), "is PositiveNumberValue");
            assertFailed(new JSONObject("{\"number\": -1}"), "is PositiveNumberValue");
        }

        @Test
        void support_negative_number() throws JSONException {
            dataAssert.getRuntimeContextBuilder().registerSchema(NegativeNumberValue.class);
            assertPass(new JSONObject("{\"number\": -1}"), "is NegativeNumberValue");
            assertPass(new JSONObject("{\"number\": -1.0}"), "is NegativeNumberValue");
            assertFailed(new JSONObject("{\"number\": 0.0}"), "is NegativeNumberValue");
            assertFailed(new JSONObject("{\"number\": 0}"), "is NegativeNumberValue");
            assertFailed(new JSONObject("{\"number\": 1}"), "is NegativeNumberValue");
        }
    }
}
