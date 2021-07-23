package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Value;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

public class VerifyValueInSchema extends Base {

    @Test
    void verify_without_type_convert() {
        dataAssert.getRuntimeContextBuilder().registerSchema(MatchString.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", "1");
        }}, "is MatchString");

        assertFailed(new HashMap<String, Object>() {{
            put("value", "2");
        }}, "is MatchString");
    }

    @Test
    void should_convert_to_target_type_in_verification() {
        dataAssert.getRuntimeContextBuilder().registerSchema(MatchString.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", 1);
        }}, "is MatchString");

        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchString");
    }

    @Test
    void verify_null_value() {
        dataAssert.getRuntimeContextBuilder().registerSchema(MatchNull.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", null);
        }}, "is MatchNull");

        assertFailed(new HashMap<String, Object>() {{
            put("value", 0);
        }}, "is MatchNull");
    }

    @Test
    void verify_number_comparison() {
        dataAssert.getRuntimeContextBuilder()
                .registerSchema(MatchLessThan2.class)
                .registerSchema(MatchGreaterThan3.class)
                .registerSchema(MatchLessOrEqualTo3.class)
                .registerSchema(MatchGreaterOrEqualTo3.class)
        ;

        assertPass(new HashMap<String, Object>() {{
            put("value", "0");
        }}, "is MatchLessThan2");
        assertPass(new HashMap<String, Object>() {{
            put("value", 0);
        }}, "is MatchLessThan2");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchLessThan2");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 3);
        }}, "is MatchLessThan2");

        assertPass(new HashMap<String, Object>() {{
            put("value", "2");
        }}, "is MatchLessOrEqualTo3");
        assertPass(new HashMap<String, Object>() {{
            put("value", 3);
        }}, "is MatchLessOrEqualTo3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is MatchLessOrEqualTo3");

        assertPass(new HashMap<String, Object>() {{
            put("value", "4");
        }}, "is MatchGreaterThan3");
        assertPass(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is MatchGreaterThan3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 3);
        }}, "is MatchGreaterThan3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchGreaterThan3");

        assertPass(new HashMap<String, Object>() {{
            put("value", "3");
        }}, "is MatchGreaterOrEqualTo3");
        assertPass(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is MatchGreaterOrEqualTo3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchGreaterOrEqualTo3");
    }

    public static class MatchString {
        public Value<String> value = Value.equalTo("1");
    }

    public static class MatchNull {
        public Value<String> value = Value.nullReference();
    }

    public static class MatchLessThan2 {
        public Value<Integer> value = Value.lessThan(2);
    }

    public static class MatchGreaterThan3 {
        public Value<Integer> value = Value.greaterThan(3);
    }

    public static class MatchLessOrEqualTo3 {
        public Value<Integer> value = Value.lessOrEqualTo(3);
    }

    public static class MatchGreaterOrEqualTo3 {
        public Value<Integer> value = Value.greaterOrEqualTo(3);
    }

    //TODO given field `Value` is null
    //TODO return null value
    //TODO customer convert method
    //TODO matches "1"
    //TODO matches Value (convert and check value(if have))
    //TODO matches /abcd/
}
