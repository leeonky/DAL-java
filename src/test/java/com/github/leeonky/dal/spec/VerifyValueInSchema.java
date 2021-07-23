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

    public static class MatchString {
        public Value<String> value = Value.equalTo("1");
    }

    public static class MatchNull {
        public Value<String> value = Value.equalTo(null);
    }

    //TODO given field `Value` is null
    //TODO return null value
    //TODO default build in Value (eq greater less...)
    //TODO customer convert method
    //TODO matches "1"
    //TODO matches Value (convert and check value(if have))
    //TODO matches /abcd/
}
