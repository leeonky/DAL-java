package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Value;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

public class VerifyValueInSchema extends Base {

    @Test
    void verify_without_type_convert() {
        dataAssert.getRuntimeContextBuilder().registerSchema(VerifyValueInSchema.ConvertToString.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", "1");
        }}, "is ConvertToString");

        assertFailed(new HashMap<String, Object>() {{
            put("value", "2");
        }}, "is ConvertToString");
    }

    @Test
    void should_convert_to_target_type_in_verification() {
        dataAssert.getRuntimeContextBuilder().registerSchema(VerifyValueInSchema.ConvertToString.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", 1);
        }}, "is ConvertToString");

        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is ConvertToString");
    }

    //TODO given field `Value` is null
    //TODO given field `Value` should be null
    //TODO return null value
    //TODO default build in Value (eq greater less...)
    //TODO customer convert method
    public static class ConvertToString {
        public Value<String> value = Value.equalTo("1");
    }
}
