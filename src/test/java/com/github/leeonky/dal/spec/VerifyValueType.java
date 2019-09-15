package com.github.leeonky.dal.spec;

import org.junit.jupiter.api.Test;

import java.util.HashMap;

class VerifyValueTypeInSchema extends Base {

    @Test
    void verify_string_type() {
        dataAssert.getRuntimeContextBuilder().registerSchema(JavaTypeInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is JavaTypeInSchema");

        assertFailed(new HashMap<String, Object>() {{
            put("stringValue", 1);
        }}, "is JavaTypeInSchema");
    }

    @Test
    void verify_type_with_value() {
        dataAssert.getRuntimeContextBuilder().registerSchema(JavaTypeWithValueInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "world";
        }}, "is JavaTypeWithValueInSchema");

        assertFailed(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is JavaTypeWithValueInSchema");
    }

    public static class JavaTypeInSchema {
        public String stringValue;
    }

    public static class JavaTypeWithValueInSchema {
        public String stringValue = "world";
    }
}
