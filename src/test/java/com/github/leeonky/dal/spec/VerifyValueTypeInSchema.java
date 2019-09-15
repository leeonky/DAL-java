package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Type;
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

    @Test
    void verify_wrapped_type() {
        dataAssert.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is WrappedJavaTypeInSchema");

        assertFailed(new HashMap<String, Object>() {{
            put("stringValue", 1);
        }}, "is WrappedJavaTypeInSchema");
    }

    @Test
    void verify_wrapped_type_with_value() {
        dataAssert.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeWithValueInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is WrappedJavaTypeWithValueInSchema");

        assertFailed(new JavaTypeInSchema() {{
            stringValue = "world";
        }}, "is WrappedJavaTypeWithValueInSchema");
    }

    @Test
    void verify_wrapped_type_with_null() {
        dataAssert.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeNullReferenceInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = null;
        }}, "is WrappedJavaTypeNullReferenceInSchema");

        assertFailed(new JavaTypeInSchema() {{
            stringValue = "world";
        }}, "is WrappedJavaTypeNullReferenceInSchema");
    }

    public static class JavaTypeInSchema {
        public String stringValue;
    }

    public static class JavaTypeWithValueInSchema {
        public String stringValue = "world";
    }

    public static class WrappedJavaTypeInSchema {
        public Type<String> stringValue;
    }

    public static class WrappedJavaTypeWithValueInSchema {
        public Type<String> stringValue = Type.equalTo("hello");
    }

    public static class WrappedJavaTypeNullReferenceInSchema {
        public Type<String> stringValue = Type.nullReference();
    }
}
