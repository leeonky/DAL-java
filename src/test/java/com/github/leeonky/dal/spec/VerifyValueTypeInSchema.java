package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Type;
import com.github.leeonky.dal.type.Schema;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

class VerifyValueTypeInSchema extends Base {

    @Test
    void verify_string_type() {
        dal.getRuntimeContextBuilder().registerSchema(JavaTypeInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is JavaTypeInSchema");

        assertFailed(new HashMap<String, Object>() {{
            put("stringValue", 1);
        }}, "is JavaTypeInSchema");
    }

    @Test
    void verify_type_with_value() {
        dal.getRuntimeContextBuilder().registerSchema(JavaTypeWithValueInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "world";
        }}, "is JavaTypeWithValueInSchema");

        assertFailed(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is JavaTypeWithValueInSchema");
    }

    @Test
    void verify_wrapped_type() {
        dal.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is WrappedJavaTypeInSchema");

        assertFailed(new HashMap<String, Object>() {{
            put("stringValue", 1);
        }}, "is WrappedJavaTypeInSchema");
    }

    @Test
    void verify_wrapped_type_with_value() {
        dal.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeWithValueInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = "hello";
        }}, "is WrappedJavaTypeWithValueInSchema");

        assertFailed(new JavaTypeInSchema() {{
            stringValue = "world";
        }}, "is WrappedJavaTypeWithValueInSchema");
    }

    @Test
    void verify_wrapped_type_with_null() {
        dal.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeNullReferenceInSchema.class);

        assertPass(new JavaTypeInSchema() {{
            stringValue = null;
        }}, "is WrappedJavaTypeNullReferenceInSchema");

        assertFailed(new JavaTypeInSchema() {{
            stringValue = "world";
        }}, "is WrappedJavaTypeNullReferenceInSchema");
    }

    @Test
    void customized_error_log() {
        dal.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeWithValueInSchema.class);
        assertErrorContains(new JavaTypeInSchema() {{
                                stringValue = "world";
                            }}, "is WrappedJavaTypeWithValueInSchema",
                "Expected to match schema `WrappedJavaTypeWithValueInSchema` but was not\n" +
                        "    Expecting field `.stringValue` [world] to be equal to [hello], but was not.");

        dal.getRuntimeContextBuilder().registerSchema(WrappedJavaTypeNullReferenceInSchema.class);
        assertErrorContains(new JavaTypeInSchema() {{
                                stringValue = "world";
                            }}, "is WrappedJavaTypeNullReferenceInSchema",
                "Expected to match schema `WrappedJavaTypeNullReferenceInSchema` but was not\n" +
                        "    Expecting field `.stringValue` [world] to be null, but was not.");
    }

    @Test
    void verify_number_comparison() {
        dal.getRuntimeContextBuilder()
                .registerSchema(VerifyValueTypeInSchema.LessThan2.class)
                .registerSchema(VerifyValueTypeInSchema.GreaterThan3.class)
                .registerSchema(VerifyValueTypeInSchema.LessOrEqualTo3.class)
                .registerSchema(VerifyValueTypeInSchema.GreaterOrEqualTo3.class)
        ;

        assertPass(new HashMap<String, Object>() {{
            put("value", 0);
        }}, "is LessThan2");
        assertPass(new HashMap<String, Object>() {{
            put("value", 0);
        }}, "is LessThan2");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is LessThan2");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 3);
        }}, "is LessThan2");

        assertPass(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is LessOrEqualTo3");
        assertPass(new HashMap<String, Object>() {{
            put("value", 3);
        }}, "is LessOrEqualTo3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is LessOrEqualTo3");

        assertPass(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is GreaterThan3");
        assertPass(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is GreaterThan3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 3);
        }}, "is GreaterThan3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is GreaterThan3");

        assertPass(new HashMap<String, Object>() {{
            put("value", 3);
        }}, "is GreaterOrEqualTo3");
        assertPass(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is GreaterOrEqualTo3");
        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is GreaterOrEqualTo3");
    }

    @Test
    void customized_message() {
        dal.getRuntimeContextBuilder()
                .registerSchema(VerifyValueTypeInSchema.LessThan2.class)
                .registerSchema(VerifyValueTypeInSchema.GreaterThan3.class)
                .registerSchema(VerifyValueTypeInSchema.LessOrEqualTo3.class)
                .registerSchema(VerifyValueTypeInSchema.GreaterOrEqualTo3.class)
        ;

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is LessThan2", "Expected to match schema `LessThan2` but was not\n" +
                "    Expecting field `.value` [2] to be less than [2], but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is GreaterThan3", "Expected to match schema `GreaterThan3` but was not\n" +
                "    Expecting field `.value` [2] to be greater than [3], but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is LessOrEqualTo3", "Expected to match schema `LessOrEqualTo3` but was not\n" +
                "    Expecting field `.value` [4] to be less or equal to [3], but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is GreaterOrEqualTo3", "Expected to match schema `GreaterOrEqualTo3` but was not\n" +
                "    Expecting field `.value` [2] to be greater or equal to [3], but was not.");
    }

    public static class JavaTypeInSchema implements Schema {
        public String stringValue;
    }

    public static class JavaTypeWithValueInSchema implements Schema {
        public String stringValue = "world";
    }

    public static class WrappedJavaTypeInSchema implements Schema {
        public Type<String> stringValue;
    }

    public static class WrappedJavaTypeWithValueInSchema implements Schema {
        public Type<String> stringValue = Type.equalTo("hello");
    }

    public static class WrappedJavaTypeNullReferenceInSchema implements Schema {
        public Type<String> stringValue = Type.nullReference();
    }

    public static class LessThan2 implements Schema {
        public Type<Integer> value = Type.lessThan(2);
    }

    public static class GreaterThan3 implements Schema {
        public Type<Integer> value = Type.greaterThan(3);
    }

    public static class LessOrEqualTo3 implements Schema {
        public Type<Integer> value = Type.lessOrEqualTo(3);
    }

    public static class GreaterOrEqualTo3 implements Schema {
        public Type<Integer> value = Type.greaterOrEqualTo(3);
    }
}
