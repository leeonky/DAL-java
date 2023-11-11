package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Value;
import com.github.leeonky.dal.runtime.DalException;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.Schema;
import com.github.leeonky.util.BeanClass;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class VerifyValueInSchema extends Base {

    @Test
    void verify_without_type_convert() {
        dal.getRuntimeContextBuilder().registerSchema(MatchString1.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", "1");
        }}, "is MatchString1");

        assertFailed(new HashMap<String, Object>() {{
            put("value", "2");
        }}, "is MatchString1");
    }

    @Test
    void should_convert_to_target_type_in_verification() {
        dal.getRuntimeContextBuilder().registerSchema(MatchString1.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", 1);
        }}, "is MatchString1");

        assertFailed(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchString1");
    }

    @Test
    void verify_null_value() {
        dal.getRuntimeContextBuilder().registerSchema(MatchNullValue.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", null);
        }}, "is MatchNullValue");

        assertFailed(new HashMap<String, Object>() {{
            put("value", 0);
        }}, "is MatchNullValue");
    }

    @Test
    void verify_number_comparison() {
        dal.getRuntimeContextBuilder()
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

    @Test
    void verify_type() {
        dal.getRuntimeContextBuilder()
                .registerSchema(MatchType.class)
                .registerSchema(MatchTypeWithNullableValue.class);

        assertPass(new HashMap<String, Object>() {{
            put("value", "0");
        }}, "is MatchType");
        DalException dalException = assertFailed(new HashMap<String, Object>() {{
            put("value", "invalid int");
        }}, "is MatchType");
        assertThat(dalException).hasMessage("Expected to match schema `MatchType` but was not\n" +
                "    Can not convert field `.value` (java.lang.String: invalid int) to type [com.github.leeonky.dal.format.Value]");

        assertPass(new HashMap<String, Object>() {{
            put("value", null);
        }}, "is MatchTypeWithNullableValue");
        dalException = assertFailed(new HashMap<String, Object>() {{
            put("value", null);
        }}, "is MatchType");
        assertThat(dalException).hasMessage("Expected to match schema `MatchType` but was not\n" +
                "    Can not convert null field `.value` to type [com.github.leeonky.dal.format.Value], use @AllowNull to verify nullable field");
    }

    @Test
    void missing_type_arg() {
        dal.getRuntimeContextBuilder()
                .registerSchema(MissingTypeArg.class)
                .registerSchema(MissingTypeArgButGivenValue.class);

        assertThrows(RuntimeException.class, () -> dal.evaluate(new HashMap<String, Object>() {{
            put("value", 1);
        }}, "is MissingTypeArg"));

        assertThrows(RuntimeException.class, () -> dal.evaluate(new HashMap<String, Object>() {{
            put("value", 1);
        }}, "is MissingTypeArgButGivenValue"));
    }

    @Test
    void customized_message() {
        dal.getRuntimeContextBuilder()
                .registerSchema(MatchString1.class)
                .registerSchema(MatchNullValue.class)
                .registerSchema(MatchLessThan2.class)
                .registerSchema(MatchGreaterThan3.class)
                .registerSchema(MatchLessOrEqualTo3.class)
                .registerSchema(MatchGreaterOrEqualTo3.class)
        ;

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchString1", "Expected to match schema `MatchString1` but was not\n" +
                "    Expecting field `.value` [2] to be equal to [1], but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchNullValue", "Expected to match schema `MatchNullValue` but was not\n" +
                "    Expecting field `.value` [2] to be null, but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchLessThan2", "Expected to match schema `MatchLessThan2` but was not\n" +
                "    Expecting field `.value` [2] to be less than [2], but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchGreaterThan3", "Expected to match schema `MatchGreaterThan3` but was not\n" +
                "    Expecting field `.value` [2] to be greater than [3], but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 4);
        }}, "is MatchLessOrEqualTo3", "Expected to match schema `MatchLessOrEqualTo3` but was not\n" +
                "    Expecting field `.value` [4] to be less or equal to [3], but was not.");

        assertErrorContains(new HashMap<String, Object>() {{
            put("value", 2);
        }}, "is MatchGreaterOrEqualTo3", "Expected to match schema `MatchGreaterOrEqualTo3` but was not\n" +
                "    Expecting field `.value` [2] to be greater or equal to [3], but was not.");
    }

    public static class MatchString1 implements Schema {
        public Value<String> value = Value.equalTo("1");
    }

    public static class MatchNullValue implements Schema {
        public Value<String> value = Value.nullReference();
    }

    public static class MatchLessThan2 implements Schema {
        public Value<Integer> value = Value.lessThan(2);
    }

    public static class MatchGreaterThan3 implements Schema {
        public Value<Integer> value = Value.greaterThan(3);
    }

    public static class MatchLessOrEqualTo3 implements Schema {
        public Value<Integer> value = Value.lessOrEqualTo(3);
    }

    public static class MatchGreaterOrEqualTo3 implements Schema {
        public Value<Integer> value = Value.greaterOrEqualTo(3);
    }

    public static class MatchType implements Schema {
        public Value<Integer> value;
    }

    public static class MatchTypeWithNullableValue implements Schema {
        @AllowNull
        public Value<Integer> value;
    }

    public static class MatchInvalidValue implements Schema {
        public Value<Integer> value;
    }

    public static class MissingTypeArg implements Schema {
        public Value<?> value;
    }

    public static class MissingTypeArgButGivenValue implements Schema {
        public Value<?> value = Value.equalTo(1);
    }

    public static class CustomizedConverter implements Schema {
        public Value<?> value = new ToIntegerAndIncrease();
    }

    public static class ToIntegerAndIncrease implements Value<Integer> {

        @Override
        public Integer convertAs(Data actual, BeanClass<?> type) {
            return (int) actual.instance() + 1;
        }

        @Override
        public boolean verify(Integer actual) {
            return actual.equals(2);
        }
    }

    @Nested
    class Converter {

        @Test
        void customized_converter() {
            dal.getRuntimeContextBuilder()
                    .registerSchema(CustomizedConverter.class);

            assertPass(new HashMap<String, Object>() {{
                put("value", 1);
            }}, "is CustomizedConverter");

            assertFailed(new HashMap<String, Object>() {{
                put("value", 2);
            }}, "is CustomizedConverter");
        }
    }
}
