package com.github.leeonky.dal;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class DataAssertTest {

    private DataAssert dataAssert = new DataAssert();

    private void assertPass(Object actual, String expression) {
        assertTrue(dataAssert.assertData(actual, expression).isPassed());
    }

    @Test
    void should_assert_root_value_when_source_code_is_empty() {
        assertPass(true, "");

        AssertResult assertResult = dataAssert.assertData(false, "");
        assertFalse(assertResult.isPassed());
        assertThat(assertResult.getMessage()).contains("Expected root value to be [true] but was <false>");
    }

    @Test
    void assert_result_should_be_boolean() {
        IllegalStateException illegalStateException = assertThrows(IllegalStateException.class,
                () -> dataAssert.assertData(1, ""));
        assertThat(illegalStateException).hasMessage("Assert result should be boolean but java.lang.Integer");
    }

    @Test
    void should_assert_const_value_and_ignore_root_when_specified() {
        assertPass(null, "true");
    }

    private void assertRuntimeException(Object input, String sourceCode, int position, String message) {
        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> dataAssert.assertData(input, sourceCode));

        assertThat(runtimeException)
                .hasFieldOrPropertyWithValue("position", position)
                .hasMessage(message);
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public boolean field;

        public int value;
    }

    @Nested
    class AccessProperty {

        @Test
        void should_access_root_value_property_when_no_instance_specified() {
            assertPass("", ".empty");
        }

        @Test
        void should_support_access_property_through_public_field() {
            assertPass(new Bean().setField(true), ".field");
        }

        @Test
        void should_support_access_property_through_getter() {
            assertPass(null, "''.empty");
        }

        @Test
        void should_support_register_customer_getter() throws JSONException {
            dataAssert.getCompilingContextBuilder().registerPropertyAccessor(JSONObject.class, JSONObject::get);
            assertPass(new JSONObject("{\"field\": true}"), ".field");
        }

        @Test
        void should_raise_error_access_invalid_property() {
            assertRuntimeException("", " = .fun", 3, "Get property failed, property can be public field, getter or customer type getter");
        }
    }

    @Nested
    class ExpressionCalculate {

        @Test
        void simple_assert_expression() {
            assertPass(2, "=2");
            assertPass(null, "1=1");
        }

        @Test
        void assert_with_simple_calculate() {
            assertPass(2, "+1=3");
        }

        @Test
        void assert_with_complex_calculate() {
            assertPass(17, "=2+3*4+4/2+1");
            assertPass(null, "2+3*4+4/2+1=2+3*4+4/2+1");
        }

        @Test
        void expression_with_bracket() {
            assertPass(5, "=(2+3)*(4+4)/(2+6)");
        }

        @Test
        void expression_with_minus() {
            assertPass(1, "-2=-1");
        }

        @Test
        void combine_logical_expression() {
            assertPass(null, "!false");

            assertPass(null, "true and true");
            assertPass(null, "true or false");
        }

        @Test
        void plus_with_un_matched_type() {
            assertRuntimeException(null, " + 1", 1, "Can not plus null and java.math.BigDecimal");
        }

        @Test
        void compare_with_un_matched_type() {
            assertRuntimeException(null, " > 1", 1, "Can not compare <null> and <1>");
        }
    }

    @Nested
    class AssertObject {

        @Nested
        class AssertType {

            @Test
            void assert_java_lang_class() {
                dataAssert.getCompilingContextBuilder().registerJavaLangType(String.class);
                assertPass("", "is String");
            }
        }

        @Nested
        class AssertValueInFormat {

            @Test
            void assert_string_value_format() {
                dataAssert.getCompilingContextBuilder().registerStringValueFormat("URL", s -> s.startsWith("http"));

                assertPass("http://www.baidu.com", "is URL");
            }
        }
    }
}
