package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.runtime.AssertResult;
import com.github.leeonky.dal.runtime.PropertyAccessor;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.HashMap;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class BasicVerify extends Base {

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public boolean field;

        public int value;
    }

    @Setter
    @Accessors(chain = true)
    public static class ObjectA {
        public String f1, f2;
    }

    @Setter
    @Accessors(chain = true)
    public static class ObjectB {
        public String f1;
    }

    @Nested
    class Basic {
        @Test
        void should_use_root_value_as_assertion_expression_when_source_code_is_empty() {
            assertPass(true, "");
            AssertResult assertResult = dal.assertTrue(false, "");
            assertThat(assertResult.isPassed()).isFalse();
            assertThat(assertResult.getMessage()).contains("Expected root value to be [true] but was <false>");
        }


        @Test
        void verify_expression_return_type_should_be_boolean() {
            IllegalStateException illegalStateException = assertThrows(IllegalStateException.class,
                    () -> dal.assertTrue(1, ""));
            assertThat(illegalStateException).hasMessage("Verification result should be boolean but 'java.lang.Integer'");
        }

        @Test
        void verify_const_value() {
            assertTrue(null, "true");
        }

        @Test
        void verify_calculation() {
            assertPass(null, "1=1");
            assertPass(1, "=1");

            assertPass(17, "=2+3*4+4/2+1");
            assertPass(null, "2+3*4+4/2+1=2+3*4+4/2+1");

            assertPass(5, "=(2+3)*(4+4)/(2+6)");
        }

        @Test
        void first_operand_should_be_root_value_when_no_specified() {
            assertPass(2, "-1=1");
        }

        @Test
        void verify_logical_combinations() {
            assertTrue(null, "!false");

            assertTrue(null, "true and true");
            assertTrue(null, "true or false");
        }

        @Test
        void alias_of_operator_and() {
            assertTrue(null, "true , true");
            assertFalse(null, "true , false");
            assertFalse(null, "false , true");
            assertFalse(null, "false , false");
        }

        @Test
        void expression_operand_type_should_be_matched() {
            assertRuntimeException(null, " + 1", 1, "Can not plus 'null' and 'java.lang.Integer'");
            assertRuntimeException(null, " > 1", 1, "Can not compare [null] and [1]");
        }
    }

    @Nested
    class AccessProperty {

        @Test
        void should_access_root_value_property_when_no_instance_specified() {
            assertTrue("", ".empty");
        }

        @Test
        void should_support_ignore_begin_dot() {
            assertTrue("", "empty");
        }

        @Test
        void should_support_access_property_through_public_field() {
            assertTrue(new Bean().setField(true), ".field");
        }

        @Test
        void should_support_access_property_through_getter() {
            assertTrue(null, "''.empty");
        }

        @Test
        void should_support_register_customer_getter() throws JSONException {
            dal.getRuntimeContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
                @Override
                public Object getValue(JSONObject instance, String name) {
                    try {
                        return instance.get(name);
                    } catch (JSONException e) {
                        throw new IllegalStateException(e);
                    }
                }

                @Override
                public Set<String> getPropertyNames(JSONObject instance) {
                    return null;
                }

                @Override
                public boolean isNull(JSONObject instance) {
                    return instance == null || instance.equals(JSONObject.NULL);
                }
            });
            assertTrue(new JSONObject("{\"field\": true}"), ".field");
        }

        @Test
        void should_support_access_property_via_bracket() {
            assertPass(new HashMap<String, String>() {{
                put(" a key ", "value");
            }}, "[' a key '] = 'value'");
        }

        @Test
        void should_raise_error_when_access_invalid_property() {
            assertRuntimeException("", " = .fun", 3, "Get property via `.fun` failed, property can be public field, getter or customer type getter:\n\t"
                    + "Method or property `fun` does not exist in `java.lang.String`");
        }
    }

    @Nested
    class Regex {

        @Nested
        class WithOptMatcher {

            @Test
            void support_matching_regex_without_type_convert() {
                assertPass("abc", ": /abc/");
                assertFailed("non match", ": /abc/");
            }

            @Test
            void support_convert_to_string_before_matching() {
                assertPass(1, ": /1/");
                assertFailed(10, ": /1/");
            }
        }

        @Nested
        class WithOptEq {

            @Test
            void support_matching_regex_without_type_convert() {
                assertPass("abc", "= /abc/");
                assertFailed("non match", "= /abc/");
            }

            @Test
            void raise_error_when_type_is_not_string() {
                assertRuntimeException(1, "= /1/", 0, "Operator = before regex need a string input value");
            }
        }
    }

    @Nested
    class Matches {

        @Test
        void number_value() {
            assertPass(1, ": 1.0");
            assertPass(1, ": 1");
            assertPass(1.0, ": 1");
            assertPass(1L, ": 1");

            assertFailed(1, ": 2");
        }

        @Test
        void boolean_value() {
            assertPass(true, ": true");
            assertPass(false, ": false");
            assertFailed(true, ": false");
            assertFailed(false, ": true");
        }

        @Test
        void string_value() {
            assertPass("abc", ": 'abc'");
            assertFailed("abc", ": 'xyz'");
        }

        @Test
        void object_to_string() {
            assertPass(Instant.parse("2000-10-10T00:00:01Z"), ": '2000-10-10T00:00:01Z'");
            assertFailed(Instant.parse("2010-10-10T00:00:01Z"), ": '2000-10-10T00:00:01Z'");
        }

        @Test
        void do_not_allow_auto_convert_to_number() {
            assertRuntimeException("1", ": 1", 0, "Cannot compare between java.lang.String\n<1>\nand java.lang.Integer\n<1>\n");
        }

        @Test
        void do_not_allow_auto_convert_to_boolean() {
            assertRuntimeException("true", ": true", 0, "Cannot compare between java.lang.String\n<true>\nand java.lang.Boolean\n<true>\n");
        }

        @Test
        void do_not_allow_number_auto_convert_to_string() {
            assertRuntimeException(1, ": '1'", 0, "Cannot compare between java.lang.Integer\n<1>\nand 'java.lang.String'");
        }

        @Test
        void do_not_allow_boolean_auto_convert_to_string() {
            assertRuntimeException(true, ": 'true'", 0, "Cannot compare between java.lang.Boolean\n<true>\nand 'java.lang.String'");
        }

        @Test
        void null_value_matching() {
            assertPass(null, ": null");
            assertFailed(1, ": null");
            assertFailed(0, ": null");
            assertFailed(true, ": null");
            assertFailed(false, ": null");
            assertFailed("", ": null");
            assertFailed(null, ": 1");
            assertFailed(null, ": 0");
            assertFailed(null, ": true");
            assertFailed(null, ": false");
            assertFailed(null, ": ''");
            assertFailed(null, ": 'any string'");
        }
    }
}
