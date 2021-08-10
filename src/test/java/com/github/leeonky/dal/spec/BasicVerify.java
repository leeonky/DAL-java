package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.util.PropertyAccessor;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
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

            AssertResult assertResult = dataAssert.assertData(false, "");
            assertFalse(assertResult.isPassed());
            assertThat(assertResult.getMessage()).contains("Expected root value to be [true] but was <false>");
        }


        @Test
        void verify_expression_return_type_should_be_boolean() {
            IllegalStateException illegalStateException = assertThrows(IllegalStateException.class,
                    () -> dataAssert.assertData(1, ""));
            assertThat(illegalStateException).hasMessage("Verification result should be boolean but 'java.lang.Integer'");
        }

        @Test
        void verify_const_value() {
            assertPass(null, "true");
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
            assertPass(null, "!false");

            assertPass(null, "true and true");
            assertPass(null, "true or false");
        }

        @Test
        void expression_operand_type_should_be_matched() {
            assertRuntimeException(null, " + 1", 1, "Can not plus 'null' and 'java.math.BigDecimal'");
            assertRuntimeException(null, " > 1", 1, "Can not compare [null] and [1]");
        }
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
            dataAssert.getRuntimeContextBuilder().registerPropertyAccessor(JSONObject.class, new PropertyAccessor<JSONObject>() {
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
            assertPass(new JSONObject("{\"field\": true}"), ".field");
        }

        @Test
        void should_support_access_property_via_bracket() {
            assertPass(new HashMap<String, String>() {{
                put(" a key ", "value");
            }}, "[' a key '] = 'value'");
        }

        @Test
        void should_raise_error_when_access_invalid_property() {
            assertRuntimeException("", " = .fun", 3, "Get property via `.fun` failed, property can be public field, getter or customer type getter");
        }
    }

    @Nested
    class Matches {

        @Nested
        class MatchesValue {

            @Test
            void support_operator_matches() {
                assertPass(1, ": 1");
                assertFailed(1, ": 2");
            }

            @Test
            void support_convert_to_target_type_before_matches_value() {
                assertPass("1", ": 1");
                assertFailed("1", ": 2");
            }

            @Test
            void process_null() {
                assertFailed(1, ": null");
                assertFailed(null, ": 1");
                assertPass(null, ": null");
            }
        }

        @Nested
        class MatchesRegex {

            @Test
            void support_matches_regex() {
                assertPass(1, ": /1/");
                assertFailed(2, ": /1/");
            }

            //TODO Support = /1/
        }
    }
}
