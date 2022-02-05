package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.runtime.PropertyAccessor;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.Instant;
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
        void verify_expression_return_type_should_be_boolean() {
            IllegalStateException illegalStateException = assertThrows(IllegalStateException.class,
                    () -> dal.assertTrue(1, ""));
            assertThat(illegalStateException).hasMessage("Verification result should be boolean but 'java.lang.Integer'");
        }
    }

    @Nested
    class AccessProperty {

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
    }

    @Nested
    class Regex {

        @Nested
        class WithOptMatcher {
        }

        @Nested
        class WithOptEq {

            @Test
            void support_matching_regex_without_type_convert() {
                assertPass("abc", "= /abc/");
                assertFailed("non match", "= /abc/");
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
            assertRuntimeException(1, ": '1'", 0, "Cannot compare between java.lang.Integer\n<1>\nand java.lang.String\n<1>\n");
        }

        @Test
        void do_not_allow_boolean_auto_convert_to_string() {
            assertRuntimeException(true, ": 'true'", 0, "Cannot compare between java.lang.Boolean\n<true>\nand java.lang.String\n<true>\n");
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
