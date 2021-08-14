package com.github.leeonky.dal.spec;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

class VerifyObject extends Base {

    @Nested
    class EqualTo {

        @Test
        void equal_to_empty_object() {
            assertPass(new HashMap<>(), "= {}");
        }

        @Test
        void raise_error_when_has_unexpected_field() {
            assertFailed(new HashMap<String, Object>() {{
                put("key", 1);
            }}, "= {}");
        }

        @Test
        void object_key_sets_should_be_equal_and_each_property_should_pass_then_return_pass() {
            assertPass(new HashMap<String, Object>() {{
                put("key", 1);
            }}, "= {key: 1}");
        }

        @Test
        void return_false_when_object_key_sets_not_equal() {
            assertFailed(new HashMap<>(), "= {key: 1}");
        }

        @Test
        void return_false_when_object_field_value_not_equal() {
            assertFailed(new HashMap<String, Object>() {{
                put("key", 1);
            }}, "= {key: 2}");
        }

        //TODO property chain
        //TODO process getClass property for java bean and size property of list
        //TODO property is alias
    }

    @Nested
    class Matches {

        @Test
        void any_non_null_object_matches_empty_object() {
            assertPass(new HashMap<>(), ": {}");
            assertPass(new HashMap<String, Object>() {{
                put("any field", "any value");
            }}, ": {}");
            assertPass(1, ": {}");
        }

        @Test
        void null_does_not_match_empty_object() {
            assertFailed(null, ": {}");
        }

        //TODO property
        //TODO property chain
        //TODO process getClass property for java bean and size property of list
        //TODO property is alias
    }
}
