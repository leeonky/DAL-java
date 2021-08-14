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
    }
}
