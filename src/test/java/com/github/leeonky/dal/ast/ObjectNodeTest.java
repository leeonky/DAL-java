package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeContextBuilder;
import com.github.leeonky.dal.ast.Operator.Equal;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectNodeTest {

    public static final Equal OPERATOR = new Equal();
    public static final Operator.Matcher MATCHER = new Operator.Matcher();

    @Nested
    class EqualTo {
        RuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);
        ObjectNode objectNode = new ObjectNode();

        @Test
        void empty_data_equal_to_empty_object() {
            assertThat(objectNode.judge(OPERATOR, new HashMap<>(), runtimeContext)).isTrue();
        }

        @Test
        void not_equal_when_has_unexpected_field() {
            assertThat(objectNode.judge(OPERATOR, new HashMap<String, Object>() {{
                put("unexpected", "field");
            }}, runtimeContext)).isFalse();
        }
    }

    @Nested
    class Matches {
        RuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);
        ObjectNode objectNode = new ObjectNode();

        @Test
        void any_data_matches_empty_object() {
            assertThat(objectNode.judge(MATCHER, new HashMap<String, Object>() {{
                put("any fields", "any value");
            }}, runtimeContext)).isTrue();
        }

        @Test
        void null_does_not_match_empty_object() {
            assertThat(objectNode.judge(MATCHER, null, runtimeContext)).isFalse();
        }
    }
}