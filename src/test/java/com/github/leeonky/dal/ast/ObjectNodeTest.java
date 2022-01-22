package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.DALOperator.Equal;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ObjectNodeTest {

    public static final Equal EQUAL = new Equal();
    public static final DALOperator.Matcher MATCHER = new DALOperator.Matcher();

    @Nested
    class EqualTo {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder().build(null);
        ObjectNode objectNode = new ObjectNode();

        @Test
        void empty_data_equal_to_empty_object() {
            assertThat(objectNode.judge(new ConstNode(new HashMap<>()), EQUAL, DALRuntimeContext)).isTrue();
        }

        @Test
        void not_equal_when_has_unexpected_field() {
            assertThrows(AssertionFailure.class, () ->
                    objectNode.judge(new ConstNode(new HashMap<String, Object>() {{
                        put("unexpected", "field");
                    }}), EQUAL, DALRuntimeContext));
        }
    }

    @Nested
    class Matches {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder().build(null);
        ObjectNode objectNode = new ObjectNode();

        @Test
        void any_data_matches_empty_object() {
            assertThat(objectNode.judge(new ConstNode(new HashMap<String, Object>() {{
                put("any fields", "any value");
            }}), MATCHER, DALRuntimeContext)).isTrue();
        }

        @Test
        void null_does_not_match_empty_object() {
            assertThrows(AssertionFailure.class, () ->
                    objectNode.judge(new ConstNode(null), MATCHER, DALRuntimeContext)
            );
        }
    }
}