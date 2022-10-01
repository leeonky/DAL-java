package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.node.ConstNode;
import com.github.leeonky.dal.ast.node.ListEllipsisNode;
import com.github.leeonky.dal.ast.node.ObjectScopeNode;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.AssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ObjectScopeNodeTest {

    public static final Equal EQUAL = new Equal();
    public static final Matcher MATCHER = new Matcher();

    @Nested
    class EqualTo {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new DAL().extend().getRuntimeContextBuilder().build(null);
        ObjectScopeNode objectScopeNode = new ObjectScopeNode(Collections.emptyList());

        @Test
        void empty_data_equal_to_empty_object() {
            assertThat(objectScopeNode.verify(new ConstNode(new HashMap<>()), EQUAL, DALRuntimeContext)).isTrue();
        }

        @Test
        void not_equal_when_has_unexpected_field() {
            assertThrows(AssertionFailure.class, () ->
                    objectScopeNode.verify(new ConstNode(new HashMap<String, Object>() {{
                        put("unexpected", "field");
                    }}), EQUAL, DALRuntimeContext));
        }
    }

    @Nested
    class Matches {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder().build(null);
        ObjectScopeNode objectScopeNode = new ObjectScopeNode(new ListEllipsisNode());

        @Test
        void any_data_matches_empty_object() {
            assertThat(objectScopeNode.verify(new ConstNode(new HashMap<String, Object>() {{
                put("any fields", "any value");
            }}), MATCHER, DALRuntimeContext)).isTrue();
        }

        @Test
        void null_does_not_match_empty_object() {
            assertThrows(AssertionFailure.class, () ->
                    objectScopeNode.verify(new ConstNode(null), MATCHER, DALRuntimeContext)
            );
        }
    }
}