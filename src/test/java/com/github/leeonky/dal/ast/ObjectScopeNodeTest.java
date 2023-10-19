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
            HashMap<Object, Object> emptyMap = new HashMap<>();
            assertThat(objectScopeNode.verify(new ConstNode(emptyMap), EQUAL, DALRuntimeContext).getInstance()).isSameAs(emptyMap);
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
            HashMap<String, Object> data = new HashMap<String, Object>() {{
                put("any fields", "any value");
            }};
            assertThat(objectScopeNode.verify(new ConstNode(data), MATCHER, DALRuntimeContext).getInstance()).isSameAs(data);
        }

        @Test
        void null_does_not_match_empty_object() {
            assertThrows(AssertionFailure.class, () ->
                    objectScopeNode.verify(new ConstNode(null), MATCHER, DALRuntimeContext)
            );
        }
    }
}