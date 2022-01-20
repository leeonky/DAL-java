package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Operator;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;

class ListNodeTest {

    public static final Operator.Equal EQUAL = new Operator.Equal();
    public static final Operator.Matcher MATCHER = new Operator.Matcher();
    RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new RuntimeContextBuilder().build(null);
    ListNode listNode = new ListNode();

    @Test
    void empty_list_equal_to_or_matches_empty_list() {
        assertThat(listNode.judge(new ConstNode(Collections.emptyList()), EQUAL, DALRuntimeContext)).isTrue();
        assertThat(listNode.judge(new ConstNode(Collections.emptyList()), MATCHER, DALRuntimeContext)).isTrue();
    }
}