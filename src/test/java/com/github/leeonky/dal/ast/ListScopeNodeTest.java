package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.node.ConstValueNode;
import com.github.leeonky.dal.ast.node.ListScopeNode;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ListScopeNodeTest {

    public static final Equal EQUAL = new Equal();
    public static final Matcher MATCHER = new Matcher();
    RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new DAL().extend().getRuntimeContextBuilder().build(null);
    ListScopeNode listScopeNode = new ListScopeNode(Collections.emptyList());

    @Test
    void empty_list_equal_to_or_matches_empty_list() {
        List<Object> emptyList = Collections.emptyList();
        assertThat(listScopeNode.verify(new ConstValueNode(emptyList), EQUAL, DALRuntimeContext).getInstance()).isSameAs(emptyList);
        assertThat(listScopeNode.verify(new ConstValueNode(emptyList), MATCHER, DALRuntimeContext).getInstance()).isSameAs(emptyList);
    }
}