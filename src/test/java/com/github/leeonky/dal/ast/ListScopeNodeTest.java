package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.ast.node.ConstValueNode;
import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.ListScopeNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.ast.opt.Factory;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ListScopeNodeTest {

    public static final DALOperator equal = Factory.equal();
    public static final DALOperator match = Factory.match();
    RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = new DAL().extend().getRuntimeContextBuilder().build(null);
    ListScopeNode listScopeNode = new ListScopeNode(Collections.emptyList());

    @Test
    void empty_list_equal_to_or_matches_empty_list() {
        List<Object> emptyList = Collections.emptyList();
        assertThat(DALExpression.expression(new ConstValueNode(emptyList), equal, listScopeNode).evaluate(DALRuntimeContext)).isSameAs(emptyList);
        assertThat(DALExpression.expression(new ConstValueNode(emptyList), match, listScopeNode).evaluate(DALRuntimeContext)).isSameAs(emptyList);
    }
}