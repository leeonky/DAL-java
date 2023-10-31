package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.node.ConstValueNode;
import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ExpressionTest {

    @Test
    void test_operator() {
        assertPassed("a", "a", new Equal());
    }

    private void assertPassed(Object s1, Object s2, DALOperator operator) {
        Object evaluate = new DALExpression(new ConstValueNode(s1), operator, new ConstValueNode(s2)).evaluate(new RuntimeContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(s1);
    }
}