package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContextBuilder;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ExpressionTest {

    @Test
    void test_operator() {
        assertPassed("a", "a", Operator.EQUAL);
        assertFailed("a", "b", Operator.EQUAL);

        assertPassed("a", "b", Operator.NOT_EQUAL);
        assertFailed("b", "b", Operator.NOT_EQUAL);

        assertPassed("b", "a", Operator.GREATER);
        assertFailed("a", "b", Operator.GREATER);

        assertPassed("a", "b", Operator.LESS);
        assertFailed("b", "a", Operator.LESS);

        assertPassed("b", "a", Operator.GREATER_OR_EQUAL);
        assertPassed("b", "b", Operator.GREATER_OR_EQUAL);
        assertFailed("a", "b", Operator.GREATER_OR_EQUAL);

        assertPassed("a", "b", Operator.LESS_OR_EQUAL);
        assertPassed("b", "b", Operator.LESS_OR_EQUAL);
        assertFailed("b", "a", Operator.LESS_OR_EQUAL);
    }

    private void assertPassed(String s1, String s2, Operator operator) {
        Object evaluate = new Expression(new ConstNode(s1), new ConstNode(s2), operator).evaluate(new CompilingContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(true);
    }

    private void assertFailed(String s1, String s2, Operator operator) {
        Object evaluate = new Expression(new ConstNode(s1), new ConstNode(s2), operator).evaluate(new CompilingContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(false);
    }
}