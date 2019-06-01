package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContextBuilder;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class ExpressionTest {

    @Test
    void test_operator() {
        assertPassed("a", "a", new Operator.Equal());
        assertFailed("a", "b", new Operator.Equal());

        assertPassed("a", "b", new Operator.NotEqual());
        assertFailed("b", "b", new Operator.NotEqual());

        assertPassed("b", "a", new Operator.Greater());
        assertFailed("a", "b", new Operator.Greater());

        assertPassed("a", "b", new Operator.Less());
        assertFailed("b", "a", new Operator.Less());

        assertPassed("b", "a", new Operator.GreaterOrEqual());
        assertPassed("b", "b", new Operator.GreaterOrEqual());
        assertFailed("a", "b", new Operator.GreaterOrEqual());

        assertPassed("a", "b", new Operator.LessOrEqual());
        assertPassed("b", "b", new Operator.LessOrEqual());
        assertFailed("b", "a", new Operator.LessOrEqual());

    }

    @Test
    void assert_simple_calculate() {
        assertCalculate(1, new Operator.Plus(), new BigDecimal(2), new BigDecimal(3));
        assertCalculate(new BigDecimal(2), new Operator.Plus(), 1, new BigDecimal(3));
        assertCalculate("1", new Operator.Plus(), "2", "12");
        assertCalculate("1", new Operator.Plus(), 2, "12");
        assertCalculate(1, new Operator.Plus(), "2", "12");

        assertCalculate(2, new Operator.Subtraction(), 1, new BigDecimal(1));

        assertCalculate(2, new Operator.Multiplication(), 3, new BigDecimal(6));

        assertCalculate(6, new Operator.Division(), 3, new BigDecimal(2));
    }

    @Test
    void assert_logic_combination() {
        assertCalculate(true, new Operator.And(), true, true);

        assertCalculate(true, new Operator.Or(), false, true);

        assertCalculate(true, new Operator.Not(), null, false);
    }

    private void assertCalculate(Object v1, Operator operator, Object v2, Object expected) {
        Object evaluate = new Expression(new ConstNode(v1), operator, new ConstNode(v2)).evaluate(new CompilingContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(expected);
    }

    private void assertPassed(Object s1, Object s2, Operator operator) {
        Object evaluate = new Expression(new ConstNode(s1), operator, new ConstNode(s2)).evaluate(new CompilingContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(true);
    }

    private void assertFailed(Object s1, Object s2, Operator operator) {
        Object evaluate = new Expression(new ConstNode(s1), operator, new ConstNode(s2)).evaluate(new CompilingContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(false);
    }
}