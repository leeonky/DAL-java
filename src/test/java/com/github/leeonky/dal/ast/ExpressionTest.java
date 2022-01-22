package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ExpressionTest {

    @Test
    void test_operator() {
        assertPassed("a", "a", new DALOperator.Equal());

        assertPassed("a", "b", new DALOperator.NotEqual());

        assertPassed("b", "a", new DALOperator.Greater());

        assertPassed("a", "b", new DALOperator.Less());

        assertPassed("b", "a", new DALOperator.GreaterOrEqual());
        assertPassed("b", "b", new DALOperator.GreaterOrEqual());

        assertPassed("a", "b", new DALOperator.LessOrEqual());
        assertPassed("b", "b", new DALOperator.LessOrEqual());
    }

    @Test
    void assert_simple_calculate() {
        assertCalculate(1, new DALOperator.Plus(), new BigDecimal(2), new BigDecimal(3));

        assertCalculate("1", new DALOperator.Plus(), "2", "12");

        assertCalculate(2, new DALOperator.Subtraction(), 1, 1);

        assertCalculate(2, new DALOperator.Multiplication(), 3, 6);

        assertCalculate(6, new DALOperator.Division(), 3, 2);

        assertCalculate(1, new DALOperator.Minus(), new BigDecimal(2), new BigDecimal(-2));
    }

    @Test
    void assert_logic_combination() {
        assertCalculate(true, new DALOperator.And("&&"), true, true);

        assertCalculate(true, new DALOperator.Or("||"), false, true);

        assertCalculate(null, new DALOperator.Not(), true, false);
    }

    @Test
    void should_support_short_circuit_expression() {
        assertTrue((boolean) new DALExpression(new ConstNode(true), new DALOperator.Or("||"), new ConstNode(null)).evaluate(new RuntimeContextBuilder().build(null)));
        assertFalse((boolean) new DALExpression(new ConstNode(false), new DALOperator.And("&&"), new ConstNode(null)).evaluate(new RuntimeContextBuilder().build(null)));
    }

    private void assertCalculate(Object v1, DALOperator operator, Object v2, Object expected) {
        Object evaluate = new DALExpression(new ConstNode(v1), operator, new ConstNode(v2)).evaluate(new RuntimeContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(expected);
    }

    private void assertPassed(Object s1, Object s2, DALOperator operator) {
        Object evaluate = new DALExpression(new ConstNode(s1), operator, new ConstNode(s2)).evaluate(new RuntimeContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(true);
    }
}