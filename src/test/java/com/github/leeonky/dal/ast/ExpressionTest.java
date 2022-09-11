package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class ExpressionTest {

    @Test
    void test_operator() {
        assertPassed("a", "a", new DALOperator.Equal());

        assertPassed("a", "b", new DALOperator.NotEqual());

//        assertPassed("b", "a", Operators.greater());

//        assertPassed("a", "b", Operators.less());

//        assertPassed("b", "a", Operators.greaterOrEqual());
//        assertPassed("b", "b", Operators.greaterOrEqual());

//        assertPassed("a", "b", Operators.lessOrEqual());
//        assertPassed("b", "b", Operators.lessOrEqual());
    }

    @Test
    void assert_simple_calculate() {
//        assertCalculate(1, Operators.plus(), new BigDecimal(2), new BigDecimal(3));

//        assertCalculate("1", Operators.plus(), "2", "12");

//        assertCalculate(2, Operators.subtract(), 1, 1);

//        assertCalculate(2, Operators.multiply(), 3, 6);

//        assertCalculate(6, Operators.divide(), 3, 2);

        assertCalculate(1, new DALOperator.Minus(), new BigDecimal(2), new BigDecimal(-2));
    }

    @Test
    void assert_logic_combination() {
//        assertCalculate(true, Operators.logical(AND, Calculator::and), true, true);

//        assertCalculate(true, Operators.operatorOr(), false, true);

        assertCalculate(null, new DALOperator.Not(), true, false);
    }

    @Test
    void should_support_short_circuit_expression() {
//        assertTrue((boolean) new DALExpression(new ConstNode(true), Operators.operatorOr(), new ConstNode(null)).evaluate(new RuntimeContextBuilder().build(null)));
//        assertFalse((boolean) new DALExpression(new ConstNode(false), Operators.logical(AND, Calculator::and), new ConstNode(null)).evaluate(new RuntimeContextBuilder().build(null)));
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