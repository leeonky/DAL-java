package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContextBuilder;
import com.github.leeonky.dal.RuntimeException;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

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

    @Test
    void assert_simple_calculate() {
        assertSimplePlus(1, new BigDecimal(2), new BigDecimal(3));
        assertSimplePlus(new BigDecimal(2), 1, new BigDecimal(3));
        assertSimplePlus("1", "2", "12");
        assertSimplePlus("1", 2, "12");
        assertSimplePlus(1, "2", "12");

        RuntimeException runtimeException = assertThrows(RuntimeException.class, () -> {
            new Expression(new ConstNode(null), new ConstNode(null), Operator.PLUS).evaluate(new CompilingContextBuilder().build(null));
        });
        assertThat(runtimeException).hasMessage("calculate type not matched");
    }

    private void assertSimplePlus(Object v1, Object v2, Object expected) {
        Object evaluate = new Expression(new ConstNode(v1), new ConstNode(v2), Operator.PLUS).evaluate(new CompilingContextBuilder().build(null));

        assertThat(evaluate).isEqualTo(expected);
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