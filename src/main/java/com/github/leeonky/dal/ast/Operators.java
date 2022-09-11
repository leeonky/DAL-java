package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.compiler.Notations.Keywords;
import com.github.leeonky.dal.compiler.Notations.Operators;
import com.github.leeonky.dal.runtime.Calculator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Notation;

import java.util.function.BiFunction;
import java.util.function.Supplier;

import static com.github.leeonky.dal.ast.DALOperator.PRECEDENCE_LOGICAL;
import static com.github.leeonky.dal.ast.DALOperator.PRECEDENCE_PLUS_SUB;
import static com.github.leeonky.dal.compiler.Notations.COMMA;

public class OperatorFactory {
    public static DALOperator operatorAnd() {
        return and(Operators.AND);
    }

    public static DALOperator keywordAnd() {
        return and(Keywords.AND);
    }

    public static DALOperator commaAnd() {
        return and(COMMA);
    }

    private static DALOperator and(Notation notation) {
        return new BaseOperator(PRECEDENCE_LOGICAL, notation, logical(Calculator::and), true);
    }

    public static DALOperator operatorOr() {
        return or(Operators.OR);
    }

    public static DALOperator keywordOr() {
        return or(Keywords.OR);
    }

    private static DALOperator or(Notation or) {
        return new BaseOperator(PRECEDENCE_LOGICAL, or, logical(Calculator::or), true);
    }

    public static DALOperator plus() {
        return new BaseOperator(PRECEDENCE_PLUS_SUB, Operators.PLUS, arithmetical(Calculator::plus), false);
    }

    private static class BaseOperator extends DALOperator {
        private final TriFunction<DALNode, DALNode, DALRuntimeContext, Object> operation;

        public BaseOperator(int precedence, Notation notation,
                            TriFunction<DALNode, DALNode, DALRuntimeContext, Object> operation, boolean needInspect) {
            super(precedence, notation.getLabel(), needInspect);
            this.operation = operation;
        }

        @Override
        public Object calculate(DALNode left, DALNode right, DALRuntimeContext context) {
            return operation.apply(left, right, context);
        }
    }

    interface TriFunction<T1, T2, T3, R> {
        R apply(T1 obj1, T2 obj2, T3 obj3);
    }

    private static TriFunction<DALNode, DALNode, DALRuntimeContext, Object> logical(BiFunction<Supplier<Object>,
            Supplier<Object>, Object> operation) {
        return (left, right, context) -> operation.apply(() -> left.evaluate(context), () -> right.evaluate(context));
    }

    private static TriFunction<DALNode, DALNode, DALRuntimeContext, Object> arithmetical(
            TriFunction<Object, Object, DALRuntimeContext, Object> operation) {
        return (left, right, context) -> operation.apply(left.evaluate(context), right.evaluate(context), context);
    }
}
