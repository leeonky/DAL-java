package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Notation;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.dal.ast.DALOperator.*;

public class Operators {

    public static DALOperator logical(Notation notation, BiFunction<Supplier<Object>, Supplier<Object>, Object> logical) {
        return new BaseOperator(PRECEDENCE_LOGICAL, notation, biAdapt1(logical), true);
    }

    public static BaseOperator plusSub(Notation notation, TriFunction<Object, Object, DALRuntimeContext, Object> plusSub) {
        return new BaseOperator(PRECEDENCE_PLUS_SUB, notation, adapt(plusSub), false);
    }

    public static BaseOperator mulDiv(Notation notation, TriFunction<Object, Object, DALRuntimeContext, Object> mulDiv) {
        return new BaseOperator(PRECEDENCE_MUL_DIV, notation, adapt(mulDiv), false);
    }

    public static BaseOperator comparator(Notation notation, TriFunction<DALNode, DALNode, DALRuntimeContext, Object> operation) {
        return new BaseOperator(PRECEDENCE_COMPARISON, notation, operation, true);
    }

    public static BaseOperator unaryOpt(Notation notation, TriFunction<DALNode, DALNode, DALRuntimeContext, Object> unary) {
        return new BaseOperator(PRECEDENCE_UNARY_OPERATION, notation, unary, true) {
            @Override
            public String inspect(String node1, String node2) {
                return notation.getLabel() + node2;
            }
        };
    }

    public static DALOperator parentheses() {
        return new DALOperator(PRECEDENCE_PARENTHESES, "", false) {
            @Override
            public Data calculateData(DALNode left, DALNode right, DALRuntimeContext context) {
                return right.evaluateData(context);
            }

            @Override
            public String inspect(String node1, String node2) {
                return String.format("(%s)", node2);
            }
        };
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

    public interface TriFunction<T1, T2, T3, R> {
        R apply(T1 obj1, T2 obj2, T3 obj3);
    }

    public static TriFunction<DALNode, DALNode, DALRuntimeContext, Object> biAdapt1(BiFunction<Supplier<Object>,
            Supplier<Object>, Object> operation) {
        return (left, right, context) -> operation.apply(() -> left.evaluate(context), () -> right.evaluate(context));
    }

    public static TriFunction<DALNode, DALNode, DALRuntimeContext, Object> adapt(
            TriFunction<Object, Object, DALRuntimeContext, Object> operation) {
        return (left, right, context) -> operation.apply(left.evaluate(context), right.evaluate(context), context);
    }

    public static TriFunction<DALNode, DALNode, DALRuntimeContext, Object> unary(
            BiFunction<Data, DALRuntimeContext, Object> operation) {
        return (left, right, context) -> operation.apply(right.evaluateData(context), context);
    }

    public static TriFunction<DALNode, DALNode, DALRuntimeContext, Object> unary(
            Function<Object, Object> operation) {
        return (left, right, context) -> operation.apply(right.evaluate(context));
    }
}
